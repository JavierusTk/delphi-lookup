# test-unicode.ps1
# Regression test for T-67W7: text must reach SQLite as UTF-16 (AsWideString),
# not cp1252 best-fit (tau -> t, >= -> =, arrow and emoji -> ?), and
# delphi-lookup must print it intact when its output is redirected.
#
# Verifies that:
#   1. No ParamByName(...).AsString := is left in the root .pas/.dpr (lint)
#   2. Indexing Tests\fixtures\unicode keeps tau, arrow, >= and the check-mark
#      emoji in symbols.content and symbols.comments, with no '?' standing in
#   3. delphi-lookup prints them intact through a pipe (UTF-8)
#   4. A database written by an older indexer (no metadata.index_format) is
#      reindexed by the next incremental pass, without --force
#
# Usage: Tests\test-unicode.ps1 [-BinDir <dir with delphi-indexer.exe and delphi-lookup.exe>]
#   Default BinDir is the repo root (fresh build). Pass -BinDir W:\bin to see
#   the deployed binaries fail.
# Requires: W:\bin\sqlite3.exe
# Exit code 0 on success, non-zero on failure.

param(
    [string]$BinDir = (Split-Path -Parent $PSScriptRoot)
)

$repo    = Split-Path -Parent $PSScriptRoot
$indexer = Join-Path $BinDir 'delphi-indexer.exe'
$lookup  = Join-Path $BinDir 'delphi-lookup.exe'
$sqlite  = 'W:\bin\sqlite3.exe'
$fixture = Join-Path $PSScriptRoot 'fixtures\unicode'

foreach ($f in $indexer, $lookup, $sqlite) {
    if (-not (Test-Path $f)) { Write-Error "Not found: $f"; exit 2 }
}

$tau   = [string][char]0x03C4
$arrow = [string][char]0x2192
$ge    = [string][char]0x2265
$checkMark = [char]::ConvertFromUtf32(0x2705)

$work = Join-Path ([IO.Path]::GetTempPath()) ("dl-unicode-" + [guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $work | Out-Null
$db = Join-Path $work 'unicode.db'

$failed = 0
$tests  = 0

function Assert-Test($name, [scriptblock]$check) {
    $script:tests++
    try {
        $ok = & $check
        if ($ok) { Write-Host "[$script:tests] OK   $name" -ForegroundColor Green }
        else     { Write-Host "[$script:tests] FAIL $name" -ForegroundColor Red; $script:failed++ }
    } catch {
        Write-Host "[$script:tests] FAIL $name ($($_.Exception.Message))" -ForegroundColor Red
        $script:failed++
    }
}

function Sql([string]$query) {
    (& $sqlite $db $query | Out-String).Trim()
}

$prevEncoding = [Console]::OutputEncoding
[Console]::OutputEncoding = [Text.UTF8Encoding]::new()
try {
    # 1. Lint
    $offenders = Get-ChildItem -Path "$repo\*" -File -Include *.pas, *.dpr |
        Select-String -Pattern "ParamByName\('[^']+'\)\.AsString\s*:="
    Assert-Test 'lint: no ParamByName(...).AsString := in root .pas/.dpr' {
        if ($offenders) { $offenders | ForEach-Object { Write-Host "    $_" } }
        -not $offenders
    }

    # 2. Indexing keeps the characters
    $indexOut = & $indexer $fixture --no-config --database $db --category user 2>&1 | Out-String
    Assert-Test 'indexer exits 0 on the fixture' { $LASTEXITCODE -eq 0 }

    foreach ($cp in 964, 8594, 8805, 9989) {
        Assert-Test "content keeps U+$('{0:X4}' -f $cp)" {
            [int](Sql "SELECT count(*) FROM symbols WHERE instr(content, char($cp)) > 0") -gt 0
        }
    }
    Assert-Test 'comments keep U+2192 (MT-BRIDGE arrow)' {
        [int](Sql "SELECT count(*) FROM symbols WHERE instr(comments, char(8594)) > 0") -gt 0
    }
    Assert-Test "no '?' stands in for a lost character" {
        [int](Sql "SELECT count(*) FROM symbols WHERE instr(content, '?') > 0 OR instr(comments, '?') > 0") -eq 0
    }

    # 3. Redirected lookup output is UTF-8
    $lookupOut = & $lookup ProbeTexto --no-config -d $db -n 1 2>&1 | Out-String
    Assert-Test 'lookup prints the constant intact through a pipe' {
        $lookupOut.Contains("$tau $arrow $ge $checkMark")
    }

    # 4. Legacy database: drop the markers and degrade the text. The stored
    #    hashes are still the real ones, so without the migration the pass
    #    would see no changes.
    Sql "UPDATE symbols SET content = replace(content, char(964), 't'); DELETE FROM metadata WHERE key IN ('text_encoding', 'index_format');" | Out-Null
    $incOut = & $indexer $fixture --no-config --database $db --category user 2>&1 | Out-String
    Assert-Test 'incremental pass runs the index-format migration from v0' {
        $incOut -match 'Index format migration \(v0 -> v\d+\)'
    }
    Assert-Test 'incremental pass reprocesses the unchanged file' {
        $incOut -match 'Changed files: 1'
    }
    Assert-Test 'reprocessed content has U+03C4 again' {
        [int](Sql "SELECT count(*) FROM symbols WHERE instr(content, char(964)) > 0") -gt 0
    }
    Assert-Test 'metadata.text_encoding is utf16' {
        (Sql "SELECT value FROM metadata WHERE key = 'text_encoding'") -eq 'utf16'
    }
    $againOut = & $indexer $fixture --no-config --database $db --category user 2>&1 | Out-String
    Assert-Test 'next incremental pass finds nothing to do' {
        $againOut -match 'No changes detected'
    }
}
finally {
    [Console]::OutputEncoding = $prevEncoding
    Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
}

Write-Host ""
if ($failed -eq 0) {
    Write-Host "All $tests tests passed." -ForegroundColor Green
    exit 0
} else {
    Write-Host "$failed of $tests tests FAILED." -ForegroundColor Red
    exit 1
}
