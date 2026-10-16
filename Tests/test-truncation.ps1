# test-truncation.ps1
# Regression test for T-5AF2: TruncateAtImplementation (FTS-only indexing)
# must cut every unit after its top-level 'implementation' line.
#
# Verifies, by indexing Tests\fixtures\truncation into a temporary database:
#   1. A one-line {$ifdef X}..{$endif} does not leave the conditional depth
#      open (OneLineCond.pas): interface symbols indexed, implementation-only
#      ones not
#   2. 'implementation' inside a { } comment does not end the interface, and
#      directive-like text in a string or after // is not counted
#      (CommentedImpl.pas)
#   3. A database already marked by T-67W7 (format v1) is reindexed by the
#      next incremental pass, because the truncation change bumped the format
#
# Usage: Tests\test-truncation.ps1 [-BinDir <dir with delphi-indexer.exe>]
#   Default BinDir is the repo root (fresh build). Pass -BinDir W:\bin to see
#   the deployed binary fail.
# Requires: W:\bin\sqlite3.exe
# Exit code 0 on success, non-zero on failure.

param(
    [string]$BinDir = (Split-Path -Parent $PSScriptRoot)
)

$indexer = Join-Path $BinDir 'delphi-indexer.exe'
$sqlite  = 'W:\bin\sqlite3.exe'
$fixture = Join-Path $PSScriptRoot 'fixtures\truncation'

foreach ($f in $indexer, $sqlite) {
    if (-not (Test-Path $f)) { Write-Error "Not found: $f"; exit 2 }
}

$work = Join-Path ([IO.Path]::GetTempPath()) ("dl-truncation-" + [guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $work | Out-Null
$db = Join-Path $work 'truncation.db'

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

function Has-Symbol([string]$name) {
    [int]((& $sqlite $db "SELECT count(*) FROM symbols WHERE name = '$name'" | Out-String).Trim()) -gt 0
}

try {
    & $indexer $fixture --no-config --database $db --category user 2>&1 | Out-Null
    Assert-Test 'indexer exits 0 on the fixtures' { $LASTEXITCODE -eq 0 }

    Assert-Test 'one-line conditional: interface procedure indexed' { Has-Symbol 'OneLineVisible' }
    Assert-Test 'one-line conditional: implementation-only helper NOT indexed' { -not (Has-Symbol 'OneLineHiddenHelper') }

    Assert-Test 'commented implementation: procedure after the comment indexed' { Has-Symbol 'CommentedVisible' }
    Assert-Test 'commented implementation: constant with directive-like string indexed' { Has-Symbol 'CommentedFakeDirective' }
    Assert-Test 'commented implementation: implementation-only helper NOT indexed' { -not (Has-Symbol 'CommentedHiddenHelper') }

    # A database indexed with the T-67W7 binaries (text_encoding='utf16', no
    # index_format) is format v1: the next incremental pass must reindex it.
    & $sqlite $db "DELETE FROM metadata WHERE key = 'index_format';" | Out-Null
    $incOut = & $indexer $fixture --no-config --database $db --category user 2>&1 | Out-String
    Assert-Test 'v1 database (T-67W7 marker only) is migrated on the next pass' {
        $incOut -match 'Index format migration \(v1 -> v\d+\)'
    }
    Assert-Test 'v1 migration reprocesses the unchanged fixtures' {
        $incOut -match 'Changed files: 2'
    }
}
finally {
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
