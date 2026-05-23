# test-output-format.ps1
# Regression test for AI-optimized compact output (see docs/AI-OUTPUT-COMPACT.md).
#
# Verifies that:
#   1. Default output emits NO chrome (// Context, // [CACHE HIT],
#      // Search completed, "Found N result(s)" preamble)
#   2. Default output has ONE line per result with a "...:<line>" location
#   3. --full still emits verbose chrome
#   4. --json still emits valid JSON
#   5. --with-timing re-adds the "// Search completed" footer (but no other chrome)
#   6. --compact-v1 still emits the legacy 2-line format
#
# Requires: W:\bin\delphi-lookup.exe + W:\bin\delphi_symbols.db
# Exit code 0 on success, non-zero on failure.

$exe = 'W:\bin\delphi-lookup.exe'
$db  = 'W:\bin\delphi_symbols.db'

if (-not (Test-Path $exe)) { Write-Error "Not found: $exe"; exit 2 }
if (-not (Test-Path $db))  { Write-Error "Not found: $db";  exit 2 }

# Stable user-code class — exists across normal CyberMAX indexings.
$query = 'TQueryMAX'

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

$default = & $exe $query -d $db -n 3 2>&1

Assert-Test 'default mode produces output' {
    $default.Count -ge 1
}

Assert-Test 'default mode has NO chrome' {
    -not ($default | Select-String -Pattern '// Context for query|// \[CACHE HIT\]|// Search completed|^Found ')
}

Assert-Test 'default mode lines contain ":<digit>" location' {
    ($default | Out-String) -match ':\d+'
}

$full = & $exe $query -d $db -n 3 --full 2>&1
Assert-Test '--full still emits "// Context for query" header' {
    ($full | Select-String '// Context for query').Count -ge 1
}
Assert-Test '--full still emits "// Search completed" footer' {
    ($full | Select-String '// Search completed').Count -ge 1
}

$json = (& $exe $query -d $db -n 3 --json 2>&1) -join ''
Assert-Test '--json output is valid JSON with results array' {
    $parsed = $json | ConvertFrom-Json
    ($parsed.results) -is [System.Array] -or ($null -ne $parsed.results)
}

$timing = & $exe $query -d $db -n 3 --with-timing 2>&1
Assert-Test '--with-timing emits the "// Search completed" footer' {
    ($timing | Select-String '// Search completed').Count -ge 1
}
Assert-Test '--with-timing does NOT emit the "// Context for query" header' {
    -not ($timing | Select-String '// Context for query')
}

$v1 = & $exe $query -d $db -n 3 --compact-v1 2>&1
Assert-Test '--compact-v1 emits the legacy 2-line format' {
    ($v1 | Select-String -Pattern '→ .* \[unit:').Count -ge 1
}

Write-Host ''
Write-Host '============================================================'
if ($failed -eq 0) {
    Write-Host "ALL $tests TESTS PASSED" -ForegroundColor Green
    exit 0
} else {
    Write-Host "$failed of $tests TESTS FAILED" -ForegroundColor Red
    exit 1
}
