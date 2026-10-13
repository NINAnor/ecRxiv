$ErrorActionPreference = 'Stop'
$test = Join-Path $PSScriptRoot 'tests\run_synthetic_end_to_end.R'
if (-not (Get-Command Rscript.exe -ErrorAction SilentlyContinue)) {
    throw 'Rscript.exe finnes ikke i PATH.'
}
& Rscript.exe $test
if ($LASTEXITCODE -ne 0) { throw "Syntetisk test feilet med kode $LASTEXITCODE." }
Write-Host 'Syntetisk ende-til-ende-test bestatt.' -ForegroundColor Green
