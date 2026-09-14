param(
    [ValidateSet('validate','manifest','inference','zonal','indicator','all')]
    [string]$Stage = 'validate',
    [switch]$Overwrite
)

$ErrorActionPreference = 'Stop'
$root = $PSScriptRoot
$config = Join-Path $root 'config.yml'
$runner = Join-Path $root 'R\run_pipeline.R'

if (-not (Get-Command Rscript.exe -ErrorAction SilentlyContinue)) {
    throw 'Rscript.exe finnes ikke i PATH. Installer R og start PowerShell paa nytt.'
}
if (-not (Test-Path $config)) { throw "Mangler $config" }
if (-not (Test-Path $runner)) { throw "Mangler $runner" }

$arguments = @($runner, '--config', $config, '--stage', $Stage)
if ($Overwrite) { $arguments += '--overwrite' }

Write-Host "NO_GJEN_002 - trinn: $Stage" -ForegroundColor Cyan
Write-Host "Konfigurasjon: $config"
& Rscript.exe @arguments
if ($LASTEXITCODE -ne 0) { throw "NO_GJEN_002 stoppet med kode $LASTEXITCODE." }
Write-Host 'Trinnet er ferdig.' -ForegroundColor Green
