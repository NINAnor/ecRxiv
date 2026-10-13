#requires -Version 5.1
param(
 [Parameter(Mandatory)][string]$Frame,
 [Parameter(Mandatory)][string]$PolygonResults,
 [Parameter(Mandatory)][string]$V11Table,
 [Parameter(Mandatory)][string]$AnoGpkg,
 [Parameter(Mandatory)][string]$OutputDirectory,
 [string]$Rscript='Rscript.exe',
 [ValidateRange(1000,100000)][int]$BootstrapReplicates=20000,
 [int]$BootstrapSeed=20260930
)
$ErrorActionPreference='Stop'
foreach($p in @($Frame,$PolygonResults,$V11Table,$AnoGpkg)) {
 if(!(Test-Path -LiteralPath $p -PathType Leaf)){throw "Required input missing: $p"}
}
if(Test-Path -LiteralPath $OutputDirectory){throw 'Choose a new output directory to preserve the frozen delivery.'}
$analysis=Join-Path $PSScriptRoot '..\R\calculate_gruk_calibrated_results_v1_2.R'
$savedPreference=$ErrorActionPreference
try {
 $ErrorActionPreference='Continue'
 & $Rscript $analysis $Frame $PolygonResults $V11Table $AnoGpkg $OutputDirectory $BootstrapReplicates $BootstrapSeed 2>&1 |
  ForEach-Object { $_.ToString() }
 $rExitCode=$LASTEXITCODE
} finally {
 $ErrorActionPreference=$savedPreference
}
if($rExitCode -ne 0){throw "V1.2 analysis failed: $rExitCode"}
