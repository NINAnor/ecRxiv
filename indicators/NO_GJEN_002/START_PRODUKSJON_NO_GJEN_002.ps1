param(
    [ValidateSet('Kontroll','Pilot','Full')][string]$Mode = 'Kontroll',
    [ValidateRange(5,500)][int]$PilotTiles = 40,
    [switch]$ByggManifestPaaNytt
)

$ErrorActionPreference='Stop'
$root=$PSScriptRoot;$config=Join-Path $root 'config.yml';$runner=Join-Path $root 'R\run_pipeline.R'
$archive='C:\nib-norge\landsdekkende_2m';$archiveManifest=Join-Path $archive 'ORTOFOTO_PRODUKSJONSGRUNNLAG_v9.txt'
$logDir=Join-Path $root 'logs';New-Item -ItemType Directory -Force -Path $logDir|Out-Null
$log=Join-Path $logDir ("produksjon_{0}_{1}.log" -f $Mode,(Get-Date -Format 'yyyyMMdd_HHmmss'))

function Find-Rscript {
    $cmd=Get-Command Rscript.exe -ErrorAction SilentlyContinue;if($cmd){return $cmd.Source}
    $hits=@(Get-ChildItem 'C:\Program Files\R' -Directory -ErrorAction SilentlyContinue|Sort-Object Name -Descending|ForEach-Object{Join-Path $_.FullName 'bin\Rscript.exe'}|Where-Object{Test-Path $_ -PathType Leaf})
    if($hits.Count){return $hits[0]};throw 'Fant ikke Rscript.exe. Installer R eller legg R i PATH.'
}
function Run-Stage([string]$Stage,[switch]$Overwrite){
    $args=@($runner,'--config',$config,'--stage',$Stage);if($Overwrite){$args+='--overwrite'}
    Write-Host "Starter trinn: $Stage" -ForegroundColor Cyan
    & $script:rscript @args
    if($LASTEXITCODE -ne 0){throw "Trinnet $Stage stoppet med kode $LASTEXITCODE."}
}

if (-not (Test-Path $config -PathType Leaf) -or -not (Test-Path $runner -PathType Leaf)){throw 'Produksjonspakken er ufullstendig.'}
if (-not (Test-Path $archive -PathType Container)){throw "Mangler ortofotoarkiv: $archive"}
if (-not (Test-Path $archiveManifest -PathType Leaf)){throw "Mangler godkjent produksjonsmanifest: $archiveManifest"}
$cfg=Get-Content $config -Raw
if($cfg -notmatch '(?m)^\s*reporting_year:\s*2024\s*$'){throw 'config.yml har ikke rapporteringsaar 2024.'}
if($cfg -notmatch '(?m)^\s*resolution_status:\s*experimental_2m\s*$'){throw 'Eksperimentell 2 m-status mangler i config.yml.'}
$rscript=Find-Rscript

Start-Transcript -Path $log|Out-Null
try{
    Write-Host '';Write-Host "NO_GJEN_002 0.3.0-rc1 - $Mode" -ForegroundColor Cyan
    Write-Host "Rapporteringsaar: 2024";Write-Host "Ortofoto:          $archive";Write-Host "Logg:              $log"
    Write-Warning 'Meta-modellen er ikke validert ved 2 m. Resultatene blir eksperimentelle og krever flyfotobasert rekalibrering.'
    Run-Stage 'preflight'
    if($Mode -eq 'Kontroll'){Write-Host 'Forhaandskontrollen er godkjent.' -ForegroundColor Green;return}
    Run-Stage 'manifest' -Overwrite:$ByggManifestPaaNytt
    if($Mode -eq 'Pilot'){
        $env:NO_GJEN_MAX_TILES=[string]$PilotTiles
        try{Run-Stage 'inference'}finally{Remove-Item Env:NO_GJEN_MAX_TILES -ErrorAction SilentlyContinue}
        Write-Host "Piloten er ferdig for inntil $PilotTiles geografisk spredte fliser." -ForegroundColor Green
        Write-Host 'Kontroller prediksjonsrastrene foer full kjoering.';return
    }
    $answer=Read-Host 'Skriv PRODUSER EKSPERIMENTELL for aa starte/fortsette full kjoering'
    if($answer -cne 'PRODUSER EKSPERIMENTELL'){Write-Host 'Avbrutt.';return}
    Remove-Item Env:NO_GJEN_MAX_TILES -ErrorAction SilentlyContinue
    Run-Stage 'inference';Run-Stage 'zonal';Run-Stage 'indicator'
    $summary=Join-Path $root 'outputs\NO_GJEN_002_national_regions.csv'
    if (-not (Test-Path $summary -PathType Leaf)){throw "Resultattabellen mangler: $summary"}
    $rows=@(Import-Csv $summary)
    if($rows.Count -ne 6){throw "Forventet Norge og fem regioner, men resultatfilen har $($rows.Count) rader."}
    if(@($rows|Where-Object reporting_year -ne '2024').Count){throw 'Resultatfilen har feil rapporteringsaar.'}
    Write-Host '';Write-Host 'NO_GJEN_002-produksjonen er ferdig.' -ForegroundColor Green
    Write-Host "Resultater: $summary";Write-Host 'Status: EKSPERIMENTELL_2M - IKKE ENDELIG KALIBRERT'
}finally{Stop-Transcript|Out-Null}
