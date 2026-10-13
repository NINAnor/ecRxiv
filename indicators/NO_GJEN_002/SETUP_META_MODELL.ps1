param(
    [string]$EnvironmentName = 'no_gjen_002',
    [switch]$SkipEnvironment
)

$ErrorActionPreference = 'Stop'
$root = $PSScriptRoot
$repo = Join-Path $root 'external\HighResCanopyHeight'
$checkpoints = Join-Path $repo 'saved_checkpoints'
$config = Join-Path $root 'config.yml'

function Require-Command {
    param([string]$Name, [string]$InstallHint)
    if (-not (Get-Command $Name -ErrorAction SilentlyContinue)) {
        throw "Mangler $Name. $InstallHint"
    }
}

Require-Command 'git.exe' 'Installer Git for Windows: winget install --id Git.Git -e'
Require-Command 'aws.exe' 'Installer AWS CLI: winget install --id Amazon.AWSCLI -e'
Require-Command 'conda.exe' 'Installer Miniconda: winget install --id Anaconda.Miniconda3 -e'

if (-not (Test-Path (Join-Path $repo '.git'))) {
    New-Item -ItemType Directory -Force -Path (Split-Path $repo) | Out-Null
    & git.exe clone 'https://github.com/facebookresearch/HighResCanopyHeight.git' $repo
    if ($LASTEXITCODE -ne 0) { throw 'Git-kloning feilet.' }
}

$revision = (& git.exe -C $repo rev-parse HEAD).Trim()
if ($LASTEXITCODE -ne 0 -or $revision -notmatch '^[0-9a-f]{40}$') {
    throw 'Kunne ikke lese Git-revisjonen til Meta-modellen.'
}
New-Item -ItemType Directory -Force -Path $checkpoints | Out-Null

$bucket = 's3://dataforgood-fb-data/forests/v1/models/saved_checkpoints'
$weights = @(
    'compressed_SSLhuge_aerial.pth',
    'aerial_normalization_quantiles_predictor.ckpt'
)
foreach ($name in $weights) {
    $target = Join-Path $checkpoints $name
    if (-not (Test-Path $target) -or (Get-Item $target).Length -eq 0) {
        Write-Host "Laster ned $name ..." -ForegroundColor Cyan
        & aws.exe s3 cp "$bucket/$name" $target --no-sign-request
        if ($LASTEXITCODE -ne 0) { throw "Nedlasting feilet: $name" }
    }
    else {
        Write-Host "Finnes allerede: $name" -ForegroundColor DarkGray
    }
}

if (-not $SkipEnvironment) {
    $envs = (& conda.exe env list --json | ConvertFrom-Json).envs
    $exists = @($envs | Where-Object { (Split-Path $_ -Leaf) -eq $EnvironmentName }).Count -gt 0
    if (-not $exists) {
        & conda.exe create -n $EnvironmentName python=3.9 -y
        if ($LASTEXITCODE -ne 0) { throw 'Kunne ikke opprette Conda-miljoet.' }
    }
    & conda.exe install -n $EnvironmentName pytorch==2.0.1 torchvision==0.15.2 cpuonly -c pytorch -y
    if ($LASTEXITCODE -ne 0) { throw 'Installasjon av PyTorch feilet.' }
    & conda.exe run -n $EnvironmentName python -m pip install -r (Join-Path $root 'python\requirements.txt')
    if ($LASTEXITCODE -ne 0) { throw 'Installasjon av Python-pakker feilet.' }
    $python = (& conda.exe run -n $EnvironmentName python -c 'import sys; print(sys.executable)' |
        Where-Object { $_ -match 'python\.exe$' } | Select-Object -Last 1).Trim()
    if (-not (Test-Path $python)) { throw 'Fant ikke Python-programmet i Conda-miljoet.' }
    $pythonYaml = $python.Replace('\', '/')
}
else {
    $pythonYaml = 'python'
}

$text = Get-Content -Raw -Encoding UTF8 $config
$text = $text -replace '(?m)^(  python:)\s*.*$', "`$1 $pythonYaml"
$text = $text -replace '(?m)^(  repository_revision:)\s*.*$', "`$1 $revision"
[IO.File]::WriteAllText($config, $text, [Text.UTF8Encoding]::new($false))

Write-Host ''
Write-Host 'Meta-modellen er installert og config.yml er oppdatert.' -ForegroundColor Green
Write-Host "Modellmappe: $repo"
Write-Host "Git-revisjon: $revision"
Write-Host 'Neste kontroll:'
Write-Host '& .\KONTROLLER_OPPSETT.ps1'
