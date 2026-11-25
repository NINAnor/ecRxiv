param(
    [string]$Sommer,
    [string]$September = 'C:\NO_GJEN_002_BUSK_05M\ASO_MIDT_NORGE_SEPTEMBER_39',
    [string]$Rot = 'C:\NO_GJEN_002_BUSK_05M',
    [string]$Ut = 'C:\NO_GJEN_002_BUSK_05M\ASO_KANTKONTROLL',
    [string]$GpuPython
)
$ErrorActionPreference = 'Stop'
if (!$Sommer) {
    $matches = @(
        Get-ChildItem -LiteralPath $Rot -Directory | ForEach-Object {
            $plan = Join-Path $_.FullName 'jobb\modellgrunnlag\plan.json'
            $rastermappe = Join-Path $_.FullName 'hoyder\predictions'
            if ((Test-Path -LiteralPath $plan) -and (Test-Path -LiteralPath $rastermappe)) {
                $data = Get-Content -LiteralPath $plan -Raw | ConvertFrom-Json
                if ($data.polygons.id -contains 'ASO_NINFP2110011521' -and
                    $data.polygons.id -contains 'ASO_NINFP2110020530') {
                    $_.FullName
                }
            }
        }
    )
    if ($matches.Count -ne 1) {
        throw 'Fant ikke entydig ASO-sommermappe. Oppgi -Sommer med mappen over hoyder og jobb.'
    }
    $Sommer = $matches[0]
}
if (!$GpuPython) {
    $GpuPython = @(
        (Join-Path $env:LOCALAPPDATA 'miniconda3\envs\no_gjen_002\python.exe'),
        (Join-Path $env:USERPROFILE 'miniconda3\envs\no_gjen_002\python.exe'),
        'C:\ProgramData\miniconda3\envs\no_gjen_002\python.exe'
    ) | Where-Object {Test-Path -LiteralPath $_ -PathType Leaf} | Select-Object -First 1
}
if (!$GpuPython) {throw 'Oppgi -GpuPython med banen til python.exe i no_gjen_002.'}
& $GpuPython (Join-Path $PSScriptRoot 'kantkontroll.py') --sommer $Sommer --september $September --ut $Ut
if ($LASTEXITCODE -ne 0) {throw "Kantkontrollen stoppet med kode $LASTEXITCODE."}
