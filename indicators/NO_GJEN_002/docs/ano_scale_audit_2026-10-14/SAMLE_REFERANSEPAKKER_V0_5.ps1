param([string]$RuntimeRoot='C:\NO_GJEN_002_RUNTIME',[string]$OutputRoot='C:\NO_GJEN_002_AUD_KONTROLL')
$ErrorActionPreference='Stop'
$stamp=Get-Date -Format 'yyyyMMdd_HHmmss';$stage=Join-Path $OutputRoot ('referanser_v05_'+$stamp)
New-Item -ItemType Directory -Path $stage -Force | Out-Null
$names=@('x100_gpu_reference_package_v0_5','x100_gpu_reference_candidate_v0_5','x100_gpu_calibration_v0_5','x100_gpu_cpu_paired_chain_audit_v0_5e','cpu_gpu_noninferiority_v0_5b')
$missing=New-Object 'System.Collections.Generic.List[string]'
foreach($name in $names){
 $src=Join-Path $RuntimeRoot $name
 if(!(Test-Path -LiteralPath $src)){$missing.Add($src);continue}
 Get-ChildItem -LiteralPath $src -Recurse -File | Where-Object { $_.Extension -in @('.csv','.json','.yml','.yaml','.md','.txt','.R','.py','.ps1') } | ForEach-Object {
  $rel=$_.FullName.Substring($RuntimeRoot.TrimEnd('\').Length).TrimStart('\');$dest=Join-Path $stage $rel
  New-Item -ItemType Directory -Force -Path (Split-Path $dest -Parent) | Out-Null
  Copy-Item -LiteralPath $_.FullName -Destination $dest
 }
}
$missing | Set-Content -LiteralPath (Join-Path $stage 'mangler.txt') -Encoding UTF8
$files=@(Get-ChildItem -LiteralPath $stage -File -Recurse)
$files | ForEach-Object {[pscustomobject]@{Fil=$_.FullName.Substring($stage.Length+1);SHA256=(Get-FileHash -LiteralPath $_.FullName).Hash;Bytes=$_.Length}} | Export-Csv -LiteralPath (Join-Path $stage 'filmanifest.csv') -Delimiter ';' -NoTypeInformation -Encoding UTF8
Compress-Archive -Path (Join-Path $stage '*') -DestinationPath ($stage+'.zip')
Write-Host "FERDIG: $stage.zip" -ForegroundColor Green
if($missing.Count){Write-Host 'Mapper ikke funnet:';$missing | ForEach-Object {Write-Host $_}}
