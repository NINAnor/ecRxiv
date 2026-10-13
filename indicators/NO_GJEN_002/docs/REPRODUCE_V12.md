# Reproduce the V1.2 delivery

The committed R algorithm has been rerun with the original inputs: all 3,759 object CSV rows and all six national/regional estimates and interval endpoints match the archived delivery. This run reproduces aggregation and uncertainty from frozen object scores, not canopy inference or the original GEE forest export.

## Required software

R with `sf` and `jsonlite`; R 4.4.3 was used for the verified run. Install missing packages with `install.packages(c("sf", "jsonlite"))`. Quarto and a GPU are not required. On Windows, the PowerShell launcher requires PowerShell 5.1 or later. Rscript must be on PATH or passed using `-Rscript`.

## Required inputs

Obtain the separate `aud_repro_inputs_*.zip` package from Vegar and extract it to a local folder. These original inputs are not supplied by the smaller recovery CSV in Git:

| File | Required layer / role |
|---|---|
| frame.gpkg | combined_delivery_frame; approved ANO/ASO/GRUK delivery frame |
| objects.gpkg | polygon_results; frozen V1.1G object scores |
| v11_table.csv | original V1.1G national/regional table |
| ano.gpkg | ANO_SurveyPoint; original ANO registration source |
| expected_v12/*.csv | retained outputs for comparison, when available |

SHA256SUMS.csv records the supplied input/output snapshot hashes. Keep the inputs unchanged. Choose a new results directory.

## Windows example

Run from the repository root, replacing the input directory:

```powershell
$inputDir = "C:\aud_repro_inputs"
& .\indicators\NO_GJEN_002\scripts\RUN_GRUK_V1_2.ps1 `
 -Frame "$inputDir\frame.gpkg" `
 -PolygonResults "$inputDir\objects.gpkg" `
 -V11Table "$inputDir\v11_table.csv" `
 -AnoGpkg "$inputDir\ano.gpkg" `
 -OutputDirectory "C:\aud_v12_results" `
 -BootstrapReplicates 20000 -BootstrapSeed 20260930
```

If execution policy blocks the launcher, invoke it with `powershell.exe -NoProfile -ExecutionPolicy Bypass -File` followed by the launcher path and the same arguments. Normal R messages written to stderr are displayed; only a nonzero R exit code is treated as failure.

## Direct R invocation (all platforms)

```text
Rscript indicators/NO_GJEN_002/R/calculate_gruk_calibrated_results_v1_2.R frame.gpkg objects.gpkg v11_table.csv ano.gpkg NEW_OUTPUT_DIRECTORY 20000 20260930
```

Use full paths or run in the folder containing the inputs. The original regional seed offsets are implemented in the R code. Expected national output: 0.917019138187917, interval [0.890955340718805, 0.939155228159990]. Compare all six estimates and endpoints with the retained expected tables; small platform-dependent floating-point differences should be distinguished from methodological differences.

## Remaining provenance

X100 GPU reference lookup and X0 lookup building have been audited. The original GEE script generating the forest p90 export remains unavailable and is explicitly documented. It is not required to rerun this frozen V1.2 aggregation.
