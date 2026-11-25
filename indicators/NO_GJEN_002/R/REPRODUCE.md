# Reproduce the reported statistical results

Run from a separate output directory, with R and glmmTMB installed:

```powershell
& 'C:\Programs\R\R-4.4.3\bin\Rscript.exe' C:\ecRxiv_Vegar\indicators\NO_GJEN_002\R\coverage2024\KJOR_USIKKERHET.R C:\ecRxiv_Vegar\indicators\NO_GJEN_002\data\coverage2024\modellgrunnlag.csv C:\ecRxiv_Vegar\indicators\NO_GJEN_002\data\coverage2024\gruk_modellgrunnlag.csv
```

The script reads `delta.R` and `referanse_punktestimater.csv` alongside itself. It verifies previously reported point estimates before writing new CSV outputs in the working directory. Archived outputs are in `results/coverage2024`. The two corrected R expressions from the successful local run are retained.

Render the QMD from the repository using Quarto with the R dependencies required by the shared `_common.R` template helper. Data/imagery preparation and statistical estimation are distinct steps; see [`data/coverage2024/INPUTS.md`](../data/coverage2024/INPUTS.md) and the code map in the QMD for the upstream requirements.
