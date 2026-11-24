# Current analysis inputs

The CSV files are frozen inputs for the reported marginal-model estimates. modellgrunnlag.csv has 299 ANO points and 339 ASO polygons. gruk_modellgrunnlag.csv has 263 GRUK polygons. Both use the 0.8 m threshold and the 0/25/100 percent scoring curve. GRUK is excluded from the combined model. These object-level summaries contain identifiers, reporting regions and scores; imagery, model weights and original field exports remain in the project archive.

ANO uses naturovervaking_eksport.gpkg and the latest recorded visit through 2024. ASO uses ASO_naturområder_21-23.shp. GRUK uses GRUK_2020-24_polygoner.shp and the investigated GRUK object summaries. The public programme citations in the QMD describe the source programmes. Export timestamps, public-release matches and the provenance of the project region layer remain to be added from the original export records.

To regenerate cover and mask statistics, the preparation scripts require the dated input packages and stored height rasters. beregn.py expects grunnlag/ beside that script; kantkontroll.py imports beregn.py. klargjor.py takes the curve package ZIP and the ASO core-report ZIP. beregn_gruk.py expects kilde.csv beside it. These upstream dependencies are external to the small statistical input snapshot in this directory.
