# X0 provenance audit — 14 October 2026

The original input `vegHeights_skog_climZoneRegion.csv` and the archived builder `BYGG_OG_TEST_SKOGREFERANSE_V0_4.ps1` have been recovered. The enclosed Python is extracted unchanged from that PowerShell script.

The input contains 25 unique region–bioclimatic-zone cells. The builder maps `p90_height` to `forest_height_m`, retains Moen KLASSE zone codes 1–5, and repeats the appropriate region–zone value across the 57 ecosystem lookup rows selected by the good-condition reference table. No new forest prediction or ecosystem-specific forest fit is performed by this builder.

All 57 delivered lookup rows match the source values formatted to 15 significant digits, exactly as implemented. Maximum numeric rounding difference is 4.973799150320701e-14 m. Earlier object audits verified all 3,759 V1.2 X0 lookup values.

This establishes reproducibility from the retained GEE export to the X0 lookup. It does not establish how the GEE `p90_height` export was generated. The original forest selection, height product, pixel/polygon aggregation, masking and GEE script remain to be recovered. The column name alone is not evidence of those choices.

The extracted builder requires the original good-reference table and configuration and can write candidate files. Retain it as provenance; do not run it against an active delivery directory without an isolated copy.

The independent V1.2 reproduction has separately verified all object CSV fields and all six national/regional estimates and interval endpoints using 20,000 Bayesian spatial cluster-bootstrap replicates and the original seed. This result-level reproducibility does not substitute for upstream reference provenance.
