# Aud review: verified changes and remaining reproduction requirements

## Evidence and scope

Prepared from the saved 6 October template package, source-code package, original V1.2 PowerShell script and the archived reference-discussion workbook. These are available archive snapshots, not a verified checkout of PR #545. The live PR and issue #155 could not be retrieved in this environment. Compare with the current branch before applying; preserve later author edits and existing bibliography entries.

## Review points

| Point | Action | Validation / remaining work |
|---|---|---|
| Plain-language method | Added Section 2.6, including pixel prediction, polygon summary, reference scaling, interpretation and aggregation | Checked against saved code and delivered object values |
| Code map | Added Section 7.0 | Paths checked against available source packages; verify current checkout paths |
| Missing GRUK and bootstrap code | Added standalone R extracted unchanged from V1.2 launcher, plus explicit-path PowerShell runner | Extracted R body byte-for-byte equivalent; full run requires four external input files |
| Frozen meaning | Defined delivery snapshot and later versioning | Added recovery CSV and source SHA-256 provenance; generation data are still missing |
| Wetlands | Corrected delivery scope, retaining broader development ambition | 3113 naturally open and 646 semi-natural assigned objects; zero wetland group and zero V-series codes. 2849 missing main-type entries and one T4 entry require later classification audit |
| Commit hash | Explained exact external model source-code revision | Link included; checkpoint hashes remain separate |
| Abbreviations | Defined Norwegian programme/habitat names at first prose use | ASO is semi-natural grassland; no invented official English expansion |
| References | Added supplemental BibTeX and citations | Tolan, Rubin and programme pages checked; authoritative NiN methodology/reference-version entry still needed; catalogue metadata marked for verification |
| here source path | Uses source(here::here("_common.R")) | Already present in archived template version; ensure live branch agrees |
| Bayesian terminology | Retained and explained | Code draws independent rexp weights per cluster, shared between ANO frame and outcome objects |
| X60 discussion | Preserved delivered transform | Implicit X60 is 0.400116533 of X100–X0 interval; independent ecological X60 is a new method version, not an editorial correction |

## Numerical audit performed

- 3759 unique outcome objects.
- All object scores reproduce after four-decimal production rounding.
- National calibration weights sum to 0.9999999999999689.
- Weighted national estimate: 0.9170191381878878, rounding to 0.9170.
- No new GPU inference, no new reference choices, no change to delivered result.

Run `python scripts/verify_recovered_delivery.py` from any directory to repeat these checks.

## Bootstrap contract

The unchanged original R algorithm consumes the approved combined_delivery_frame layer, frozen polygon_results layer, V1.1G comparison CSV, and original ANO_SurveyPoint layer. It uses 20000 replicates and base seed 20260930, with reporting-area offsets. Cluster weights are Exp(1), with the cluster universe formed from ANO-frame and outcome clusters together. These same weights re-estimate the ANO GRUK proportion and both area-weighted domain means. The 95% limits are the 2.5%/97.5% quantiles.

The full ANO frame is the denominator, not just image-complete ANO outcome objects. This distinction must be retained. When a regional ANO frame contains no GRUK observations, Bayesian weights do not create one; regional share stays zero.

The reported intervals 0.8910–0.9392 were not re-run here. Raw ANO source and frame GeoPackages are absent from the recovered packages. An unchanged code extraction is evidence of preserved algorithm, not independent reproduction of its full run.

## Frozen reference provenance

The recovery CSV retains X100/X0 and object scores from the archived workbook. It is sufficient for the point estimate audit, but not a reconstruction of the original X100 generation package. The candidate reference_good_1m_v0_4.csv must not silently replace these values. The forest lookup matches the delivered X0 according to the earlier audit, but its source population and generation script remain to be recovered.

## Before integration and claiming full reproducibility

1. Compare QMD against current branch; this package intentionally does not overwrite `_common.R` or an existing references.bib.
2. Resolve bibliography duplication by reusing current keys; replace provisional catalogue entry with verified dataset and NiN-version citations.
3. Run unchanged V1.2 R script with original inputs in a new output directory; compare all six estimates and intervals.
4. Recover X100 generation package and X0 generation provenance; record hashes, IDs, filtering and summary definitions.
5. Render QMD with repository dependencies and inspect output. No R/Quarto runtime was available here.
6. Review issue #155 directly before designing a revised X60 method.

Status remains incomplete. This is a review-ready archive revision, not a merged PR or a claim of complete independent reproduction.
