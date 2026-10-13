# NO_GJEN_002 reference audit, 2026-10-06

## Scope

This audit compares the reference values stored in the frozen V1.2 delivery
with the candidate 1 m lookup tables currently present in the repository. It
does not rerun canopy-height inference and does not alter the delivery.

## Results

- Frozen delivery objects: 3,759.
- The documented condition function reproduces every frozen object value
  exactly after the production rounding to four decimals.
- `reference_forest_1m_v0_4.csv` matches X0 for all 3,759 objects exactly.
- `reference_good_1m_v0_4.csv` matches X100 for 0 of 3,759 objects exactly.
- Maximum absolute X100 difference: 0.608711 m.
- Every delivery stratum is present in both repository lookup tables.
- Forest values are identical across ecosystems within each available
  region-bioclimate stratum.
- The legacy source `vegHeights_skog_climZoneRegion.csv` was not found in the
  repository or runtime collection.

## Interpretation

The discrepancy is a version/provenance problem, not a reason to alter the
frozen V1.2 result. The repository candidate X100 table predates the X100 table
used by the delivery. It must not be substituted silently. The generating
reference package, including source-object identifiers, inclusion rules,
height metric, aggregation and file hashes, should be committed before the
reference calculation is promoted from candidate status.

The X0 lookup is technically reproducible from the committed lookup table, but
its statistical origin cannot be regenerated until the mature-forest source
and generation script are available. The current production code does not
apply a separate 0.8 m X0 override to naturally open ecosystems.

## X60

X60 is implicit in the current sigmoid rather than independently estimated:

`X60 = X100 + 0.400116533 * (X0 - X100)`.

This is practically the same position as linear scaling between the two
endpoints. Alternative ecological X60 values therefore represent a deliberate
method change and should be versioned and sensitivity-tested.
