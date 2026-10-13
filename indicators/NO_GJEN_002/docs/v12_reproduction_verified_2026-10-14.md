# Verified V1.2 reproduction

The user-run R reproduction used 20,000 replicates and seed 20260930 with original regional offsets. All 3,759 object CSV rows match in every field. All six national/regional estimates and both interval endpoints match numerically. Two historical area-label encodings differ; numerical results are unchanged. The national estimate is 0.917019138187917, with interval [0.890955340718805, 0.939155228159990]. The national bootstrap cluster union contains 587 clusters.

This supersedes statements in historical audit notes that the full bootstrap had not yet been rerun. It verifies aggregation from frozen objects and the original ANO frame/source, not new canopy inference or upstream GEE export generation.
