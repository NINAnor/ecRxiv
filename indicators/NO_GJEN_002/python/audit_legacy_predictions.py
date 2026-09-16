#!/usr/bin/env python3
"""Summarize the legacy prediction/CHM table without treating it as production."""

from __future__ import annotations

import argparse
import csv
import math
from collections import defaultdict
from pathlib import Path
from statistics import median


def metrics(predicted: list[float], observed: list[float]) -> dict[str, float]:
    n = len(predicted)
    errors = [p - o for p, o in zip(predicted, observed)]
    mean_p = sum(predicted) / n
    mean_o = sum(observed) / n
    covariance = sum((p - mean_p) * (o - mean_o) for p, o in zip(predicted, observed))
    variance_p = sum((p - mean_p) ** 2 for p in predicted)
    variance_o = sum((o - mean_o) ** 2 for o in observed)
    correlation = covariance / math.sqrt(variance_p * variance_o) if variance_p and variance_o else math.nan
    return {
        "n": n,
        "prediction_median_m": median(predicted),
        "observed_median_m": median(observed),
        "bias_m": sum(errors) / n,
        "mae_m": sum(abs(e) for e in errors) / n,
        "rmse_m": math.sqrt(sum(e * e for e in errors) / n),
        "correlation": correlation,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_csv", type=Path)
    parser.add_argument("output_csv", type=Path)
    args = parser.parse_args()
    groups: dict[tuple[str, str, str], tuple[list[float], list[float]]] = defaultdict(lambda: ([], []))
    raw_pairs: list[tuple[float, float]] = []

    with args.input_csv.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        required = {"hvdksys", "tilstnd", "Pred_media", "PredAj_med", "chm_median"}
        if not required.issubset(reader.fieldnames or []):
            raise SystemExit("Input lacks required legacy fields.")
        for row in reader:
            try:
                observed = float(row["chm_median"])
                raw = float(row["Pred_media"])
                adjusted = float(row["PredAj_med"])
            except (TypeError, ValueError):
                continue
            raw_pairs.append((raw, adjusted))
            state = row["tilstnd"] or "mangler"
            for label, prediction in (("Pred_media", raw), ("PredAj_med", adjusted)):
                predicted, observations = groups[(row["hvdksys"], state, label)]
                predicted.append(prediction)
                observations.append(observed)

    # The legacy adjustment is deterministic. Verify and expose it rather than
    # carrying an undocumented transformed column into the production model.
    max_residual = max(abs(adjusted - ((25.0 / 18.0) * raw - 0.6875)) for raw, adjusted in raw_pairs)
    if max_residual > 1e-5:
        raise SystemExit(f"Unexpected legacy adjustment; max residual {max_residual}")

    rows = []
    for (ecosystem, state, prediction), (predicted, observed) in sorted(groups.items()):
        row = {"ecosystem": ecosystem, "condition": state, "prediction": prediction}
        row.update(metrics(predicted, observed))
        rows.append(row)
    args.output_csv.parent.mkdir(parents=True, exist_ok=True)
    with args.output_csv.open("w", encoding="utf-8", newline="") as handle:
        fields = list(rows[0])
        writer = csv.DictWriter(handle, fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {len(rows)} group summaries. Legacy adjustment max residual: {max_residual:.3g}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
