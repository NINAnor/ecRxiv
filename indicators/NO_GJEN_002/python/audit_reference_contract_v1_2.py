#!/usr/bin/env python3
"""Audit frozen V1.2 X100/X0 values against repository candidate tables.

No model inference is run and no input is modified.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
from collections import Counter
from pathlib import Path


def number(value: str, decimal_comma: bool = False) -> float:
    text = (value or "").strip()
    if decimal_comma:
        text = text.replace(",", ".")
    return float(text)


def read_rows(path: Path, delimiter: str = ",", decimal_comma: bool = False):
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle, delimiter=delimiter)), decimal_comma


def key(row, ecosystem="ecosystem", region="region_id", zone="bioclimatic_zone"):
    return (str(row[ecosystem]).strip(), str(row[region]).strip(), str(row[zone]).strip())


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("delivery_csv", type=Path)
    parser.add_argument("good_reference_csv", type=Path)
    parser.add_argument("forest_reference_csv", type=Path)
    parser.add_argument("output_dir", type=Path)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    objects, _ = read_rows(args.delivery_csv, delimiter=";", decimal_comma=True)
    good, _ = read_rows(args.good_reference_csv)
    forest, _ = read_rows(args.forest_reference_csv)
    good_lookup = {key(r): number(r["reference_height_m"]) for r in good}
    forest_lookup = {key(r): number(r["forest_height_m"]) for r in forest}

    detailed = []
    counts = Counter()
    max_good = 0.0
    max_forest = 0.0
    for row in objects:
        k = (row["indicator_ecosystem"].strip(), row["region_id"].strip(), row["moen_zone_code"].strip())
        frozen_good = number(row["good_height_m"], True)
        frozen_forest = number(row["forest_height_m"], True)
        candidate_good = good_lookup.get(k, math.nan)
        candidate_forest = forest_lookup.get(k, math.nan)
        good_diff = frozen_good - candidate_good
        forest_diff = frozen_forest - candidate_forest
        good_match = math.isfinite(good_diff) and abs(good_diff) <= 1e-12
        forest_match = math.isfinite(forest_diff) and abs(forest_diff) <= 1e-12
        counts["objects"] += 1
        counts["good_exact"] += int(good_match)
        counts["forest_exact"] += int(forest_match)
        counts["good_missing"] += int(not math.isfinite(candidate_good))
        counts["forest_missing"] += int(not math.isfinite(candidate_forest))
        if math.isfinite(good_diff):
            max_good = max(max_good, abs(good_diff))
        if math.isfinite(forest_diff):
            max_forest = max(max_forest, abs(forest_diff))
        detailed.append({
            "delivery_id": row["delivery_id"], "source_dataset": row["source_dataset"],
            "ecosystem": k[0], "region_id": k[1], "bioclimatic_zone": k[2],
            "frozen_X100_m": frozen_good, "candidate_X100_m": candidate_good,
            "X100_difference_m": good_diff, "X100_exact_match": good_match,
            "frozen_X0_m": frozen_forest, "candidate_X0_m": candidate_forest,
            "X0_difference_m": forest_diff, "X0_exact_match": forest_match,
        })

    detail_path = args.output_dir / "reference_contract_object_audit_v1_2.csv"
    with detail_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(detailed[0]))
        writer.writeheader()
        writer.writerows(detailed)

    forest_by_stratum = {}
    for row in forest:
        stratum = (row["region_id"], row["bioclimatic_zone"])
        forest_by_stratum.setdefault(stratum, set()).add(number(row["forest_height_m"]))
    summary = {
        **counts,
        "maximum_absolute_X100_difference_m": max_good,
        "maximum_absolute_X0_difference_m": max_forest,
        "forest_strata_with_ecosystem_specific_values": sum(len(v) > 1 for v in forest_by_stratum.values()),
        "interpretation": {
            "X100": "Repository candidate table is not the frozen V1.2 X100 table.",
            "X0": "Repository forest table reproduces frozen V1.2 X0 exactly.",
        },
    }
    (args.output_dir / "reference_contract_summary_v1_2.json").write_text(
        json.dumps(summary, ensure_ascii=False, indent=2), encoding="utf-8")
    print(json.dumps(summary, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
