#!/usr/bin/env python3
"""Audit raw vegHeights exports and build a provisional good-state table.

Only files named <ecosystem>_ref_<tile>.csv are used for reference values.
Population samples are summarized for QA but never mixed into the reference.
The output remains provisional until the canopy-height model and aggregation
choice have been approved by the indicator owner.
"""

from __future__ import annotations

import argparse
import csv
import math
from collections import defaultdict
from pathlib import Path
from statistics import median


ECOSYSTEMS = {
    "aapne": "Naturlig aapne",
    "semi": "Semi-naturlig",
    "vaatmark": "Vaatmark",
}


def quantile(values: list[float], probability: float) -> float:
    ordered = sorted(values)
    if not ordered:
        return math.nan
    position = (len(ordered) - 1) * probability
    lower = math.floor(position)
    upper = math.ceil(position)
    if lower == upper:
        return ordered[lower]
    return ordered[lower] + (ordered[upper] - ordered[lower]) * (position - lower)


def read_exports(root: Path):
    grouped: dict[tuple[str, str, int, int], list[float]] = defaultdict(list)
    file_counts: dict[tuple[str, str], list[int]] = defaultdict(lambda: [0, 0])
    rejected: list[tuple[str, str]] = []

    for path in sorted(root.glob("*.csv")):
        stem = path.stem.split("_")
        if len(stem) < 2 or stem[0] not in ECOSYSTEMS or stem[1] not in {"ref", "pop"}:
            continue
        ecosystem, sample_type = stem[0], stem[1]
        file_counts[(ecosystem, sample_type)][0] += 1
        if path.stat().st_size <= 2:
            continue
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            reader = csv.DictReader(handle)
            required = {"chm", "region_id", "vegClimZone"}
            if not required.issubset(reader.fieldnames or []):
                rejected.append((path.name, "mangler chm/region_id/vegClimZone"))
                continue
            accepted = 0
            for row_number, row in enumerate(reader, start=2):
                try:
                    height = float(row["chm"])
                    region = int(round(float(row["region_id"])))
                    zone = int(round(float(row["vegClimZone"])))
                    if not math.isfinite(height) or not 1 <= region <= 5 or not 1 <= zone <= 5:
                        raise ValueError
                except (TypeError, ValueError):
                    rejected.append((f"{path.name}:{row_number}", "ugyldig verdi"))
                    continue
                grouped[(ecosystem, sample_type, region, zone)].append(height)
                accepted += 1
            if accepted:
                file_counts[(ecosystem, sample_type)][1] += 1
    return grouped, file_counts, rejected


def write_csv(path: Path, fieldnames: list[str], rows: list[dict]):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=Path)
    parser.add_argument("--output-dir", type=Path, default=Path("vegheights_audit"))
    args = parser.parse_args()

    grouped, file_counts, rejected = read_exports(args.input_dir)
    audit_rows = []
    for (eco, sample_type, region, zone), values in sorted(grouped.items()):
        audit_rows.append({
            "ecosystem": ECOSYSTEMS[eco],
            "sample_type": sample_type,
            "region_id": region,
            "bioclimatic_zone": zone,
            "n": len(values),
            "min_m": f"{min(values):.6f}",
            "p05_m": f"{quantile(values, 0.05):.6f}",
            "median_m": f"{median(values):.6f}",
            "mean_m": f"{sum(values) / len(values):.6f}",
            "p95_m": f"{quantile(values, 0.95):.6f}",
            "max_m": f"{max(values):.6f}",
        })
    write_csv(args.output_dir / "vegheights_stratum_audit.csv", list(audit_rows[0]), audit_rows)

    reference_rows = []
    for row in audit_rows:
        if row["sample_type"] != "ref":
            continue
        reference_rows.append({
            "ecosystem": row["ecosystem"],
            "region_id": row["region_id"],
            "bioclimatic_zone": row["bioclimatic_zone"],
            "reference_height_m": row["median_m"],
            "n_reference": row["n"],
            "provisional": "TRUE",
        })
    write_csv(
        args.output_dir / "reference_good_provisional.csv",
        ["ecosystem", "region_id", "bioclimatic_zone", "reference_height_m", "n_reference", "provisional"],
        reference_rows,
    )

    file_rows = [{
        "ecosystem": ECOSYSTEMS[eco], "sample_type": typ,
        "files_planned": counts[0], "files_with_rows": counts[1],
        "empty_files": counts[0] - counts[1],
    } for (eco, typ), counts in sorted(file_counts.items())]
    write_csv(args.output_dir / "vegheights_file_audit.csv", list(file_rows[0]), file_rows)

    missing = []
    for eco in ECOSYSTEMS:
        for region in range(1, 6):
            for zone in range(1, 6):
                if (eco, "ref", region, zone) not in grouped:
                    missing.append({"ecosystem": ECOSYSTEMS[eco], "region_id": region, "bioclimatic_zone": zone})
    write_csv(args.output_dir / "missing_reference_strata.csv",
              ["ecosystem", "region_id", "bioclimatic_zone"], missing)
    print(f"Reference strata: {len(reference_rows)}; missing of 75: {len(missing)}; rejected rows: {len(rejected)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
