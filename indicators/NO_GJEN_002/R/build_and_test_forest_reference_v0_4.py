from __future__ import annotations

import csv
import hashlib
import json
import math
import shutil
import sys
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

repo = Path(sys.argv[1])
source_forest = Path(sys.argv[2])
candidate_root = Path(sys.argv[3])
runtime_root = Path(sys.argv[4])

good_path = candidate_root / "reference_good_1m_v0_4.csv"
forest_output = candidate_root / "reference_forest_1m_v0_4.csv"
config_path = repo / "config_x100_1m_v0_4.yml"
audit_path = runtime_root / "x100_v0_4_forest_denominator_audit.csv"
source_audit_path = runtime_root / "x100_v0_4_forest_source_25_cells.csv"
summary_path = runtime_root / "x100_v0_4_forest_reference_summary.txt"
provenance_path = runtime_root / "x100_v0_4_forest_reference_provenance.json"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, fieldnames: list[str], rows: list[dict]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def write_candidate_safely(path: Path, fieldnames: list[str], rows: list[dict]) -> str:
    temporary = runtime_root / (path.name + ".candidate")
    write_csv(temporary, fieldnames, rows)
    if path.exists():
        if sha256(path) != sha256(temporary):
            temporary.unlink(missing_ok=True)
            raise RuntimeError(f"Eksisterende kandidat avviker og blir ikke overskrevet: {path}")
        temporary.unlink()
        return "identical_existing"
    shutil.copyfile(temporary, path)
    temporary.unlink()
    return "created"


source_rows = read_csv(source_forest)
required_source = {"climZone", "p90_height", "region"}
if not source_rows or not required_source.issubset(source_rows[0]):
    raise RuntimeError("Den opprinnelige skogfilen mangler climZone, p90_height eller region.")

forest_by_cell: dict[tuple[str, str], float] = {}
source_audit: list[dict] = []
for line_number, row in enumerate(source_rows, start=2):
    try:
        region_number = int(round(float(row["region"])))
        zone_number = int(round(float(row["climZone"])))
        height = float(row["p90_height"])
    except (TypeError, ValueError) as exc:
        raise RuntimeError(f"Ugyldig verdi i skogkilden paa linje {line_number}") from exc
    if region_number not in range(1, 6) or zone_number not in range(1, 6):
        raise RuntimeError(f"Ugyldig region/sone paa linje {line_number}")
    if not math.isfinite(height) or height <= 0:
        raise RuntimeError(f"Ugyldig skoghoyde paa linje {line_number}")
    key = (str(region_number), str(zone_number))
    if key in forest_by_cell:
        raise RuntimeError(f"Duplisert region-sonecelle i skogkilden: {key}")
    forest_by_cell[key] = height
    source_audit.append({
        "region_id": region_number,
        "bioclimatic_zone": zone_number,
        "forest_height_m": format(height, ".15g"),
        "source_line": line_number,
    })

expected_cells = {(str(region), str(zone)) for region in range(1, 6) for zone in range(1, 6)}
missing_source_cells = sorted(expected_cells - set(forest_by_cell))
extra_source_cells = sorted(set(forest_by_cell) - expected_cells)
if len(forest_by_cell) != 25 or missing_source_cells or extra_source_cells:
    raise RuntimeError(
        f"Forventet alle 25 region-soneceller; fant {len(forest_by_cell)}. "
        f"Mangler: {missing_source_cells}; ekstra: {extra_source_cells}"
    )
write_csv(
    source_audit_path,
    ["region_id", "bioclimatic_zone", "forest_height_m", "source_line"],
    sorted(source_audit, key=lambda r: (int(r["region_id"]), int(r["bioclimatic_zone"]))),
)

good_rows = read_csv(good_path)
required_good = {"ecosystem", "region_id", "bioclimatic_zone", "reference_height_m"}
if len(good_rows) != 57 or not good_rows or not required_good.issubset(good_rows[0]):
    raise RuntimeError("Godtilstandskandidaten har ikke forventet struktur med 57 rader.")

key_counts = Counter(
    (row["ecosystem"], str(row["region_id"]), str(row["bioclimatic_zone"]))
    for row in good_rows
)
duplicates = [key for key, count in key_counts.items() if count > 1]
if duplicates:
    raise RuntimeError(f"Dupliserte godtilstandsnokler: {duplicates[:3]}")

candidate_rows: list[dict] = []
audit_rows: list[dict] = []
invalid_rows: list[dict] = []
for row in good_rows:
    region = str(row["region_id"])
    zone = str(row["bioclimatic_zone"])
    cell = (region, zone)
    if cell not in forest_by_cell:
        raise RuntimeError(f"Mangler skogverdi for region/sone {cell}")
    reference_height = float(row["reference_height_m"])
    forest_height = forest_by_cell[cell]
    denominator = forest_height - reference_height
    valid = math.isfinite(reference_height) and reference_height >= 0 and denominator > 0
    output_row = {
        "ecosystem": row["ecosystem"],
        "region_id": region,
        "bioclimatic_zone": zone,
        "forest_height_m": format(forest_height, ".15g"),
    }
    candidate_rows.append(output_row)
    audit_row = {
        **output_row,
        "reference_height_m": format(reference_height, ".15g"),
        "denominator_m": format(denominator, ".15g"),
        "valid_denominator": "TRUE" if valid else "FALSE",
    }
    audit_rows.append(audit_row)
    if not valid:
        invalid_rows.append(audit_row)

candidate_rows.sort(key=lambda r: (r["ecosystem"], int(r["region_id"]), int(r["bioclimatic_zone"])))
audit_rows.sort(key=lambda r: (r["ecosystem"], int(r["region_id"]), int(r["bioclimatic_zone"])))
write_csv(
    audit_path,
    ["ecosystem", "region_id", "bioclimatic_zone", "forest_height_m",
     "reference_height_m", "denominator_m", "valid_denominator"],
    audit_rows,
)

if invalid_rows:
    print("UGYLDIGE NEVNERE")
    for row in invalid_rows:
        print(row)
    raise RuntimeError(f"{len(invalid_rows)} av 57 strata har ugyldig nevner.")

forest_status = write_candidate_safely(
    forest_output,
    ["ecosystem", "region_id", "bioclimatic_zone", "forest_height_m"],
    candidate_rows,
)

config_lines = config_path.read_text(encoding="utf-8").splitlines()
hits = [i for i, line in enumerate(config_lines) if line.lstrip().startswith("forest_reference:")]
if len(hits) != 1:
    raise RuntimeError("Forventet noyaktig ett forest_reference-felt i kandidatkonfigurasjonen.")
target_line = "  forest_reference: data/x100_reference_v0_4/reference_forest_1m_v0_4.csv"
config_status = "identical_existing"
if config_lines[hits[0]] != target_line:
    backup = runtime_root / "config_x100_1m_v0_4_before_forest.yml"
    if not backup.exists():
        shutil.copyfile(config_path, backup)
    config_lines[hits[0]] = target_line
    config_path.write_text("\n".join(config_lines) + "\n", encoding="utf-8")
    config_status = "updated_candidate_only"

ecosystem_counts = Counter(row["ecosystem"] for row in candidate_rows)
minimum_denominator = min(float(row["denominator_m"]) for row in audit_rows)
summary_lines = [
    "X100 V0.4 SKOGREFERANSEKANDIDAT",
    "",
    f"Originale region-soneceller: {len(forest_by_cell)} / 25",
    f"Kandidatstrata: {len(candidate_rows)} / 57",
    f"Naturlig aapne: {ecosystem_counts['Naturlig aapne']}",
    f"Semi-naturlig: {ecosystem_counts['Semi-naturlig']}",
    f"Vaatmark: {ecosystem_counts['Vaatmark']}",
    "Manglende skogreferanser: 0",
    "Ugyldige nevnere: 0",
    f"Minste positive nevner: {minimum_denominator:.6f} m",
    f"Skogreferansetabell: {forest_status}",
    f"Kandidatkonfigurasjon: {config_status}",
    "",
    "Kildekodene climZone 1-5 beholdes som autoritative Moen KLASSE-koder.",
    "Eksisterende config.yml og data/reference_forest.csv er ikke endret.",
    "Kandidaten er ikke aktivert som standard.",
]
summary_path.write_text("\n".join(summary_lines) + "\n", encoding="utf-8")

provenance = {
    "created_utc": datetime.now(timezone.utc).isoformat(),
    "source_file": str(source_forest),
    "source_sha256": sha256(source_forest),
    "good_reference_file": str(good_path),
    "good_reference_sha256": sha256(good_path),
    "forest_reference_file": str(forest_output),
    "forest_reference_sha256": sha256(forest_output),
    "source_statistic": "p90_height",
    "source_zone_semantics": "Moen KLASSE 1-5",
    "candidate_active_as_default": False,
}
provenance_path.write_text(json.dumps(provenance, indent=2) + "\n", encoding="utf-8")

print("\n".join(summary_lines))
print(f"\nSkogreferansekandidat: {forest_output}")
print(f"Kandidatkonfigurasjon: {config_path}")
print(f"Revisjonsresultater: {runtime_root}")