"""Recalculate the existing GRUK sample using the ANO/ASO coverage curve."""
import csv
import re
from datetime import date
from pathlib import Path
from statistics import mean, median

root = Path(__file__).resolve().parent
with (root / "kilde.csv").open(encoding="utf-8-sig", newline="") as f:
    source = list(csv.DictReader(f))
assert len(source) == len({r["ID"] for r in source}) == 266

def write(name, rows):
    with (root / name).open("w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)

rows = []
for r in source:
    share = float(r["Andel ≥0,8 m"])
    area = float(r["Analyseareal (m²)"])
    assert 0 <= share <= 1 and area > 0
    years = [date.fromisoformat(d).year for d in re.split(r"[;|]", r["Lokale bildedatoer"])]
    assert max(years) <= 2024
    score = 1 - 1.6*share if share <= .25 else .8*(1-share)
    ident = r["ID"]
    rows.append(dict(id=ident, program="GRUK", flate=ident, region="GRUK-utvalget",
                     score=score, dekning_ge_08=share, analyseareal_m2=area,
                     naturtypekode=r["Naturtypekode"], felttilstand=r["Felttilstand"],
                     bildedato=r["Lokale bildedatoer"], bilder_2020_2024=int(min(years)>=2020)))

def summary(label, group):
    a = sum(r["analyseareal_m2"] for r in group)
    return dict(utvalg=label, n=len(group), areal_ha=a/10000,
                snitt=mean(r["score"] for r in group),
                median=median(r["score"] for r in group),
                arealvektet=sum(r["score"]*r["analyseareal_m2"] for r in group)/a,
                dekning_arealvektet=sum(r["dekning_ge_08"]*r["analyseareal_m2"] for r in group)/a,
                score0=sum(r["score"]==0 for r in group),
                score1=sum(r["score"]==1 for r in group),
                dekning_over_25=sum(r["dekning_ge_08"]>.25 for r in group))
recent = [r for r in rows if r["bilder_2020_2024"]]
write("gruk_modellgrunnlag.csv", recent)
write("gruk_objektverdier.csv", rows)
write("gruk_referanse.csv", [summary("Tidligere undersøkt utvalg", rows),
                             summary("Bildeår 2020–2024", recent),
                             *[summary(s,[r for r in recent if r["felttilstand"]==s])
                               for s in sorted({r["felttilstand"] for r in recent})]])
