import csv
import json
import re
from collections import defaultdict
from pathlib import Path
from statistics import mean

ROOT = Path(__file__).resolve().parent
INPUT = ROOT / "grunnlag"
TYPES = "T31 T32 T33 T34 T40 T41 T2 T8 T11 T12 T13 T15 T16 T18 T21 T24".split()
REGIONS = ["Nord-Norge", "Midt-Norge", "Østlandet", "Vestlandet", "Sørlandet"]
COVER = "dekning_prosent_busker_og_traer_ge_08"

def read(path):
    with path.open(encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream, delimiter=";"))

def write(path, rows):
    with path.open("w", encoding="utf-8-sig", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), delimiter=";")
        writer.writeheader()
        writer.writerows(rows)

def index(rows, key="id"):
    result = {row[key]: row for row in rows}
    assert len(result) == len(rows)
    return result

def score(p):
    assert 0 <= p <= 100
    return 1 - .016*p if p <= 25 else .008*(100-p)

def summary(dataset, region, kind, rows):
    vals = [r["score"] for r in rows if r["score"] is not None]
    groups = defaultdict(list)
    for r in rows:
        groups[r["region"],r["type"]].append(r)
    weighted, empty = 0, 0
    for group in groups.values():
        observed = [r["score"] for r in group if r["score"] is not None]
        if observed:
            weighted += len(group) * mean(observed)
        else:
            empty += len(group)
    n = len(rows)
    return dict(datasett=dataset, region=region, type=kind, ramme=n,
                med_score=len(vals), andel_med_score=len(vals)/n if n else None,
                snitt_observert=mean(vals) if vals else None,
                uten_gruppesnitt=empty, rammevektet_nedre=weighted/n if n else None,
                rammevektet_ovre=(weighted+empty)/n if n else None)

def main():
    assert [score(p) for p in (0,25,75,100)] == [1,.6,.2,0]
    base = index(read(INPUT/"objektstatus_region.csv"))
    ano = index(json.loads((INPUT/"ANO_utvalg.json").read_text(encoding="utf-8-sig"))["selected"])
    aso = index(read(INPUT/"ASO_kildeattributter.csv"), "identifi_1")
    assert set(ano) == {k for k,r in base.items() if r["datasett"]=="ANO"}
    assert {"ASO_"+k for k in aso} == {k for k,r in base.items() if r["datasett"]=="ASO"}
    measurements = {}
    for dataset in ("ANO","ASO"):
        for season in ("sommer","september"):
            for ident,r in index(read(INPUT/f"{dataset}_{season}.csv")).items():
                b = base[ident]
                assert b["datasett"]==dataset and b["type"]==r["type"] and ident not in measurements
                assert bool(b["score"]) == bool(r["score_busker_og_traer_ge_08"] and season=="sommer")
                if r[COVER]:
                    assert float(r["andel_gyldig"])==1
                    measurements[ident] = (float(r[COVER]),season)
    rows = []
    for ident,b in base.items():
        ds = b["datasett"]
        if ds == "ANO":
            assert ano[ident]["source"]["Ntrtypk"] == b["type"]
            types = {b["type"]}
        else:
            src = aso[ident.removeprefix("ASO_")]
            assert src["naturtypeK"] == b["type"]
            types = set(re.findall(r"NA_([TV]\d+)-C-\d+",src["ninKartleg"]))
        inside = bool(types.intersection(TYPES))
        p,season = measurements.get(ident,(None,""))
        rows.append(dict(id=ident,datasett=ds,region=b["region"],type=b["type"],
                         nin=",".join(sorted(types)), innenfor=int(inside),
                         andre_hovedtyper=",".join(sorted(types-set(TYPES))),
                         dekning=p, score=score(p) if inside and p is not None else None,
                         bildesesong=season))
    write(ROOT/"objektverdier.csv",rows)
    result = []
    for ds in ("ANO","ASO"):
        eligible = [r for r in rows if r["datasett"]==ds and r["innenfor"]]
        for region in ["Alle",*REGIONS]:
            subset = [r for r in eligible if region=="Alle" or r["region"]==region]
            result.append(summary(ds,region,"Alle",subset))
        for kind in TYPES if ds=="ANO" else sorted({r["type"] for r in eligible}):
            result.append(summary(ds,"Alle",kind,[r for r in eligible if r["type"]==kind]))
    eligible_aso = [r for r in rows if r["datasett"]=="ASO" and r["innenfor"]]
    scenarios = {
        "Alle ASO med minst én avtalt hovedtype": eligible_aso,
        "Bare D02": [r for r in eligible_aso if r["type"].startswith("ntyp_D02")],
        "Bare avtalte hovedtyper i kodelista": [r for r in eligible_aso if not r["andre_hovedtyper"]],
        "Alle ASO uten hagemark": [r for r in eligible_aso if r["type"] != "ntyp_D02_02_01"],
    }
    write(ROOT/"ASO_utvalgsscenarioer.csv",
          [summary("ASO","Alle",label,group) for label,group in scenarios.items()])
    write(ROOT/"resultater.csv",result)
    for r in result:
        if r["type"]=="Alle":
            print(r)
    for r in rows:
        if (not r["innenfor"] or r["type"] in {"ntyp_A04","ntyp_D05","ntyp_E16"}):
            print(r)

if __name__=="__main__":
    main()
