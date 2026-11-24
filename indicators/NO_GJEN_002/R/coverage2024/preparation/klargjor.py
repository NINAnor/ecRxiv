import csv
import math
import re
from collections import defaultdict
from io import StringIO
from pathlib import Path
from statistics import mean
from sys import argv
from zipfile import ZipFile

def read(z, name):
    return list(csv.DictReader(StringIO(z.read(name).decode("utf-8-sig")), delimiter=";"))

def save(name, rows):
    with (Path(__file__).parent / name).open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)

with ZipFile(argv[1]) as z:
    base = read(z, "objektverdier.csv")
with ZipFile(argv[2]) as z:
    edge = read(z, "ASO_kant_objekter.csv")
by_id = {r["id"]: r for r in edge}
assert len(by_id) == len(edge) == 339
assert len({r["id"] for r in base}) == len(base) == 879
rows = []
for r in base:
    if r["innenfor"] != "1" or not r["score"]:
        continue
    ident, program = r["id"], r["datasett"]
    score = float(r["score"])
    if program == "ASO":
        k = by_id.pop(ident)
        assert k["status"] == "OK" and k["region"] == r["region"] and k["type"] == r["type"]
        assert math.isclose(score, float(k["score_hele"]), abs_tol=1e-12)
        score = float(k["score_kjerne"])
        flate = ident
    else:
        assert program == "ANO"
        flate = re.fullmatch(r"ANO_(ANO\d+)_\d+", ident)[1]
    assert 0 <= score <= 1
    rows.append(dict(id=ident, program=program, flate=flate, region=r["region"],
                     naturtype=r["type"], score=score))
assert not by_id
assert sum(r["program"] == "ANO" for r in rows) == 299
save("modellgrunnlag.csv", rows)
result = []
for region in ["Samlet", "Nord-Norge", "Midt-Norge", "Østlandet", "Vestlandet", "Sørlandet"]:
    g = defaultdict(list)
    for r in rows:
        if region == "Samlet" or r["region"] == region:
            g[r["program"]].append(r["score"])
    result.append(dict(region=region, ANO=len(g["ANO"]), ASO=len(g["ASO"]),
                       score0=sum(x==0 for a in g.values() for x in a),
                       score1=sum(x==1 for a in g.values() for x in a),
                       objektvektet=mean(g["ANO"]+g["ASO"]),
                       lik_programvekt=mean(map(mean,g.values()))))
save("deskriptiv_referanse.csv",result)
