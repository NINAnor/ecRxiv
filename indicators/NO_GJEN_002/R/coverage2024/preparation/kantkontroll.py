"""Compare the original polygon mask with a one-cell inward erosion."""
import argparse
import hashlib
import json
from collections import defaultdict
from pathlib import Path

import numpy as np
from matplotlib.path import Path as PolygonPath

from beregn import ROOT, read, score, summary, write

def masks(parts, x, y, size=1024, step=.5):
    cols = x + (np.arange(-1, size+1) + .5)*step
    rows = y + size*step - (np.arange(-1, size+1) + .5)*step
    xx, yy = np.meshgrid(cols, rows)
    points = np.column_stack((xx.ravel(), yy.ravel()))
    full = np.zeros(len(points), dtype=bool)
    for polygon in parts:
        part = np.zeros_like(full)
        for ring in polygon:
            part ^= PolygonPath(ring).contains_points(points)
        full |= part
    full = full.reshape(size+2,size+2)
    interior = full[1:-1,1:-1].copy()
    for dy in range(3):
        for dx in range(3):
            interior &= full[dy:dy+size,dx:dx+size]
    return full[1:-1,1:-1], interior

def run(folder, expected):
    import rasterio
    from rasterio.transform import from_origin

    plan = json.loads((folder/"jobb"/"modellgrunnlag"/"plan.json").read_text(encoding="utf-8-sig"))
    assert (plan["crs"], plan["pixel_m"], plan["tile_m"], plan["pixels"]) == (25833,.5,512,1024)
    by_id = {p["id"]: p for p in plan["polygons"]}
    if not set(expected) <= set(by_id):
        raise ValueError(f"Feil modellgrunnlag i {folder}. Mangler {len(set(expected)-set(by_id))} polygoner.")
    tile_objects = defaultdict(list)
    for ident in expected:
        p = by_id[ident]
        for x,y in {tuple(t) for t in p["tiles"]}:
            tile_objects[p["project_id"],x,y].append(ident)
    counts = defaultdict(lambda: np.zeros(6,dtype=np.int64))
    failures = defaultdict(list)
    for (pid,x,y), ids in sorted(tile_objects.items()):
        name = f"p{pid}_x{x}_y{y}"
        path = folder/"hoyder"/"predictions"/f"{name}.tif"
        try:
            side = json.loads(path.with_suffix(".json").read_text())
            if hashlib.sha256(path.read_bytes()).hexdigest() != side["prediction_sha256"]:
                raise ValueError("SHA256 avviker")
            with rasterio.open(path) as ds:
                if (ds.count, ds.width, ds.height) != (1,1024,1024) or ds.crs.to_epsg()!=25833 or ds.transform!=from_origin(x,y+512,.5,.5):
                    raise ValueError("Rastergeometri avviker")
                data = ds.read(1,masked=True)
            valid = np.isfinite(data.data) & ~np.ma.getmaskarray(data)
            high = valid & (data.data >= .8)
            for ident in ids:
                full, inner = masks(plan["geometries"][ident],x,y)
                counts[ident] += [full.sum(),(full & valid).sum(),(full & high).sum(),
                                  inner.sum(),(inner & valid).sum(),(inner & high).sum()]
        except (OSError, ValueError, KeyError, rasterio.errors.RasterioError) as exc:
            for ident in ids:
                failures[ident].append(f"{name} {exc}")
    results = []
    for ident,row in expected.items():
        n,v,h,ni,vi,hi = map(int,counts[ident])
        p = float(row["dekning"])
        status = ("FLISFEIL" if failures[ident] else
                  "HELMASKE_AVVIK" if not n or v!=n or abs(100*h/n-p)>1e-8 else
                  "TOM_KJERNE" if not ni else
                  "KJERNE_NODATA" if vi!=ni else "OK")
        inner = 100*hi/ni if status=="OK" else None
        results.append(dict(id=ident,region=row["region"],type=row["type"],
                            bildesesong=row["bildesesong"],status=status,
                            piksler_hele=n,piksler_gyldige_hele=v,piksler_kjerne=ni,
                            piksler_gyldige_kjerne=vi,dekning_hele=p,dekning_kjerne=inner,
                            score_hele=score(p),score_kjerne=score(inner) if inner is not None else None,
                            flisfeil=" | ".join(failures[ident])))
    return results

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--sommer",type=Path,required=True)
    parser.add_argument("--september",type=Path,required=True)
    parser.add_argument("--ut",type=Path,required=True)
    a = parser.parse_args()
    a.ut.mkdir(parents=True,exist_ok=True)
    rows = read(ROOT/"objektverdier.csv")
    results = []
    for season,folder in (("sommer",a.sommer),("september",a.september)):
        expected = {r["id"]:r for r in rows if r["datasett"]=="ASO" and r["score"] and r["bildesesong"]==season}
        results.extend(run(folder,expected))
    write(a.ut/"ASO_kant_objekter.csv",results)
    by_id = {r["id"]:r for r in results if r["status"]=="OK"}
    frame = [r for r in rows if r["datasett"]=="ASO" and r["innenfor"]=="1"]
    groups = [("Alle","Alle",frame)]
    groups += [(reg,"Alle",[r for r in frame if r["region"]==reg]) for reg in sorted({r["region"] for r in frame})]
    groups += [("Alle",typ,[r for r in frame if r["type"]==typ]) for typ in sorted({r["type"] for r in frame})]
    tab = []
    for reg,typ,group in groups:
        for col in ("score_hele","score_kjerne"):
            # Hold the same paired set for both mask alternatives.
            paired = [dict(r,score=by_id[r["id"]][col] if r["id"] in by_id else None) for r in group]
            tab.append(dict(maske=col,**summary("ASO",reg,typ,paired)))
    write(a.ut/"ASO_kant_resultater.csv",tab)
    from zipfile import ZipFile, ZIP_DEFLATED
    report = a.ut/"ASO_KANTRAPPORT.zip"
    with ZipFile(report,"w",compression=ZIP_DEFLATED) as z:
        for name in ("ASO_kant_objekter.csv","ASO_kant_resultater.csv"):
            z.write(a.ut/name,name)
    print(f"{len(by_id)}/{len(results)} flater med sammenlignbar helmaske og kjerne.")
    print(f"SEND RAPPORT: {report}")

if __name__ == "__main__":
    main()
