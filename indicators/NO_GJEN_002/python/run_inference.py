#!/usr/bin/env python3
"""Resumable CHM inference from the nationwide NIB JPEG archive.

Each 1000 px source tile is analysed as overlapping 256 px chips. Predictions
are feather-blended back to one georeferenced raster matching the source tile.
"""
from __future__ import annotations
import argparse, csv, hashlib, json, re, sys
from datetime import datetime, timezone
from pathlib import Path
import numpy as np
import rasterio
import torch
import torch.nn as nn
import torchvision.transforms as transforms
from rasterio.transform import from_origin
from tqdm import tqdm

def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--source-manifest", type=Path, required=True)
    p.add_argument("--aoi-map", type=Path, required=True)
    p.add_argument("--output", type=Path, required=True)
    p.add_argument("--model-repo", type=Path, required=True)
    p.add_argument("--checkpoint", type=Path, required=True)
    p.add_argument("--normalization-checkpoint", type=Path, required=True)
    p.add_argument("--manifest", type=Path, required=True)
    p.add_argument("--batch-size", type=int, default=16)
    p.add_argument("--workers", type=int, default=4)
    p.add_argument("--source-tile-size", type=int, default=1000)
    p.add_argument("--chip-size", type=int, default=256)
    p.add_argument("--chip-overlap", type=int, default=64)
    p.add_argument("--resolution", type=float, default=2.0)
    p.add_argument("--target-crs", type=int, default=25833)
    p.add_argument("--device", choices=("auto", "cpu", "cuda"), default="auto")
    p.add_argument("--input-low-quantile", type=float, default=0.05)
    p.add_argument("--input-high-quantile", type=float, default=0.95)
    p.add_argument("--output-scale", type=float, default=10.0)
    p.add_argument("--output-offset", type=float, default=0.0)
    p.add_argument("--model-revision", default="UNSPECIFIED")
    p.add_argument("--max-tiles", type=int, default=0,
                   help="Geographically spread pilot subset; 0 means all selected tiles")
    p.add_argument("--overwrite", action="store_true")
    return p.parse_args()

def choose_device(requested, compressed):
    if compressed: return torch.device("cpu")
    if requested == "cuda" and not torch.cuda.is_available():
        raise RuntimeError("CUDA was requested but is unavailable")
    if requested == "auto": requested = "cuda" if torch.cuda.is_available() else "cpu"
    return torch.device(requested)

def normalize_batch(images, model_norm, device, low_quantile, high_quantile):
    x=images.to(device)
    predicted=model_norm(x).detach()
    low_target,high_target=predicted[:,:3],predicted[:,3:]
    flattened=images.flatten(2)
    low_input=torch.quantile(flattened,low_quantile,dim=2).to(device)
    high_input=torch.quantile(flattened,high_quantile,dim=2).to(device)
    span=torch.clamp(high_input-low_input,min=torch.finfo(torch.float32).eps)
    return (x-low_input[:,:,None,None])*((high_target-low_target)/span)[:,:,None,None]+low_target[:,:,None,None]

def load_models(args, device):
    sys.path.insert(0, str(args.model_repo.resolve()))
    from models.backbone import SSLVisionTransformer
    from models.dpt_head import DPTHead
    from models.regressor import RNet
    class SSLAE(nn.Module):
        def __init__(self, huge):
            super().__init__()
            if huge:
                self.backbone=SSLVisionTransformer(embed_dim=1280,num_heads=20,out_indices=(9,16,22,29),depth=32,pretrained=None)
                self.decode_head=DPTHead(classify=True,in_channels=(1280,1280,1280,1280),embed_dims=1280,post_process_channels=[160,320,640,1280])
            else:
                self.backbone=SSLVisionTransformer(pretrained=None)
                self.decode_head=DPTHead(classify=True,n_bins=256)
        def forward(self,x): return self.decode_head(self.backbone(x))
    compressed="compressed" in args.checkpoint.name
    model=SSLAE("huge" in args.checkpoint.name).eval()
    if compressed:
        model=torch.quantization.quantize_dynamic(model,{nn.Linear,nn.Conv2d,nn.ConvTranspose2d},dtype=torch.qint8)
        state=torch.load(args.checkpoint,map_location="cpu")
    else:
        state=torch.load(args.checkpoint,map_location=device); state=state.get("state_dict",state)
    model.load_state_dict(state,strict=False); model.to(device).eval()
    state=torch.load(args.normalization_checkpoint,map_location=device)["state_dict"]
    state={k.replace("backbone.",""):v for k,v in state.items()}
    norm=RNet(n_classes=6).to(device).eval(); norm.load_state_dict(state)
    return model,norm

def read_rows(path):
    with path.open(newline="",encoding="utf-8-sig") as f: return list(csv.DictReader(f))
def truthy(x): return x.strip().lower() in {"true","t","1","yes"}

def sha256(path):
    digest=hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda:handle.read(4*1024*1024),b""): digest.update(chunk)
    return digest.hexdigest()

def write_provenance(args,device):
    data={
        "created_utc":datetime.now(timezone.utc).isoformat(),
        "model_revision":args.model_revision,
        "checkpoint":str(args.checkpoint.resolve()),
        "checkpoint_sha256":sha256(args.checkpoint),
        "normalization_checkpoint":str(args.normalization_checkpoint.resolve()),
        "normalization_checkpoint_sha256":sha256(args.normalization_checkpoint),
        "input_low_quantile":args.input_low_quantile,
        "input_high_quantile":args.input_high_quantile,
        "output_scale":args.output_scale,
        "output_offset_m":args.output_offset,
        "chip_size":args.chip_size,"chip_overlap":args.chip_overlap,
        "source_resolution_m":args.resolution,"source_tile_pixels":args.source_tile_size,
        "target_crs":args.target_crs,"device":str(device),
        "torch":torch.__version__,"numpy":np.__version__,"rasterio":rasterio.__version__
    }
    target=args.output/"inference_provenance.json"; temporary=target.with_suffix(".tmp")
    temporary.write_text(json.dumps(data,indent=2),encoding="utf-8"); temporary.replace(target)

def selected_sources(args):
    mapped={r["tile_id"] for r in read_rows(args.aoi_map)}
    rows=[r for r in read_rows(args.source_manifest) if r["tile_id"] in mapped and truthy(r.get("selected_for_inference","true"))]
    missing=[r["file"] for r in rows if not Path(r["file"]).is_file()]
    if missing: raise FileNotFoundError(f"{len(missing)} indexed source tiles are missing; first: {missing[0]}")
    rows.sort(key=lambda r: (float(r.get("ymin", 0)), float(r.get("xmin", 0))))
    if args.max_tiles > 0 and len(rows) > args.max_tiles:
        indices=np.linspace(0,len(rows)-1,args.max_tiles,dtype=int)
        rows=[rows[i] for i in sorted(set(indices.tolist()))]
    return rows

def positions(length,chip,overlap):
    stride=chip-overlap; result=list(range(0,length-chip+1,stride))
    if result[-1] != length-chip: result.append(length-chip)
    return result
def feather(chip,overlap):
    if overlap==0: return np.ones((chip,chip),dtype=np.float32)
    ramp=np.ones(chip,dtype=np.float32); edge=np.linspace(1e-3,1,overlap+1,dtype=np.float32)[:-1]
    ramp[:overlap]=edge; ramp[-overlap:]=edge[::-1]
    return np.outer(ramp,ramp)
def coordinates(row,path):
    try: return float(row["xmin"]),float(row["ymax"])
    except (KeyError,ValueError):
        m=re.search(r"_x(-?\d+)_y(-?\d+)$",path.stem)
        if not m: raise ValueError(f"Cannot derive coordinates from {path}")
        xmin,ymin=map(float,m.groups()); return xmin,ymin+2000

def valid_prediction(path,row,args):
    if not path.exists() or path.stat().st_size<=0: return False
    try:
        xmin,ymax=coordinates(row,Path(row["file"]))
        with rasterio.open(path) as src:
            return (src.count==1 and src.width==args.source_tile_size and
                    src.height==args.source_tile_size and src.crs is not None and
                    src.crs.to_epsg()==args.target_crs and
                    abs(src.transform.a-args.resolution)<1e-9 and
                    abs(src.transform.e+args.resolution)<1e-9 and
                    abs(src.bounds.left-xmin)<1e-6 and abs(src.bounds.top-ymax)<1e-6)
    except Exception:
        return False

def infer_tile(row,target,args,model,model_norm,device,normalizer):
    source=Path(row["file"])
    with rasterio.open(source) as src: raw=src.read([1,2,3])
    expected=(args.source_tile_size,args.source_tile_size)
    if raw.shape[1:]!=expected: raise ValueError(f"{source} is {raw.shape[2]}x{raw.shape[1]}; expected {expected[1]}x{expected[0]}")
    if not np.isfinite(raw).all() or np.all(raw==0): raise ValueError(f"{source} contains invalid or all-zero pixels")
    scale=np.iinfo(raw.dtype).max if np.issubdtype(raw.dtype,np.integer) else 1.
    image=torch.from_numpy(raw.astype(np.float32)/scale)
    ys=positions(args.source_tile_size,args.chip_size,args.chip_overlap); xs=positions(args.source_tile_size,args.chip_size,args.chip_overlap)
    windows=[(y,x) for y in ys for x in xs]; total=np.zeros(expected,np.float32); weights=np.zeros(expected,np.float32); weight=feather(args.chip_size,args.chip_overlap)
    with torch.inference_mode():
        for start in range(0,len(windows),args.batch_size):
            pos=windows[start:start+args.batch_size]
            chips=[image[:,y:y+args.chip_size,x:x+args.chip_size] for y,x in pos]
            normalized=normalizer(normalize_batch(torch.stack(chips),model_norm,device,args.input_low_quantile,args.input_high_quantile))
            predictions=(args.output_scale*model(normalized)+args.output_offset).detach().relu().cpu().numpy()
            for prediction,(y,x) in zip(predictions,pos):
                pred=np.squeeze(prediction).astype(np.float32)
                if pred.shape!=(args.chip_size,args.chip_size): raise ValueError(f"Model returned {pred.shape}")
                total[y:y+args.chip_size,x:x+args.chip_size]+=pred*weight; weights[y:y+args.chip_size,x:x+args.chip_size]+=weight
    output=total/np.maximum(weights,np.finfo(np.float32).eps); xmin,ymax=coordinates(row,source)
    profile={"driver":"GTiff","height":args.source_tile_size,"width":args.source_tile_size,"count":1,"dtype":"float32","crs":f"EPSG:{args.target_crs}","transform":from_origin(xmin,ymax,args.resolution,args.resolution),"compress":"deflate","predictor":3,"tiled":True,"blockxsize":256,"blockysize":256,"nodata":np.nan}
    target.parent.mkdir(parents=True,exist_ok=True); temporary=target.with_suffix(".tmp.tif")
    with rasterio.open(temporary,"w",**profile) as dst: dst.write(output,1)
    temporary.replace(target)

def write_manifest(rows,path):
    path.parent.mkdir(parents=True,exist_ok=True); temporary=path.with_suffix(path.suffix+".tmp")
    with temporary.open("w",newline="",encoding="utf-8") as f:
        w=csv.DictWriter(f,fieldnames=("tile_id","tile","prediction","status","message")); w.writeheader(); w.writerows(rows)
    temporary.replace(path)

def main():
    args=parse_args()
    for path in (args.source_manifest,args.aoi_map,args.model_repo,args.checkpoint,args.normalization_checkpoint):
        if not path.exists(): raise FileNotFoundError(path)
    if args.chip_size!=256 or not 0<=args.chip_overlap<args.chip_size: raise ValueError("chip-size must be 256 and overlap must be in [0,255]")
    if not 0<=args.input_low_quantile<args.input_high_quantile<=1: raise ValueError("input quantiles must satisfy 0 <= low < high <= 1")
    args.output.mkdir(parents=True,exist_ok=True); sources=selected_sources(args)
    completed={}
    for row in sources:
        target=args.output/f"{row['tile_id']}_pred.tif"
        if valid_prediction(target,row,args) and not args.overwrite:
            completed[row["tile_id"]]={"tile_id":row["tile_id"],"tile":row["file"],"prediction":str(target),"status":"existing","message":""}
    write_manifest(list(completed.values()),args.manifest)
    pending=[(r,args.output/f"{r['tile_id']}_pred.tif") for r in sources if r["tile_id"] not in completed]
    if not pending: print("All selected prediction rasters already exist; nothing to do"); return
    device=choose_device(args.device,"compressed" in args.checkpoint.name); write_provenance(args,device); model,model_norm=load_models(args,device)
    normalizer=transforms.Normalize((.420,.411,.296),(.213,.156,.143))
    for row,target in tqdm(pending,desc="Canopy-height tiles"):
        try:
            infer_tile(row,target,args,model,model_norm,device,normalizer)
            result={"tile_id":row["tile_id"],"tile":row["file"],"prediction":str(target),"status":"completed","message":""}
        except Exception as exc:
            result={"tile_id":row["tile_id"],"tile":row["file"],"prediction":str(target),"status":"failed","message":str(exc)}
        completed[row["tile_id"]]=result; write_manifest(list(completed.values()),args.manifest)
    failures=sum(r["status"]=="failed" for r in completed.values()); print(f"Processed {len(pending)} source tiles on {device}; failures: {failures}")
    if failures: raise RuntimeError(f"Inference failed for {failures} tiles; see {args.manifest}")
if __name__=="__main__": main()
