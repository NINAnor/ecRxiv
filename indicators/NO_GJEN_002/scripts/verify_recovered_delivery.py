"""Verify archived object scores and national point estimate; no model inference."""
import csv, math
from pathlib import Path
root=Path(__file__).resolve().parents[1]
with (root/'data/delivery_v1_2_frozen/objects_recovered.csv').open(newline='',encoding='utf-8') as f:
    rows=list(csv.DictReader(f))
assert len(rows)==3759
assert len({r['delivery_id'] for r in rows})==3759
for r in rows:
    h,x100,x0=map(float,[r['calibrated_height_m'],r['X100_m'],r['X0_m']])
    assert x0>x100
    z=max(0,min(1,(h-x100)/(x0-x100)))
    value=max(0,min(1,1-1.0068*(1-math.exp(-5*z**2.5))))
    assert abs(round(value,4)-float(r['indicator_value']))<1e-9,r['delivery_id']
weight=sum(float(r['calibration_weight_national']) for r in rows)
value=sum(float(r['calibration_weight_national'])*float(r['indicator_value']) for r in rows)
assert abs(weight-1)<1e-10
assert round(value,4)==0.9170
print('3759 object scores verified; national estimate:',value)
print('Bootstrap interval not tested: complete ANO frame required.')
