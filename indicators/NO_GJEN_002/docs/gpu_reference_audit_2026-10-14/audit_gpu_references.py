from pathlib import Path
import hashlib,csv,sqlite3,re,json
pkg=Path('refs_v05/x100_gpu_reference_package_v0_5')
with (pkg/'SHA256SUMS.csv').open(encoding='utf-8-sig') as f:hashrows=list(csv.DictReader(f,delimiter=';'))
checked=[];missing=[];bad=[]
for r in hashrows:
 p=pkg/r['relative_path']
 if not p.exists():missing.append(r['relative_path']);continue
 h=hashlib.sha256(p.read_bytes()).hexdigest().upper();checked.append(r['relative_path'])
 if h!=r['sha256'].upper():bad.append(r['relative_path'])
lookup={};regions={'Nord-Norge':1,'Midt-Norge':2,'Østlandet':3,'Vestlandet':4,'Sørlandet':5};zones={'N':1,'BN':2,'SB':3,'MB':4,'NB':5}
for name,eco in [('refaapne_1m_gpu_v0_5.csv','Naturlig aapne'),('refsemi_1m_gpu_v0_5.csv','Semi-naturlig')]:
 with (pkg/name).open(encoding='utf-8-sig') as f:
  for r in csv.DictReader(f,delimiter=';'):
   key=(eco,regions[r['region']],zones[re.search(r'\(([^()]*)\)',r['vegClimZoneLab']).group(1)])
   assert key not in lookup;lookup[key]=float(r['ref'])
b=Path('aud_current_v2');c=sqlite3.connect(next((b/'runtime').rglob('delivery_polygon_results_gpu_v1_1.gpkg')));c.row_factory=sqlite3.Row
objects=[dict(r) for r in c.execute('select * from polygon_results where included=1')]
result=[]
for r in objects:
 x=lookup[(r['indicator_ecosystem'],int(r['region_id']),int(r['moen_zone_code']))]
 result.append(dict(delivery_id=r['delivery_id'],reference_group=r['indicator_ecosystem'],region=r['region_id'],zone=r['moen_zone_code'],delivery_X100=r['good_height_m'],package_X100=x,difference=x-r['good_height_m']))
assert max(abs(r['difference']) for r in result)<1e-12
with Path('reference_audit/X100_object_comparison.csv').open('w',encoding='utf-8-sig',newline='') as f:
 w=csv.DictWriter(f,fieldnames=result[0].keys(),delimiter=';');w.writeheader();w.writerows(result)
meta=json.loads((pkg/'x100_gpu_reference_package_metadata_v0_5.json').read_text())
summary={'included_objects':len(result),'X100_matches_within_1e_12':len(result),'max_abs_difference':max(abs(r['difference']) for r in result),'hashes_checked':len(checked),'hashes_mismatched':bad,'hash_files_missing':missing,'delivery_calibration':meta['calibration'],'distinct_terrestrial_reference_strata':len(lookup)}
Path('reference_audit/summary.json').write_text(json.dumps(summary,indent=2));print(json.dumps(summary,indent=2))
