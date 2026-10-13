from pathlib import Path
import sqlite3,csv,re,json,collections
BASE=Path('aud_current_v2')
a=sqlite3.connect(next((BASE/'original_ano/unpacked').rglob('*.gpkg')));a.row_factory=sqlite3.Row
f=sqlite3.connect(next((BASE/'runtime').rglob('delivery_frame_ano_aso_gruk_2024_v0_9.gpkg')));f.row_factory=sqlite3.Row
p=sqlite3.connect(next((BASE/'runtime').rglob('delivery_polygon_results_gpu_v1_1.gpkg')));p.row_factory=sqlite3.Row
pat=re.compile(r'(?<![A-Z0-9])T(?:2|8|11|12|13|15|16|18|21|24|29|31|32|33|34|40|41)(?=-|[^0-9]|$)')
def main(r,scale):
 text=' '.join(str(r[k] or '') for k in ['kartleggingsenhet_'+scale,'hovedtype_'+scale]).upper().strip();m=pat.search(text);return m.group() if m else None
src=[]
for r in a.execute('select * from ANO_SurveyPoint'):
 r=dict(r)
 if not r['ano_punkt_id'] or r['aar'] is None or r['aar']>2024 or str(r['utilgjengelig_punkt']).lower().strip() in ('ja','yes','true','1'):continue
 r['main250']=main(r,'250m2');r['main1']=main(r,'1m2');r['selected']=r['main250'] or r['main1'];src.append(r)
src.sort(key=lambda r:(r['aar'],str(r['registeringsdato'] or '')),reverse=True)
latest_all={};latest_eligible={}
for r in src:
 latest_all.setdefault(r['ano_punkt_id'],r)
 if r['selected']:latest_eligible.setdefault(r['ano_punkt_id'],r)
frame=[dict(r) for r in f.execute("select * from combined_delivery_frame where source_dataset='ANO'")]
objects={r['source_id']:dict(r) for r in p.execute("select * from polygon_results where source_dataset='ANO' and included=1")}
rows=[]
for r in frame:
 s=latest_eligible[r['source_id']];o=objects.get(r['source_id']);latest=latest_all[r['source_id']]
 rows.append(dict(source_id=r['source_id'],in_result=o is not None,frame_year=r['observation_year'],source_year=s['aar'],source_objectid=s['objectid'],source_date=s['registeringsdato'],code250=s['kartleggingsenhet_250m2'],code1=s['kartleggingsenhet_1m2'],main250=s['main250'],main1=s['main1'],scale_selected='250m2' if s['main250'] else '1m2_fallback',selected=s['selected'],frame_type=r['main_type_selected'],matches_frame=s['selected']==r['main_type_selected'] and s['aar']==r['observation_year'],wet250=str(s['kartleggingsenhet_250m2'] or '').startswith('V'),wet1=str(s['kartleggingsenhet_1m2'] or '').startswith('V'),reference_group=o['indicator_ecosystem'] if o else '',latest_all_year=latest['aar'],latest_all_type=latest['selected'],latest_all_same_record=latest['objectid']==s['objectid']))
def write(name,data):
 with (Path('scale_audit')/name).open('w',encoding='utf-8-sig',newline='') as f:
  w=csv.DictWriter(f,fieldnames=data[0].keys(),delimiter=';');w.writeheader();w.writerows(data)
write('ano_all_frame_scale_audit.csv',rows)
result=[r for r in rows if r['in_result']];mixed=[r for r in result if r['wet250'] or r['wet1']];write('ano_wetland_cross_scale_cases.csv',mixed)
summary={'frame_points':len(rows),'included_ano_objects':len(result),'selection_matches_frame':sum(r['matches_frame'] for r in rows),'selection_mismatches':[r['source_id'] for r in rows if not r['matches_frame']],'selected_scales_frame':dict(collections.Counter(r['scale_selected'] for r in rows)),'selected_scales_included':dict(collections.Counter(r['scale_selected'] for r in result)),'included_wet250':sum(r['wet250'] for r in result),'included_wet1':sum(r['wet1'] for r in result),'included_any_wet_source_code':len(mixed),'included_latest_all_differs':sum(not r['latest_all_same_record'] for r in result)}
Path('scale_audit/summary.json').write_text(json.dumps(summary,indent=2));print(json.dumps(summary,indent=2))
