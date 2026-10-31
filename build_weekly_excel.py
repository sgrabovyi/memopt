import os,sys
HERE=os.path.dirname(os.path.abspath(__file__))
os.chdir(HERE); sys.path.insert(0,HERE)
src=open("build_gate_report.py",encoding="utf-8").read().split("\n")
__file__=HERE+"/build_gate_report.py"
SUB=os.environ.get("MO_SUB","")
OUTF=os.environ.get("MO_OUT","output/показники_за_тижнями_підтвердження_районом.xlsx")
TXT="\n".join(src[:96]).replace('"output", "memopt_results.xlsx"','"output", SUB, "memopt_results.xlsx"').replace('"output", "reference_series_filled.csv"','"output", SUB, "reference_series_filled.csv"')
exec(TXT)
import numpy as np, pandas as pd
from openpyxl import Workbook
from openpyxl.styles import PatternFill, Font, Alignment
from openpyxl.formatting.rule import FormulaRule
from openpyxl.utils import get_column_letter as CL
RC=["Епідемічний поріг","Середній поріг","Високий поріг","Дуже високий поріг"]
NAME=terr.set_index("id")["name"]
sm=terr[terr.kind=="small"].copy()
sm["med_cases"]=[SM.loc[i,COLS[1]]*POP[i]/1e5 for i in sm.id]
sm["typ"]=[SM.loc[i,"Медіана випадків на тиждень"] for i in sm.id]
sm["small"]=(sm.med_cases<20)|sm.name.isin(["Кириківська селищна ОТГ","Грунська сільська ОТГ","Великописарівська селищна ОТГ"])
sm=sm.sort_values("typ",ascending=False)
refs=[r for d in ["UA5902","UA5904","UA5906","UA5908","UA5910"] for r in (d,d+"_nc")]
fills={2:"FFE699",3:"F4B183",4:"E06666"}
NW=len(WEEKS); W0=9; H0=W0+NW+2
NOTE={"2023/2024":" (перший сезон нинішнього періоду, початок припав на хвилю COVID-19; у висновках не враховується)"}
wb=Workbook(); wb.remove(wb.active)
for S in SEAS:
    ws=wb.create_sheet(S.replace("/","-"))
    ws["A1"]="Сезон "+S+NOTE.get(S,"")+". У клітинках захворюваність на 100 тис. за тиждень. Громади відсортовано за типовою кількістю випадків на тиждень, від найбільших до найменших."
    ws["A2"]="Зафарбовано рівень «середній» і вище: жовтий середній, помаранчевий високий, червоний дуже високий. Правило підтвердження районом діє лише для малих громад (середній поріг відповідає менше ніж 20 випадкам на тиждень): якщо район-еталон ще нижче свого епідемічного порогу, громада показується «низькою». Великі громади завжди за власними порогами."
    ws["A3"]="Праворуч розміщено допоміжну сітку рівнів для умовного форматування. Пороги районів-еталонів: "+("розраховані на трьох останніх сезонах (2023/24, 2024/25, 2025/26)." if SUB=="experiment_last3" else "набір сезонів для кожного району-еталона і міста-райцентру програма добирає окремо (колонка «Сезони в розрахунку» на аркуші «Пороги» книги результатів).")
    r0=5
    for bi,title in enumerate(("1. Власні пороги громад, без правила","2. З підтвердженням районом (лише для малих громад)","3. Райони-еталони: зафарбовано, коли район не нижче свого епідемічного порогу (умова правила підтвердження районом)","4. Міста-райцентри: власні пороги MEM, зафарбовано рівень «середній» і вище (без правила)")):
        ws.cell(r0,1,title).font=Font(bold=True,size=12)
        hdr=r0+1
        heads=["Територія","Еталон","Населення","Типово випадків на тиждень","Середній поріг на 100 тис.","Середній поріг, випадків на тиждень","Правило підтвердження районом","Епідемічний поріг на 100 тис."] 
        for c,t in enumerate(heads,1):
            ws.cell(hdr,c,t).font=Font(bold=True); ws.cell(hdr,c).alignment=Alignment(wrap_text=True,vertical="top")
        for j,wk in enumerate(WEEKS):
            ws.cell(hdr,W0+j,wk).font=Font(bold=True); ws.cell(hdr,W0+j).alignment=Alignment(horizontal="center")
        rr0=hdr+1; i=0
        if bi<2:
            for _,h in sm.iterrows():
                _,y,_,rr,t,rb,lv,_=prep(h.id,S)
                L=list(lv)
                if bi==1 and h["small"]:
                    L=[l if (l is None or l<2 or (rr.values[k]==rr.values[k] and rr.values[k]>=rb)) else 1 for k,l in enumerate(lv)]
                vals=[None,NAME[h.reference_id],int(h.population),round(h.typ,1),round(SM.loc[h.id,COLS[1]]),round(h.med_cases,1),"так" if h["small"] else "ні",round(SM.loc[h.id,COLS[0]])]
                vals[0]=h["name"]
                for c,v in enumerate(vals,1): ws.cell(rr0+i,c,v)
                for j,(v,l) in enumerate(zip(y.values,L)):
                    c=ws.cell(rr0+i,W0+j,None if v!=v else round(float(v),0)); c.alignment=Alignment(horizontal="center"); c.font=Font(size=9)
                    ws.cell(rr0+i,H0+j,l)
                i+=1
        elif bi==3:
            for rid in ["UA5902_city","UA5904_city","UA5906_city","UA5908_city","UA5910_city"]:
                rrw,rr=ref_series(rid,S); tt=[THR.loc[rid,c] for c in RC]
                vals=[NAME[rid],"","","",round(tt[1]),"","ні",round(tt[0])]
                for c,v in enumerate(vals,1): ws.cell(rr0+i,c,v)
                for j,v in enumerate(rr.values):
                    c=ws.cell(rr0+i,W0+j,None if v!=v else round(float(v),0)); c.alignment=Alignment(horizontal="center"); c.font=Font(size=9)
                    ws.cell(rr0+i,H0+j,None if v!=v else (4 if v>=tt[3] else 3 if v>=tt[2] else 2 if v>=tt[1] else 1 if v>=tt[0] else 0))
                i+=1
        else:
            for rid in refs:
                rrw,rr=ref_series(rid,S); b=THR.loc[rid,"Епідемічний поріг"]
                vals=[NAME[rid],"","","",round(THR.loc[rid,"Середній поріг"]),"","",round(b)]
                for c,v in enumerate(vals,1): ws.cell(rr0+i,c,v)
                for j,v in enumerate(rr.values):
                    c=ws.cell(rr0+i,W0+j,None if v!=v else round(float(v),0)); c.alignment=Alignment(horizontal="center"); c.font=Font(size=9)
                    ws.cell(rr0+i,H0+j,None if v!=v else (4 if v>=b else 0))
                i+=1
        last=rr0+i-1
        rng=f"{CL(W0)}{rr0}:{CL(W0+NW-1)}{last}"; hc=CL(H0)
        if bi<2 or bi==3:
            for v,colr in fills.items(): ws.conditional_formatting.add(rng,FormulaRule(formula=[f"{hc}{rr0}={v}"],fill=PatternFill(start_color=colr,end_color=colr,fill_type="solid")))
            ws.conditional_formatting.add(rng,FormulaRule(formula=[f"AND(ISNUMBER({hc}{rr0}),{hc}{rr0}<2)"],font=Font(color="999999")))
        else:
            ws.conditional_formatting.add(rng,FormulaRule(formula=[f"{hc}{rr0}=4"],fill=PatternFill(start_color="9DC3E6",end_color="9DC3E6",fill_type="solid")))
        r0=last+3
    widths=[38,30,11,12,11,13,11,11]
    for c,wd in enumerate(widths,1): ws.column_dimensions[CL(c)].width=wd
    for j in range(NW): ws.column_dimensions[CL(W0+j)].width=5.3; ws.column_dimensions[CL(H0+j)].hidden=True
    ws.freeze_panes=ws.cell(5,W0)
out=OUTF; wb.save(out); print(out)
print(sm[["name","typ","med_cases","small"]].head(3).to_string(), sm.small.sum())
# метрики без 2023/24 лише малі
res={}
for S in ["2024/2025","2025/2026"]:
    for _,h in sm[sm.small].iterrows():
        r,y,rr_raw,rr,t,rb,lv,_=prep(h.id,S); w=window(rr,rb)
        if w is None: continue
        ws_,we=w; U=np.array([l is not None and l>=2 for l in lv]); g=np.array([(v>=rb) if v==v else False for v in rr.values]); inw=np.array([ws_<=k<=we for k in range(len(U))])
        for k,v in (("без правила",U),("правило підтвердження районом",U&g)):
            d=res.setdefault((S,k),dict(out=0,inn=0)); d["out"]+=int((v&~inw).sum()); d["inn"]+=int((v&inw).sum())
print(pd.DataFrame(res).T)
