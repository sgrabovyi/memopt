import os
import numpy as np
import pandas as pd

ROOT = "C:/Диск D/01 - Word/аналіз грві/мем (Грабовий С.Л.)/memopt/"
B = "C:/Диск D/01 - Word/аналіз грві/мем (Грабовий С.Л.)/Аналіз порогів/"
E = "C:/Диск D/01 - Word/аналіз грві/Dashboard GRVI/GRVI_Dashboard_ETL/output/csv/"
os.makedirs(ROOT + "input", exist_ok=True)

q = pd.read_csv(B + "study3_hromada_data_quality.csv")
hr = pd.read_csv(E + "Dim_Hromadas.csv", encoding="utf-8-sig")
w = pd.read_csv(B + "dashboard_hromada_weekly_rates.csv", dtype={"Calend_Week": int})

districts = {  # id, назва, файл району, аркуш, файл без центру, аркуш, файл міста, аркуш
    "UA5902": ("Конотопський район", "Конотопський.xlsx", "Конотопський", "Сезони без міст.xlsx", "Без Конотопа", "Сезони по містах.xlsx", "Конотоп"),
    "UA5904": ("Охтирський район", "Охтирський.xlsx", "Охтирський", "Сезони без міст.xlsx", "Без Охтирки", "Сезони по містах.xlsx", "Охтирка"),
    "UA5906": ("Роменський район", "Роменський.xlsx", "Роменський", "Сезони без міст.xlsx", "Без Ромнів", "Сезони по містах.xlsx", "Ромни"),
    "UA5908": ("Сумський район", "Сумський.xlsx", "Сумський", "Сезони без міст.xlsx", "Без Сум", "Сезони по містах.xlsx", "Суми"),
    "UA5910": ("Шосткинський район", "Шосткінський.xlsx", "Шосткінський", "Сезони без міст.xlsx", "Без Шостки", "Сезони по містах.xlsx", "Шостка"),
}
raion_code = {"Конотопський": "UA5902", "Охтирський": "UA5904", "Роменський": "UA5906", "Сумський": "UA5908", "Шосткинський": "UA5910"}
rows = [dict(id="UA59", name="Сумська область", level="oblast", source="xlsx", file="Сумська область.xlsx", sheet="Сумська", parent_center="", parent_nocenter="", settlement_type="", pop="")]
for code, (nm, f1, s1, f2, s2, f3, s3) in districts.items():
    rows.append(dict(id=code, name=nm, level="district", source="xlsx", file=f1, sheet=s1, parent_center="UA59", parent_nocenter="", settlement_type="", pop=""))
    rows.append(dict(id=code + "_nc", name=nm.replace(" район", "") + " район без центру", level="district_nocenter", source="xlsx", file=f2, sheet=s2, parent_center="", parent_nocenter="", settlement_type="", pop=""))
    rows.append(dict(id=code + "_city", name="м. " + s3, level="city", source="xlsx", file=f3, sheet=s3, parent_center="", parent_nocenter="", settlement_type="", pop=""))
for r in q.itertuples():
    nm_ds = hr.Name_in_Dataset[hr.ADM3_PCODE == r.pcode].iloc[0]
    typ = "міська" if "міська" in nm_ds else ("селищна" if "селищна" in nm_ds else "сільська")
    rc = raion_code[r.raion]
    rows.append(dict(id=r.pcode, name=nm_ds, level="hromada", source="long", file="input/hromada_raw_long.csv", sheet=r.pcode,
                     parent_center=rc, parent_nocenter=rc + "_nc", settlement_type=typ, pop=int(r.pop)))
pd.DataFrame(rows).to_csv(ROOT + "input/territories.csv", index=False, encoding="utf-8-sig")

seas = {"2023/24": "2023/2024", "2024/25": "2024/2025", "2025/26": "2025/2026"}
w = w[w.Season_Display.isin(seas)].copy()
w["season"] = w.Season_Display.map(seas)
out = w.rename(columns={"ADM3_PCODE": "id", "Calend_Week": "week"})[["id", "season", "week", "rate"]]
out["rate"] = out.rate.round(4)
out.to_csv(ROOT + "input/hromada_raw_long.csv", index=False, encoding="utf-8")
print(len(rows), "територій;", len(out), "рядків довгого файлу")
