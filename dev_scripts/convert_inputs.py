import json
import os
import shutil
import numpy as np
import pandas as pd

M = "C:/Диск D/01 - Word/аналіз грві/мем (Грабовий С.Л.)/memopt/"
os.makedirs(M + "templates", exist_ok=True)
shutil.copy(M + "input/territories.csv", M + "input/territories_v02_backup.csv")

terr = pd.read_csv(M + "input/territories_v02_backup.csv", encoding="utf-8-sig")
long = pd.read_csv(M + "input/hromada_raw_long.csv")
weeks = list(range(40, 53)) + list(range(1, 21))

# 1. ряди громад у тому самому форматі, що й для інших територій: тижні в рядках, сезони в колонках
with pd.ExcelWriter(M + "input/hromadas.xlsx", engine="openpyxl") as xw:
    for hid in terr.loc[terr.level == "hromada", "id"]:
        L = long[long.id == hid]
        seas = sorted(L.season.unique())
        df = pd.DataFrame({"Week": weeks})
        for s in seas:
            sub = L[L.season == s].set_index("week")["rate"]
            df[s] = [sub.get(w, np.nan) for w in weeks]
        df.to_excel(xw, sheet_name=hid, index=False)

# 2. перелік територій: універсальні колонки
lvl = {"oblast": "область", "district": "район", "district_nocenter": "район без центру", "city": "місто", "hromada": "громада"}
rows = []
for _, r in terr.iterrows():
    small = r.level == "hromada"
    urban = r.settlement_type in ("міська", "селищна") if small else False
    ref = (r.parent_center if urban else r.parent_nocenter) if small else ""
    rows.append({
        "id": r.id, "name": r["name"], "level": lvl[r.level], "kind": "small" if small else "main",
        "file": "hromadas.xlsx" if small else r.file, "sheet": r.id if small else r.sheet,
        "population": int(r["pop"]) if small else "", "reference_id": ref,
        "type": r.settlement_type if small else "", "group": r.parent_center if small else (r.parent_center if isinstance(r.parent_center, str) else ""),
        "export_dim": "yes" if r.level in ("oblast", "district", "hromada") else "no"})
new = pd.DataFrame(rows)
new.to_csv(M + "input/territories.csv", index=False, encoding="utf-8-sig")
print(new.head(3).to_string()); print(new.tail(2).to_string())
os.remove(M + "input/hromada_raw_long.csv") if False else None

# 3. універсальний конфіг
old = json.load(open(M + "config_sumy.json", encoding="utf-8"))
cfg = {
    "project": "Сумська область: ГРВІ, пороги MEM на сезон 2026/27",
    "paths": {"series_dir": old["paths"]["datasets_dir"], "territories": "input/territories.csv", "output_dir": "output"},
    "season": {"start_week": 40, "end_week": 20, "holiday_weeks": [52, 1, 2], "max_seasons": 10},
    "placeholder_seasons": old["ignore_seasons"],
    "exclude_seasons": old["exclude_seasons"],
    "angle": old["angle"],
    "small_areas": {"angle": "auto", "method": "mixture", "groups": {"reliable_min": 14, "limited_min": 6}, "groups_window_seasons": None},
    "start_cleaning": {"enabled": True, "apply_to": "small", "mad_multiplier": 2, "compare_weeks": 8},
    "season_subsets": {"min_seasons": 5},
    "validation": {"hidden_season_check": True},
    "diagnostics": {"proportionality": True, "multiple_waves": True},
    "export": {"dashboard_dim_thresholds": True},
    "parallel": {"workers": 6},
}
json.dump(cfg, open(M + "config.json", "w", encoding="utf-8"), ensure_ascii=False, indent=2)
print(json.dumps(cfg, ensure_ascii=False)[:400])
