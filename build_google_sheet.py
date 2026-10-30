# Книга «Пороги для гугл-таблиці»: 51 громада, 10 районів, область, у тому вигляді, в якому пороги переносяться
# в оперативну гугл-таблицю (назви як у колонці A таблиці, еталон, позначка правила А, чотири пороги, середній поріг
# у випадках на тиждень). Позначку правила А (так/ні) беремо з колонки export_rule_a переліку територій, а якщо її нема,
# з попередньої версії книги. Запуск: python build_google_sheet.py (після run_optimizer.R)
import os

import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "output", "Пороги_для_гугл_таблиці.xlsx")
T = pd.read_csv(os.path.join(HERE, "output", "memopt_thresholds.csv")).set_index("id")
terr = pd.read_csv(os.path.join(HERE, "input", "territories.csv"))
S = pd.read_excel(os.path.join(HERE, "output", "memopt_results.xlsx"), sheet_name="Малі території").set_index("Код")

prev_flags = {}
if os.path.exists(OUT):
    old = pd.read_excel(OUT)
    prev_flags = dict(zip(old.iloc[:, 0], old["Мала громада (правило А діє)"]))

label = {}
for _, r in terr.iterrows():
    label[r["id"]] = r["name"].upper() if r["level"] in ("район", "область") else r["name"]

rows = []
for _, h in terr[terr["kind"] == "small"].iterrows():
    t = T.loc[h["id"]]
    med_cases = t["medium"] * h["population"] / 1e5
    flag = h["export_rule_a"] if "export_rule_a" in terr.columns and pd.notna(h.get("export_rule_a")) else prev_flags.get(h["name"], "так" if med_cases < 20 else "ні")
    rows.append([h["name"], label[h["reference_id"]], flag, round(t["base"], 2), round(t["medium"], 2), round(t["high"], 2), round(t["veryhigh"], 2), round(med_cases, 1)])
for _, r in terr[(terr["kind"] == "main") & (terr["level"].isin(["район", "район без центру"]))].iterrows():
    t = T.loc[r["id"]]
    rows.append([label[r["id"]], None, "ні", round(t["base"], 2), round(t["medium"], 2), round(t["high"], 2), round(t["veryhigh"], 2), None])
t = T.loc["UA59"]
rows.append([label["UA59"], None, "ні", round(t["base"], 2), round(t["medium"], 2), round(t["high"], 2), round(t["veryhigh"], 2), None])
df = pd.DataFrame(rows, columns=["Територія (як у колонці A)", "Еталон (як у колонці A)", "Мала громада (правило А діє)", "Епідемічний поріг", "Середній поріг", "Високий поріг", "Дуже високий поріг", "Середній поріг, випадків на тиждень"])
df.to_excel(OUT, index=False)
print("saved", OUT, df.shape, "правило А діє:", (df["Мала громада (правило А діє)"] == "так").sum())
