# Книга для Google Таблиць «Пороги 2026-27 і підтвердження районом»: для кожного району аркуш, який через IMPORTRANGE
# тягне тижневі показники з книги «Тренди ГРВІ <район> DW», ставить поруч пороги і рахує рівень за порогами та
# рівень, що показується з підтвердженням районом (для малих громад «середній» і вище лише коли район-еталон не нижче
# свого епідемічного порогу). Результат: output/Пороги_2026-27_і_підтвердження_районом.xlsx, далі завантажується на Google Drive
# як Google Таблиця. Запуск: python build_google_rule_sheet.py
import os
import re

import pandas as pd
from openpyxl import Workbook
from openpyxl.formatting.rule import FormulaRule
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.utils import get_column_letter

HERE = os.path.dirname(os.path.abspath(__file__))
SLIM = os.environ.get("MO_SLIM") == "1"  # полегшена версія для завантаження на Google Drive: без пояснень, порогів, заливок
OUT = os.path.join(HERE, "output", "Пороги_2026-27_і_підтвердження_районом" + ("_slim" if SLIM else "") + ".xlsx")
T = pd.read_csv(os.path.join(HERE, "output", "memopt_thresholds.csv"))
G = pd.read_excel(os.path.join(HERE, "output", "Пороги_для_гугл_таблиці.xlsx"))
FLAG = dict(zip(G.iloc[:, 0], G["Потрібне підтвердження районом"]))
REF = dict(zip(G.iloc[:, 0], G["Еталон (як у колонці A)"]))

WEEKS = [f"w-26/{w}" for w in range(40, 53)] + [f"w-27/{w:02d}" for w in range(1, 20)]  # 32 тижні, як у книгах DW
NW = len(WEEKS)
LASTCOL = get_column_letter(1 + NW)  # AG

# Книги «Тренди ГРВІ … DW»: id, рядки аркуша «Тренди DW» (з 3-го), id району і району без центру в memopt
DW = {
    "Конотопський": dict(id="1Uy0jq-yHMI1xRAH2J84hrWtnijilfh4f9GJcdzHhuyk", d="UA5902", nc="UA5902_nc", rows=[
        "КОНОТОПСЬКИЙ РАЙОН", "Бочечківська сільська ОТГ", "Буринська міська ОТГ", "Дубов’язівська селищна ОТГ", "м. Конотоп",
        "Конотопська міська ОТГ без м. Конотоп", "Кролевецька міська ОТГ", "Новослобідська сільська ОТГ", "Попівська сільська ОТГ", "Путивльська міська ОТГ"]),
    "Охтирський": dict(id="1Y8a8NfWk8Z5BYXR3CJ7aM__Syr8xwzGVsTXSYH23ons", d="UA5904", nc="UA5904_nc", rows=[
        "ОХТИРСЬКИЙ РАЙОН", "Боромлянська сільська ОТГ", "Великописарівс. селищна ОТГ", "Грунська сільська ОТГ", "Кириківська селищна ОТГ",
        "Комишанська сільська ОТГ", "Охтирська міська ОТГ", "Тростянецька міська ОТГ", "Чернеччинська сільська ОТГ", "Чупахівська селищна ОТГ"]),
    "Роменський": dict(id="1MY8Y2i1G81cxb7r0RZihopPaIcmqHBmixI1hkKM6j4w", d="UA5906", nc="UA5906_nc", rows=[
        "РОМЕНСЬКИЙ РАЙОН", "Андріяшівська сільська ОТГ", "Вільшанська сільська ОТГ", "Коровинська сільська ОТГ", "Липоводолинська селищна ОТГ",
        "Недригайлівська селищна ОТГ", "м.Ромни", "Роменська міська ОТГ без м. Ромни", "Синівська сільська ОТГ", "Хмелівська сільська ОТГ"]),
    "Сумський": dict(id="1-EFIuKpXPl6gnHq6HrThSfvX_STp1ft_Zdr5uG6rLMM", d="UA5908", nc="UA5908_nc", rows=[
        "СУМСЬКИЙ РАЙОН", "Бездрицька сільська ТГ", "Білопільська міська ТГ", "Верхньосироватська сільська ТГ", "Ворожбянська міська ТГ",
        "Краснопільська селищна ТГ", "Лебединська міська ТГ", "Миколаївська селищна ТГ (Білопільський)", "Миколаївська сільська ТГ (Сумський)",
        "Миропільська сільська ТГ", "Нижньосироватська сільська ТГ", "Річківська сільська ТГ", "Садівська сільська ТГ", "Степанівська селищна ТГ",
        "м. Суми", "Сумська міська ТГ без м. Суми", "Хотінська селищна ТГ", "Юнаківська сільська ТГ"]),
    "Шосткинський": dict(id="1Tdq9KalKMkVnEHuNoJJODNpIOXSgev5Xh-npJorNsFc", d="UA5910", nc="UA5910_nc", rows=[
        "ШОСТКИНСЬКИЙ РАЙОН", "Березівська сільська ОТГ", "Глухівська міська ОТГ", "Хутір-Михайлівська міська ОТГ", "Есманьська селищна ОТГ",
        "Зноб-Новгородська селищна ОТГ", "Свеська селищна ОТГ", "Середино-Будська міська ОТГ", "Шалигинська селищна ОТГ", "м. Шостка",
        "Шосткинська міська ОТГ без м. Шостка", "Ямпільська селищна ОТГ"]),
}


def norm(s):
    s = str(s).lower().replace("’", "'").replace("`", "'")
    s = re.sub(r"\(.*?\)", "", s)
    s = s.replace("м.", "м. ").replace("великописарівс.", "великописарівська")
    return re.sub(r"[^а-яіїєґa-z0-9]", "", s)


TN = {norm(n): i for i, n in zip(T["id"], T["name"])}
TID = T.set_index("id")


def find_thr(dw_name):
    """Пороги для рядка книги DW; None для рядків «ОТГ без міста» (окремого порогу для них немає)."""
    if " без м." in dw_name:
        return None
    key = norm(dw_name)
    if key in TN:
        return TID.loc[TN[key]]
    # назви з уточненням у дужках: Миколаївські
    for k, i in TN.items():
        if key.startswith(k[:14]) and k[:14] == key[:14] and ("миколаївська" in key):
            nm = T.loc[T["id"] == i, "name"].iloc[0].lower()
            if ("селищна" in dw_name.lower()) == ("селищна" in nm):
                return TID.loc[i]
    raise KeyError(dw_name)


def level_formula(rr):
    v = f"B{rr}:{LASTCOL}{rr}"
    ab, am, ah, av = (f"${get_column_letter(2 + NW + 2 + k)}{rr}" for k in range(4))
    return f'=ARRAYFORMULA(IF({v}="","",IF({v}<{ab},"базовий",IF({v}<{am},"низький",IF({v}<{ah},"середній",IF({v}<{av},"високий","дуже високий"))))))'


LEVEL_FILL = {"базовий": "DDEBF7", "низький": "E2EFDA", "низький*": "FFF2CC", "середній": "FFE699", "високий": "F4B183", "дуже високий": "E06666"}

wb = Workbook()
ws0 = wb.active
ws0.title = "Як це працює"
if SLIM:
    wb.remove(ws0)
intro = [
    "Пороги MEM на сезон 2026/2027 і правило підтвердження районом для малих громад.",
    "",
    "1. На кожному аркуші району блок «Показник» тягне тижневі значення з вашої книги «Тренди ГРВІ <район> DW» через IMPORTRANGE.",
    "   При першому відкритті Google попросить дозволити доступ: натисніть на клітинку з #REF! і кнопку «Дозволити доступ».",
    "2. Рядок «<Район> без центру» треба заповнювати або зв'язати з вашою тижневою таблицею: це еталон для селищних і сільських громад,",
    "   а в книгах DW такого рядка немає. Поки він порожній, для цих громад у блоці «Показується» стоїть «немає даних еталона».",
    "3. Блок «Рівень за порогами території» порівнює показник із чотирма порогами (колонки AJ до AM): базовий, низький, середній, високий, дуже високий.",
    "4. Блок «Показується (з підтвердженням районом)»: для громад з позначкою «так» у колонці AN рівень «середній» і вище показується лише в тижні,",
    "   коли район-еталон (колонка AO) не нижче свого епідемічного порогу; інакше показується «низький*» (зірочка означає місцеве підвищення).",
    "   Для решти територій показується рівень за власними порогами без змін.",
    "5. Рядки «… ОТГ без м. …» окремого порогу не мають (пороги розраховано для громади разом із містом), тому рівень для них не виводиться.",
    "6. Усі пороги на аркуші «Пороги»; джерело: memopt, розрахунок 22.10.2026.",
    "7. Щоб мати це всередині книги DW: правою кнопкою на вкладці аркуша району, «Копіювати до», оберіть книгу DW; формули IMPORTRANGE працюватимуть і там.",
]
if not SLIM:
  for i, line in enumerate(intro, 1):
    ws0.cell(row=i, column=1, value=line)
  ws0.column_dimensions["A"].width = 150

wsP = wb.create_sheet("Пороги") if not SLIM else None
cols = ["Територія", "Вид", "Еталон", "Потрібне підтвердження районом", "Епідемічний поріг", "Середній поріг", "Високий поріг", "Дуже високий поріг"]
for j, c in enumerate(cols if not SLIM else [], 1):
    wsP.cell(row=1, column=j, value=c).font = Font(bold=True)
r = 2
for _, t in (T.iterrows() if not SLIM else []):
    wsP.append([t["name"], t["kind"], REF.get(t["name"], ""), FLAG.get(t["name"], "ні" if t["kind"] == "основна" else ""), round(t["base"], 2), round(t["medium"], 2), round(t["high"], 2), round(t["veryhigh"], 2)])
if not SLIM:
  wsP.column_dimensions["A"].width = 38
  wsP.column_dimensions["C"].width = 32
  for c in "DEFGH":
    wsP.column_dimensions[c].width = 16
  wsP.freeze_panes = "B2"

for dist, cfg in DW.items():
    ws = wb.create_sheet(dist)
    n = len(cfg["rows"])
    url = f"https://docs.google.com/spreadsheets/d/{cfg['id']}/"
    ws.cell(row=1, column=1, value=f"{dist} район: показники з книги «Тренди ГРВІ … DW», пороги на сезон 2026/2027 і підтвердження районом").font = Font(bold=True, size=12)
    hdr = 2
    ws.cell(row=hdr, column=1, value="Територія").font = Font(bold=True)
    for j, w in enumerate(WEEKS, 2):
        c = ws.cell(row=hdr, column=j, value=w)
        c.font = Font(bold=True)
        c.alignment = Alignment(horizontal="center")
    thr_cols = {"base": 2 + NW + 2, "medium": 2 + NW + 3, "high": 2 + NW + 4, "veryhigh": 2 + NW + 5}  # AI..AL
    col_flag = 2 + NW + 6  # AM
    col_ref = 2 + NW + 7   # AN
    for key, lab in zip(["base", "medium", "high", "veryhigh"], ["Епідемічний", "Середній", "Високий", "Дуже високий"]):
        ws.cell(row=hdr, column=thr_cols[key], value=lab).font = Font(bold=True)
    ws.cell(row=hdr, column=col_flag, value="Підтвердження районом").font = Font(bold=True)
    ws.cell(row=hdr, column=col_ref, value="Еталон").font = Font(bold=True)

    # Блок 1: показники
    b1 = hdr + 2
    ws.cell(row=b1 - 1, column=1, value="ПОКАЗНИК на 100 тисяч (автоматично з книги DW, аркуш «Тренди DW»)").font = Font(bold=True)
    ws.cell(row=b1, column=2, value=f'=IMPORTRANGE("{url}","\'Тренди DW\'!B3:{LASTCOL}{2 + n}")')
    info = []
    for i, name in enumerate(cfg["rows"]):
        rr = b1 + i
        ws.cell(row=rr, column=1, value=name)
        thr = find_thr(name)
        flag = FLAG.get(thr["name"], "ні") if thr is not None else ""
        refname = REF.get(thr["name"], "") if thr is not None else ""
        if i == 0:
            refname = ""
        if thr is not None:
            for key in thr_cols:
                ws.cell(row=rr, column=thr_cols[key], value=round(float(thr[key]), 2))
        ws.cell(row=rr, column=col_flag, value=flag if thr is not None else "")
        ws.cell(row=rr, column=col_ref, value=refname)
        info.append((rr, name, thr, flag, refname))
    # рядок району без центру
    r_nc = b1 + n
    tnc = TID.loc[cfg["nc"]]
    ws.cell(row=r_nc, column=1, value=f"{tnc['name']} (заповніть або зв'яжіть з вашою таблицею)").font = Font(italic=True)
    for key in thr_cols:
        ws.cell(row=r_nc, column=thr_cols[key], value=round(float(tnc[key]), 2))
    if not SLIM:
      for j in range(2, 2 + NW):
        ws.cell(row=r_nc, column=j).fill = PatternFill("solid", start_color="FFFBE6")
    r_d = b1  # рядок району з центром

    # Блок 2: рівень за порогами території
    b2 = r_nc + 3
    ws.cell(row=b2 - 1, column=1, value="РІВЕНЬ ЗА ПОРОГАМИ ТЕРИТОРІЇ (без правила)").font = Font(bold=True)
    for (rr, name, thr, flag, refname) in info:
        r2 = b2 + (rr - b1)
        ws.cell(row=r2, column=1, value=name)
        if thr is None:
            ws.cell(row=r2, column=2, value="окремого порогу немає (пороги для громади разом із містом)").font = Font(italic=True, color="808080")
            continue
        ws.cell(row=r2, column=2, value=level_formula(rr))
    r2_nc = b2 + n
    ws.cell(row=r2_nc, column=1, value=tnc["name"])
    ws.cell(row=r2_nc, column=2, value=level_formula(r_nc))

    # Блок 3: що показується з підтвердженням районом
    b3 = r2_nc + 3
    ws.cell(row=b3 - 1, column=1, value="ПОКАЗУЄТЬСЯ (підтвердження районом для малих громад: «середній» і вище лише коли еталон не нижче свого епідемічного порогу)").font = Font(bold=True)
    for (rr, name, thr, flag, refname) in info:
        r2 = b2 + (rr - b1)
        r3 = b3 + (rr - b1)
        ws.cell(row=r3, column=1, value=name)
        if thr is None:
            continue
        L = f"B{r2}:{LASTCOL}{r2}"
        if flag != "так":
            ws.cell(row=r3, column=2, value=f"=ARRAYFORMULA({L})")
            continue
        rref = r_nc if "без центру" in str(refname) else r_d
        ref_v, ref_b = f"B{rref}:{LASTCOL}{rref}", f"${get_column_letter(thr_cols['base'])}{rref}"
        ws.cell(row=r3, column=2, value=(f'=ARRAYFORMULA(IF({L}="","",IF({ref_v}="","немає даних еталона",IF({ref_v}>={ref_b},{L},'
                                          f'IF(({L}="середній")+({L}="високий")+({L}="дуже високий"),"низький*",{L})))))'))
    # умовне форматування рівнів у блоках 2 і 3
    rng = f"B{b2}:{LASTCOL}{b3 + n}"
    for lvl, color in (LEVEL_FILL.items() if not SLIM else []):
        ws.conditional_formatting.add(rng, FormulaRule(formula=[f'B{b2}="{lvl}"'], fill=PatternFill("solid", start_color=color, end_color=color)))
    ws.column_dimensions["A"].width = 44
    for j in range(2, 2 + NW):
        ws.column_dimensions[get_column_letter(j)].width = 9
    for key in thr_cols:
        ws.column_dimensions[get_column_letter(thr_cols[key])].width = 12
    ws.column_dimensions[get_column_letter(col_flag)].width = 10
    ws.column_dimensions[get_column_letter(col_ref)].width = 30
    ws.freeze_panes = "B3"

wb.save(OUT)
print("saved", OUT, "sheets:", wb.sheetnames)
