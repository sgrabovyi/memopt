# Таблиця порогів у Word: область, райони і громади, згруповані за районами, чотири пороги на 100 тис. за тиждень.
# Дані з output/memopt_thresholds.csv і input/territories.csv. Результат: docs/Пороги_<сезон>_таблиця.docx
# Запуск вручну, раз на сезон: python build_thresholds_table.py [дата для підзаголовка ДД.ММ.РРРР]
# Сезон визначається з розрахунку: наступний після останнього сезону, взятого в пороги.
import json
import os
from datetime import date

import pandas as pd
from docx import Document
from docx.enum.section import WD_ORIENT
from docx.enum.table import WD_TABLE_ALIGNMENT, WD_CELL_VERTICAL_ALIGNMENT
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Cm, Pt, RGBColor

HERE = os.path.dirname(os.path.abspath(__file__))
CFG = json.load(open(os.path.join(HERE, "config.json"), encoding="utf-8"))
THR = os.path.join(HERE, "output", "memopt_thresholds.csv")
T = pd.read_csv(THR).set_index("id")
last = max(s.strip() for v in T["seasons_used"].dropna() for s in str(v).split(","))
y = int(last[5:9]); SEASON = "%d/%d" % (y, y + 1)
OUT = os.path.join(HERE, "docs", "Пороги_%s_таблиця.docx" % SEASON.replace("/", "-"))
import sys
from thresholds_date import thresholds_date
RUN_DAY = sys.argv[1] if len(sys.argv) > 1 else thresholds_date()

terr = pd.read_csv(os.path.join(HERE, "input", "territories.csv"))
LEVELS = [("base", "Епідемічний"), ("medium", "Середній"), ("high", "Високий"), ("veryhigh", "Дуже високий")]
# кольори заголовків рівнів (ті самі відтінки, що в рисунках і дашборді); для жовтого темний текст
HEAD = [("17345E", "FFFFFF"), ("2E75B6", "FFFFFF"), ("E6B800", "1F1F1F"), ("E07B00", "FFFFFF"), ("C00000", "FFFFFF")]
FILL_OBL, FILL_DIST, FILL_ZEBRA = "C9D6EA", "E4EBF5", "F5F7FA"
FONT = "Calibri"


def shade(cell, hexcolor):
    tcPr = cell._tc.get_or_add_tcPr()
    sh = OxmlElement("w:shd"); sh.set(qn("w:val"), "clear"); sh.set(qn("w:color"), "auto"); sh.set(qn("w:fill"), hexcolor)
    tcPr.append(sh)


def borders(table, color="BFC7D5"):
    tblPr = table._tbl.tblPr
    b = OxmlElement("w:tblBorders")
    for edge in ("top", "left", "bottom", "right", "insideH", "insideV"):
        e = OxmlElement("w:" + edge); e.set(qn("w:val"), "single"); e.set(qn("w:sz"), "4"); e.set(qn("w:space"), "0"); e.set(qn("w:color"), color)
        b.append(e)
    tblPr.append(b)


def row_props(row, header=False):
    trPr = row._tr.get_or_add_trPr()
    cs = OxmlElement("w:cantSplit"); trPr.append(cs)
    if header:
        h = OxmlElement("w:tblHeader"); trPr.append(h)


def put(cell, text, bold=False, color="1F1F1F", align=WD_ALIGN_PARAGRAPH.LEFT, size=10, indent=0.0):
    cell.text = ""
    p = cell.paragraphs[0]; p.alignment = align
    p.paragraph_format.space_before = Pt(1.5); p.paragraph_format.space_after = Pt(1.5)
    if indent:
        p.paragraph_format.left_indent = Cm(indent)
    r = p.add_run(text); r.bold = bold; r.font.size = Pt(size); r.font.name = FONT
    r._element.rPr.rFonts.set(qn("w:eastAsia"), FONT)
    r.font.color.rgb = RGBColor.from_string(color)
    cell.vertical_alignment = WD_CELL_VERTICAL_ALIGNMENT.CENTER


def num(v):
    return ("%.1f" % v).replace(".", ",")


doc = Document()
sec = doc.sections[0]
sec.orientation = WD_ORIENT.PORTRAIT
sec.page_width, sec.page_height = Cm(21), Cm(29.7)
sec.left_margin = sec.right_margin = Cm(1.8); sec.top_margin = sec.bottom_margin = Cm(1.6)
st = doc.styles["Normal"]; st.font.name = FONT; st.font.size = Pt(10)
st.element.rPr.rFonts.set(qn("w:eastAsia"), FONT)

p = doc.add_paragraph(); r = p.add_run("Епідемічні пороги захворюваності на ГРВІ на сезон %s" % SEASON)
r.bold = True; r.font.size = Pt(16); r.font.color.rgb = RGBColor.from_string("17345E"); p.paragraph_format.space_after = Pt(2)
p = doc.add_paragraph(); r = p.add_run("Сумська область, райони і територіальні громади за районами. "
                                       "Випадків ГРВІ на 100 тис. населення за тиждень. Розрахунок методом рухомої епідемії (MEM), %s." % RUN_DAY)
r.font.size = Pt(10); r.font.color.rgb = RGBColor.from_string("555555"); p.paragraph_format.space_after = Pt(8)

tbl = doc.add_table(rows=1, cols=5)
tbl.alignment = WD_TABLE_ALIGNMENT.CENTER
tbl.autofit = False
borders(tbl)
widths = [Cm(7.6), Cm(2.4), Cm(2.4), Cm(2.4), Cm(2.6)]
hdr = tbl.rows[0]; row_props(hdr, header=True)
for i, (txt, (fill, fg)) in enumerate(zip(["Територія"] + [l for _, l in LEVELS], HEAD)):
    c = hdr.cells[i]; c.width = widths[i]; shade(c, fill)
    put(c, txt, bold=True, color=fg, align=WD_ALIGN_PARAGRAPH.LEFT if i == 0 else WD_ALIGN_PARAGRAPH.CENTER)


def add(name, tid, kind, zebra=False):
    r = tbl.add_row(); row_props(r)
    fill = FILL_OBL if kind == "obl" else FILL_DIST if kind == "dist" else (FILL_ZEBRA if zebra else None)
    bold = kind in ("obl", "dist")
    put(r.cells[0], name, bold=bold, indent=0.0 if bold else 0.4)
    for j, (k, _) in enumerate(LEVELS, 1):
        put(r.cells[j], num(T.loc[tid, k]), bold=bold, align=WD_ALIGN_PARAGRAPH.RIGHT)
    for j, c in enumerate(r.cells):
        c.width = widths[j]
        if fill:
            shade(c, fill)


obl = terr[terr["level"] == "область"].iloc[0]
add(obl["name"], obl["id"], "obl")
for _, d in terr[terr["level"] == "район"].iterrows():
    add(d["name"], d["id"], "dist")
    hs = terr[(terr["kind"] == "small") & (terr["group"] == d["id"])].sort_values("name")
    for i, (_, h) in enumerate(hs.iterrows()):
        add(h["name"], h["id"], "hrom", zebra=bool(i % 2))

p = doc.add_paragraph(); p.paragraph_format.space_before = Pt(8)
r = p.add_run("Рівень захворюваності тижня визначається найвищим порогом, якого вона досягла: нижче епідемічного порогу епідемії немає, "
              "від епідемічного до середнього низька інтенсивність, далі середня, висока і дуже висока. Пороги району розраховано для всього району разом із містом-центром.")
r.font.size = Pt(9); r.font.color.rgb = RGBColor.from_string("555555")
doc.save(OUT)
print("saved", OUT, "рядків:", len(tbl.rows) - 1)
