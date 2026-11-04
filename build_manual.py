"""Побудова посібника користувача memopt в обох форматах (Markdown і Word) одним скриптом.

Вхід:  docs/manual_template.md (текст з позначками {{...}}), output/memopt_results.xlsx, output/examples.json, config.json,
       output/calibration/*.xlsx (результати запуску оптимізатора і калібрування 20-21.10.2026).
Вихід: docs/ПОСІБНИК_користувача.md і docs/ПОСІБНИК_користувача.docx. Числа в обох версіях беруться з однієї книги результатів.

Запуск після кожного запуску оптимізатора:
    python build_manual.py
Потрібні пакети: python-docx, pandas, openpyxl (pip install python-docx pandas openpyxl).
"""
import re
import sys
from docx import Document
from docx.enum.section import WD_ORIENT
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_BREAK
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Cm, Pt, RGBColor

NAVY = RGBColor(0x1F, 0x4E, 0x78)
GREY = RGBColor(0x59, 0x59, 0x59)
BODY_FONT = "Calibri"
CODE_FONT = "Consolas"


def set_font(run, name=None, size=None, bold=None, italic=None, color=None):
    if name:
        run.font.name = name
        rpr = run._element.get_or_add_rPr()
        rfonts = rpr.find(qn("w:rFonts"))
        if rfonts is None:
            rfonts = OxmlElement("w:rFonts")
            rpr.append(rfonts)
        for a in ("w:ascii", "w:hAnsi", "w:cs", "w:eastAsia"):
            rfonts.set(qn(a), name)
    if size:
        run.font.size = Pt(size)
    if bold is not None:
        run.font.bold = bold
    if italic is not None:
        run.font.italic = italic
    if color is not None:
        run.font.color.rgb = color


def shade(element_pr, fill):
    shd = OxmlElement("w:shd")
    shd.set(qn("w:val"), "clear")
    shd.set(qn("w:color"), "auto")
    shd.set(qn("w:fill"), fill)
    element_pr.append(shd)


def cell_shade(cell, fill):
    shade(cell._tc.get_or_add_tcPr(), fill)


def para_border_left(paragraph, color="1F4E78", sz=18):
    ppr = paragraph._p.get_or_add_pPr()
    pb = OxmlElement("w:pBdr")
    left = OxmlElement("w:left")
    left.set(qn("w:val"), "single")
    left.set(qn("w:sz"), str(sz))
    left.set(qn("w:space"), "8")
    left.set(qn("w:color"), color)
    pb.append(left)
    ppr.append(pb)


def para_border_bottom(paragraph, color="1F4E78", sz=8):
    ppr = paragraph._p.get_or_add_pPr()
    pb = OxmlElement("w:pBdr")
    b = OxmlElement("w:bottom")
    b.set(qn("w:val"), "single")
    b.set(qn("w:sz"), str(sz))
    b.set(qn("w:space"), "2")
    b.set(qn("w:color"), color)
    pb.append(b)
    ppr.append(pb)


def add_field(paragraph, instr, size=9, color=GREY):
    run = paragraph.add_run()
    set_font(run, BODY_FONT, size, color=color)
    f1 = OxmlElement("w:fldChar")
    f1.set(qn("w:fldCharType"), "begin")
    it = OxmlElement("w:instrText")
    it.set(qn("xml:space"), "preserve")
    it.text = instr
    f2 = OxmlElement("w:fldChar")
    f2.set(qn("w:fldCharType"), "separate")
    t = OxmlElement("w:t")
    t.text = "1"
    f3 = OxmlElement("w:fldChar")
    f3.set(qn("w:fldCharType"), "end")
    for e in (f1, it, f2, t, f3):
        run._r.append(e)


INLINE = re.compile(r"(\*\*[^*]+\*\*|`[^`]+`)")


def add_inline(paragraph, text, size=None, color=None, bold_all=False):
    for part in INLINE.split(text):
        if not part:
            continue
        if part.startswith("**") and part.endswith("**"):
            r = paragraph.add_run(part[2:-2])
            set_font(r, BODY_FONT, size, True, None, color)
        elif part.startswith("`") and part.endswith("`"):
            r = paragraph.add_run(part[1:-1])
            set_font(r, CODE_FONT, (size or 11) - 1, bold_all or None, None, RGBColor(0x9C, 0x2B, 0x2B))
        else:
            r = paragraph.add_run(part)
            set_font(r, BODY_FONT, size, True if bold_all else None, None, color)


def make_doc():
    doc = Document()
    sec = doc.sections[0]
    sec.page_width, sec.page_height = Cm(21.0), Cm(29.7)
    sec.left_margin = sec.right_margin = Cm(2.2)
    sec.top_margin, sec.bottom_margin = Cm(2.2), Cm(2.0)
    st = doc.styles["Normal"]
    st.font.name = BODY_FONT
    st.font.size = Pt(11)
    st.element.rPr.rFonts.set(qn("w:eastAsia"), BODY_FONT)
    st.paragraph_format.space_after = Pt(6)
    st.paragraph_format.line_spacing = 1.15
    for name, size, before, after in (("Heading 1", 18, 20, 8), ("Heading 2", 14, 14, 5), ("Heading 3", 12, 10, 4)):
        h = doc.styles[name]
        h.font.name = BODY_FONT
        h.font.size = Pt(size)
        h.font.bold = True
        h.font.color.rgb = NAVY
        h.element.rPr.rFonts.set(qn("w:eastAsia"), BODY_FONT)
        h.element.rPr.rFonts.set(qn("w:ascii"), BODY_FONT)
        h.element.rPr.rFonts.set(qn("w:hAnsi"), BODY_FONT)
        h.paragraph_format.space_before = Pt(before)
        h.paragraph_format.space_after = Pt(after)
        h.paragraph_format.keep_with_next = True
    return doc


def header_footer(doc, title):
    sec = doc.sections[0]
    sec.different_first_page_header_footer = True
    hp = sec.header.paragraphs[0]
    hp.alignment = WD_ALIGN_PARAGRAPH.RIGHT
    r = hp.add_run(title)
    set_font(r, BODY_FONT, 9, color=GREY)
    para_border_bottom(hp, "BFBFBF", 4)
    fp = sec.footer.paragraphs[0]
    fp.alignment = WD_ALIGN_PARAGRAPH.CENTER
    r = fp.add_run("Сторінка ")
    set_font(r, BODY_FONT, 9, color=GREY)
    add_field(fp, "PAGE")
    r = fp.add_run(" з ")
    set_font(r, BODY_FONT, 9, color=GREY)
    add_field(fp, "NUMPAGES")


def title_page(doc, title, subtitle, lines):
    for _ in range(6):
        doc.add_paragraph()
    p = doc.add_paragraph()
    r = p.add_run(title)
    set_font(r, BODY_FONT, 28, True, None, NAVY)
    p.paragraph_format.space_after = Pt(10)
    p2 = doc.add_paragraph()
    para_border_bottom(p2, "1F4E78", 18)
    p3 = doc.add_paragraph()
    r = p3.add_run(subtitle)
    set_font(r, BODY_FONT, 16, None, None, GREY)
    p3.paragraph_format.space_before = Pt(14)
    for _ in range(8):
        doc.add_paragraph()
    for ln in lines:
        pp = doc.add_paragraph()
        r = pp.add_run(ln)
        set_font(r, BODY_FONT, 11, None, None, GREY)
        pp.paragraph_format.space_after = Pt(2)
    doc.add_page_break()


def toc(doc, entries=None):
    p = doc.add_paragraph()
    r = p.add_run("Зміст")
    set_font(r, BODY_FONT, 18, True, None, NAVY)
    para_border_bottom(p, "1F4E78", 8)
    p = doc.add_paragraph()
    run = p.add_run()
    f1 = OxmlElement("w:fldChar")
    f1.set(qn("w:fldCharType"), "begin")
    it = OxmlElement("w:instrText")
    it.set(qn("xml:space"), "preserve")
    it.text = 'TOC \\o "1-2" \\h \\z \\u'
    f2 = OxmlElement("w:fldChar")
    f2.set(qn("w:fldCharType"), "separate")
    f3 = OxmlElement("w:fldChar")
    f3.set(qn("w:fldCharType"), "end")
    for e in (f1, it, f2):
        run._r.append(e)
    for lvl, txt in (entries or []):
        t = OxmlElement("w:t")
        t.set(qn("xml:space"), "preserve")
        t.text = ("      " if lvl == 2 else "") + txt
        run._r.append(t)
        run._r.append(OxmlElement("w:br"))
    run._r.append(f3)
    note = doc.add_paragraph()
    r = note.add_run("Номери сторінок з'являться після оновлення змісту: у Word натисніть правою кнопкою на змісті і виберіть «Оновити поле» (або погодьтесь на оновлення полів під час відкриття).")
    set_font(r, BODY_FONT, 9, None, True, GREY)
    doc.add_page_break()
    # попросити Word оновити поля при відкритті
    settings = doc.settings.element
    uf = OxmlElement("w:updateFields")
    uf.set(qn("w:val"), "true")
    settings.append(uf)


def set_cell_margins(table, top=60, bottom=60, left=90, right=90):
    tblPr = table._tbl.tblPr
    m = OxmlElement("w:tblCellMar")
    for k, v in (("top", top), ("left", left), ("bottom", bottom), ("right", right)):
        e = OxmlElement("w:" + k)
        e.set(qn("w:w"), str(v))
        e.set(qn("w:type"), "dxa")
        m.append(e)
    tblPr.append(m)


def table_borders(table, color="BFBFBF"):
    tblPr = table._tbl.tblPr
    b = OxmlElement("w:tblBorders")
    for k in ("top", "left", "bottom", "right", "insideH", "insideV"):
        e = OxmlElement("w:" + k)
        e.set(qn("w:val"), "single")
        e.set(qn("w:sz"), "4")
        e.set(qn("w:space"), "0")
        e.set(qn("w:color"), color)
        b.append(e)
    tblPr.append(b)


def add_table(doc, rows):
    header, body = rows[0], rows[1:]
    ncol = len(header)
    t = doc.add_table(rows=1 + len(body), cols=ncol)
    t.alignment = WD_TABLE_ALIGNMENT.CENTER
    t.autofit = False
    table_borders(t)
    set_cell_margins(t)
    total = 16.6
    lens = []
    for j in range(ncol):
        blen = max([len(re.sub(r"[`*]", "", r[j])) if j < len(r) else 0 for r in body] + [0])
        head = len(re.sub(r"[`*]", "", header[j]))
        L = max(blen, head * 0.45)
        lens.append(min(max(L, 9), 60) ** 0.85)
    s = sum(lens)
    widths = [Cm(total * x / s) for x in lens]
    lay = OxmlElement("w:tblLayout")
    lay.set(qn("w:type"), "fixed")
    t._tbl.tblPr.append(lay)
    for j in range(ncol):
        t.columns[j].width = widths[j]
    for i, row in enumerate([header] + body):
        tr = t.rows[i]
        trPr = tr._tr.get_or_add_trPr()
        cs = OxmlElement("w:cantSplit")
        trPr.append(cs)
        if i == 0:
            th = OxmlElement("w:tblHeader")
            trPr.append(th)
        for j in range(ncol):
            cell = tr.cells[j]
            cell.width = widths[j]
            txt = row[j] if j < len(row) else ""
            p = cell.paragraphs[0]
            p.paragraph_format.space_after = Pt(0)
            p.paragraph_format.line_spacing = 1.05
            if i == 0:
                cell_shade(cell, "1F4E78")
                add_inline(p, txt, 9.5, RGBColor(0xFF, 0xFF, 0xFF), True)
            else:
                if i % 2 == 0:
                    cell_shade(cell, "EEF3F8")
                add_inline(p, txt, 9.5)
    doc.add_paragraph().paragraph_format.space_after = Pt(2)


def add_code(doc, lines):
    t = doc.add_table(rows=1, cols=1)
    t.autofit = False
    table_borders(t, "D9D9D9")
    set_cell_margins(t, 90, 90, 140, 140)
    c = t.rows[0].cells[0]
    c.width = Cm(16.6)
    cell_shade(c, "F3F3F3")
    p = c.paragraphs[0]
    for k, ln in enumerate(lines):
        if k:
            p.add_run().add_break()
        r = p.add_run(ln if ln else " ")
        set_font(r, CODE_FONT, 9.5, None, None, RGBColor(0x1F, 0x1F, 0x1F))
    p.paragraph_format.space_after = Pt(0)
    doc.add_paragraph().paragraph_format.space_after = Pt(2)


def add_callout(doc, text, fill, border):
    t = doc.add_table(rows=1, cols=1)
    t.autofit = False
    set_cell_margins(t, 100, 100, 160, 140)
    c = t.rows[0].cells[0]
    c.width = Cm(16.6)
    cell_shade(c, fill)
    tcPr = c._tc.get_or_add_tcPr()
    b = OxmlElement("w:tcBorders")
    left = OxmlElement("w:left")
    left.set(qn("w:val"), "single")
    left.set(qn("w:sz"), "36")
    left.set(qn("w:color"), border)
    b.append(left)
    tcPr.append(b)
    p = c.paragraphs[0]
    p.paragraph_format.space_after = Pt(0)
    add_inline(p, text, 10.5)
    doc.add_paragraph().paragraph_format.space_after = Pt(2)


def convert(md_path, out_path, title, subtitle, lines, header_title):
    text = open(md_path, encoding="utf-8").read().replace("\r\n", "\n")
    src = text.split("\n")
    doc = make_doc()
    header_footer(doc, header_title)
    title_page(doc, title, subtitle, lines)
    entries = []
    infence = False
    for ln0 in src:
        if ln0.strip().startswith("```"):
            infence = not infence
        if infence:
            continue
        if ln0.startswith("## "):
            entries.append((1, ln0[3:].strip().replace("`", "")))
        elif ln0.startswith("### "):
            entries.append((2, ln0[4:].strip().replace("`", "")))
    toc(doc, entries)
    i = 0
    first_h1_skipped = False
    n = len(src)
    while i < n:
        ln = src[i]
        s = ln.strip()
        if s.startswith("```"):
            i += 1
            buf = []
            while i < n and not src[i].strip().startswith("```"):
                buf.append(src[i])
                i += 1
            i += 1
            add_code(doc, buf)
            continue
        if s == "" or s == "---":
            i += 1
            continue
        if s.startswith("# ") and not first_h1_skipped:
            first_h1_skipped = True
            i += 1
            continue
        if s.startswith("## "):
            doc.add_heading(s[3:].strip().replace("`", ""), level=1)
            i += 1
            continue
        if s.startswith("### "):
            doc.add_heading(s[4:].strip().replace("`", ""), level=2)
            i += 1
            continue
        if s.startswith("|"):
            rows = []
            while i < n and src[i].strip().startswith("|"):
                cells = [c.strip() for c in src[i].strip().strip("|").split("|")]
                if not all(re.fullmatch(r":?-{2,}:?", c) for c in cells):
                    rows.append(cells)
                i += 1
            add_table(doc, rows)
            continue
        m = re.match(r"^(\s*)([-*])\s+(.*)$", ln)
        if m:
            p = doc.add_paragraph()
            p.paragraph_format.left_indent = Cm(0.9)
            p.paragraph_format.first_line_indent = Cm(-0.5)
            p.paragraph_format.space_after = Pt(3)
            r = p.add_run("•  ")
            set_font(r, BODY_FONT, 11, True, None, NAVY)
            add_inline(p, m.group(3))
            i += 1
            continue
        m = re.match(r"^(\d+)\.\s+(.*)$", s)
        if m:
            p = doc.add_paragraph()
            p.paragraph_format.left_indent = Cm(0.9)
            p.paragraph_format.first_line_indent = Cm(-0.7)
            p.paragraph_format.space_after = Pt(3)
            r = p.add_run(m.group(1) + ".  ")
            set_font(r, BODY_FONT, 11, True, None, NAVY)
            add_inline(p, m.group(2))
            i += 1
            continue
        # абзац
        buf = [s]
        i += 1
        while i < n and src[i].strip() and not re.match(r"^(#|\||```|---|\s*[-*]\s|\d+\.\s)", src[i].strip() if not src[i].startswith(" ") else src[i]):
            buf.append(src[i].strip())
            i += 1
        para = " ".join(buf)
        p = doc.add_paragraph()
        p.alignment = WD_ALIGN_PARAGRAPH.LEFT
        add_inline(p, para)
    doc.core_properties.title = title
    doc.core_properties.author = "Сергій Грабовий"
    doc.core_properties.language = "uk-UA"
    doc.save(out_path)
    print("saved", out_path)




# ======================================================================================
# Заповнення шаблону числами з книги результатів і побудова обох версій посібника
# ======================================================================================
import os
import pandas as pd
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
TEMPLATE = os.path.join(HERE, "docs", "manual_template.md")
WORKBOOK = os.path.join(HERE, "output", "memopt_results.xlsx")
OUT_MD = os.path.join(HERE, "docs", "ПОСІБНИК_користувача.md")
OUT_DOCX = os.path.join(HERE, "docs", "ПОСІБНИК_користувача.docx")


def f1(v):
    return ("%.1f" % v).replace(".", ",")


def f0(v):
    return "%d" % round(v)


def md_table(df, cols, rename, fmt=None):
    d = df[cols].copy()
    head = [rename.get(c, c) for c in cols]
    lines = ["| " + " | ".join(head) + " |", "|" + "|".join(["---"] * len(cols)) + "|"]
    for _, r in d.iterrows():
        cells = []
        for c in cols:
            v = r[c]
            if isinstance(v, (float, np.floating)):
                cells.append("" if np.isnan(v) else (fmt or {}).get(c, f1)(v))
            else:
                cells.append(str(v))
        lines.append("| " + " | ".join(cells) + " |")
    return "\n".join(lines)



def c2(v):
    return ("%.2f" % v).replace(".", ",")


def plur(n, one, few, many):
    n = abs(int(n))
    if n % 10 == 1 and n % 100 != 11:
        return one
    if 2 <= n % 10 <= 4 and not 12 <= n % 100 <= 14:
        return few
    return many


def fmn(v, d=0):
    if v is None or (isinstance(v, float) and np.isnan(v)):
        return "н/д"
    return (("%." + str(d) + "f") % v).replace(".", ",")


def join_ua(items):
    """Перелік через кому зі сполучником «і» перед останнім: a, b і c."""
    items = [str(i) for i in items]
    if len(items) <= 1:
        return "".join(items)
    return ", ".join(items[:-1]) + " і " + items[-1]


def tbl(rows, head):
    lines = ["| " + " | ".join(head) + " |", "|" + "|".join(["---"] * len(head)) + "|"]
    for r in rows:
        lines.append("| " + " | ".join(str(x) for x in r) + " |")
    return "\n".join(lines)


def ang(v):
    """Кут нахилу так, як у тексті посібника: 2,0, 2,25, 2,5, 2,75."""
    s = ("%.2f" % v).replace(".", ",")
    return s[:-1] if s.endswith("0") else s


def _cmp_words(v, c, unit, same_word):
    """Порівняння значення сусіднього кута v з обраним c: «більше (1,6 проти 0,6)» або «стільки ж (0,6)»."""
    if abs(v - c) < 1e-9:
        return "%s (%s%s)" % (same_word, c2(v), unit)
    return "%s (%s проти %s%s)" % ("більше" if v > c else "менше", c2(v), c2(c), unit)


def angle_choice_text(o, floor):
    """Пояснення до таблиці вибору кута однієї території: які кути не допущені і чому, який обрано, як виглядають сусідні."""
    o = o.sort_values("Кут нахилу").reset_index(drop=True)
    FA, DL, LS = "Хибних сигналів на сезон", "Запізнення, тижнів", "Сумарна помилка (хибні сигнали + запізнення)"
    ok = o["У допустимому діапазоні"].astype(bool)
    reasons = {}
    for _, r in o[~ok].iterrows():
        if r["Кут нахилу"] < floor - 1e-9:
            why = "нижчий за межу стабільності для цієї території (%s, розділ 2.9)" % ang(floor)
        elif not (r["Епідемічний поріг"] < r["Середній поріг"]):
            why = "епідемічний поріг там не нижчий за середній"
        elif not (r["Коефіцієнт кореляції Метьюза"] > 0):
            why = "коефіцієнт кореляції Метьюза там не більший за 0"
        else:
            why = "не пройшов запобіжники"
        reasons.setdefault(why, []).append(ang(r["Кут нахилу"]))
    parts = []
    for why, angs in reasons.items():
        one = len(angs) == 1
        why = why if one else why.replace("нижчий за", "вони нижчі за")
        why = why if not one else why.replace("нижчий за", "він нижчий за")
        parts.append("%s %s не %s, бо %s." % ("Кут" if one else "Кути", join_ua(angs), "допускається" if one else "допускаються", why))
    ch = o[o["Обрано"].astype(bool)].iloc[0]
    parts.append("Кут %s дає суму %s + %s = %s, найменшу серед допустимих кутів, тому його обрано." % (ang(ch["Кут нахилу"]), c2(ch[FA]), c2(ch[DL]), c2(ch[LS])))
    allowed = o[ok]
    lower = allowed[allowed["Кут нахилу"] < ch["Кут нахилу"] - 1e-9].tail(1)
    upper = allowed[allowed["Кут нахилу"] > ch["Кут нахилу"] + 1e-9].head(1)
    for nb in (lower, upper):
        if not len(nb):
            continue
        r = nb.iloc[0]
        tail = ("сума та сама, і за правилом береться більший із двох кутів" if abs(r[LS] - ch[LS]) < 1e-9 else "сума %s" % c2(r[LS]))
        parts.append("При сусідньому куті %s хибних сигналів %s, а запізнення %s, тому %s." % (ang(r["Кут нахилу"]), _cmp_words(r[FA], ch[FA], "", "стільки ж"), _cmp_words(r[DL], ch[DL], " тижня", "таке саме"), tail))
    return " ".join(parts)


def stability_text(sc, floors):
    """Два приклади межі стабільності: територія, де крок до 2,0 допустимий, і територія, де падіння порогу зупинило спуск."""
    fl = floors.set_index("Назва")["Нижня межа за стабільністю"]

    def thr_at(name, ang):
        r = sc[(sc["Назва"] == name) & ((sc["Кут нахилу"] - ang).abs() < 1e-9)]
        return float(r["Епідемічний поріг"].iloc[0]) if len(r) else float("nan")

    parts = []
    low = [n for n, v in fl.items() if v <= 2.0 + 1e-9]
    if low:
        n1 = "Сумський район без центру" if "Сумський район без центру" in low else low[0]
        t225, t200 = thr_at(n1, 2.25), thr_at(n1, 2.0)
        parts.append("Приклад 1, територія %s. При куті 2,25 епідемічний поріг %s, а при куті 2,0 він %s: падіння на %s%%, у межах допустимого. Нижче 2,0 програма не йде, бо це нижня межа діапазону, рекомендованого статтею Vega, тому межа стабільності для цієї території дорівнює 2,0."
                     % (n1, fmn(t225, 0), fmn(t200, 0), fmn((1 - t200 / t225) * 100, 0)))
    high = [n for n, v in fl.items() if v > 2.0 + 1e-9]
    if high:
        n2 = "Сумська область" if "Сумська область" in high else high[0]
        f2 = float(fl[n2])
        t_f, t_b = thr_at(n2, f2), thr_at(n2, f2 - 0.25)
        if t_b != t_b:
            parts.append("Приклад 2, територія %s. При куті %s епідемічний поріг %s, а при куті %s він не будується (доепідемічного періоду майже не лишається), тому межа стабільності дорівнює %s, і менші кути для цієї території не розглядаються."
                         % (n2, ang(f2), fmn(t_f, 0), ang(f2 - 0.25), ang(f2)))
        else:
            parts.append("Приклад 2, територія %s. При куті %s епідемічний поріг %s, а при куті %s він падає до %s, тобто на %s%%, більше за допустимі 20%%, тому межа стабільності дорівнює %s, і менші кути для цієї території не розглядаються."
                         % (n2, ang(f2), fmn(t_f, 0), ang(f2 - 0.25), fmn(t_b, 0), fmn((1 - t_b / t_f) * 100, 0), ang(f2)))
    return " ".join(parts)


def fill(template, wb_path):
    import json
    ex = json.load(open(os.path.join(HERE, "output", "examples.json"), encoding="utf-8"))
    x = pd.read_excel(wb_path, sheet_name=None)
    reps = {}
    # --- крива MAP і кут
    mp = ex["angle_a"]["map"]
    reps["T_MAP"] = tbl([[m["duration"], fmn(m["share"], 1), fmn(m["slope"], 2)] for m in mp[:14]],
                        ["Тривалість групи, тижнів", "Відсоток випадків сезону в групі", "Приріст при додаванні цього тижня, відсоткових пунктів"])
    aa, bb = ex["angle_a"]["angles"], ex["angle_b"]["angles"]
    reps["T_ANGLE_EFFECT"] = tbl([[c2(a["angle"]), a["duration"], "%s-%s" % (a["start_week"], a["end_week"]), fmn(a["share"], 1), b["duration"], "%s-%s" % (b["start_week"], b["end_week"])]
                                  for a, b in zip(aa, bb)],
                                 ["Кут нахилу", "2024/2025: тижнів", "2024/2025: тижні", "2024/2025: % випадків", "2025/2026: тижнів", "2025/2026: тижні"])
    # --- пороги
    t = ex["thr"]
    reps["EX_ANGLE"] = fmn(t["angle"], 2).rstrip("0").rstrip(",") if "," in fmn(t["angle"], 2) else fmn(t["angle"], 2)
    tr28 = [r for r in ex["hidden"]["truth"] if abs(r["truth_angle"] - 2.8) < 1e-9][0]
    reps.update({"HID_EPI_FROM": str(tr28["start_week"]), "HID_EPI_TO": str(tr28["end_week"]), "HID_EPI_LEN": str(int(tr28["end"] - tr28["start"] + 1))})
    reps.update({"THR_PRE_N": str(t["pre_n"]), "THR_PRE_MEAN": fmn(t["pre_mean"], 1), "THR_PRE_SD": fmn(t["pre_sd"], 1), "THR_BASE": fmn(t["base"], 0),
                 "THR_EPI_N": str(t["epi_n"]), "THR_EPI_GM": fmn(t["epi_geomean"], 0), "THR_MEDIUM": fmn(t["medium"], 0), "THR_HIGH": fmn(t["high"], 0),
                 "THR_VERYHIGH": fmn(t["veryhigh"], 0), "THR_BELOW_1": str(t["epi_below"][0]), "THR_BELOW_2": str(t["epi_below"][1]), "THR_BELOW_3": str(t["epi_below"][2]),
                 "THR_PER_SEASON": str(t["n_values_per_season"])})
    for i in range(3):
        reps["THR_BELOW_PCT_%d" % (i + 1)] = fmn(100.0 * t["epi_below"][i] / t["epi_n"], 0)
    # --- відкладений сезон
    h = ex["hidden"]
    rows = []
    for w, v, pr, ab in zip(h["weeks"], h["values"], h["period"], h["above"]):
        if v is None:
            continue
        rows.append([w, fmn(v, 1), pr, "так" if ab else "ні"])
    reps["T_HIDDEN"] = tbl(rows, ["Тиждень", "Показник", "Період", "Вище порогу"])
    TP, FN, FP, TN = h["confusion"]
    sens, spec, ppv, npv = TP / (TP + FN), TN / (TN + FP), TP / (TP + FP), TN / (TN + FN)
    acc = (TP + TN) / (TP + TN + FP + FN)
    mcc = (TP * TN - FP * FN) / np.sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
    reps.update({"HID_T": fmn(h["threshold"], 1), "HID_TP": str(TP), "HID_FN": str(FN), "HID_FP": str(FP), "HID_TN": str(TN), "HID_SENS": c2(sens), "HID_SPEC": c2(spec),
                 "HID_PPV": c2(ppv), "HID_NPV": c2(npv), "HID_ACC": c2(acc), "HID_MCC": c2(mcc), "HID_YOUDEN": c2(sens + spec - 1),
                 "HID_NWEEKS": str(len(rows)), "HID_NEPI": str(TP + FN), "HID_NNON": str(FP + TN), "HID_BELOW_N": str(FN + TN)})
    fn_weeks = [w for w, v, pr, ab in zip(h["weeks"], h["values"], h["period"], h["above"]) if v is not None and pr == "епідемічний" and not ab]
    reps["HID_FN_WEEKS"] = join_ua(w + "-й" for w in fn_weeks) + (" тижні" if len(fn_weeks) > 1 else " тиждень")
    reps["HID_FP_TEXT"] = "Жодного хибного сигналу." if FP == 0 else "Хибних сигналів %d." % FP
    reps["HID_PPV_TEXT"] = "Усі тижні вище порогу були справді епідемічними." if FP == 0 else "З %d тижнів вище порогу %d були справді епідемічними." % (TP + FP, TP)
    reps["HID_MCC_QUAL"] = "Добра якість." if mcc >= 0.5 else ("Помірна якість." if mcc >= 0.3 else "Слабка якість.")
    hv = [(w, v, ab) for w, v, pr, ab in zip(h["weeks"], h["values"], h["period"], h["above"]) if v is not None and pr == "епідемічний"]
    if hv:
        first = next((i for i, x in enumerate(hv) if x[2]), None)
        st_w = hv[0][0]
        if first is None:
            reps["HID_DELAY_TEXT"] = "Епідемічний період почався на %s-му тижні, але показник так і не перевищив поріг: епідемію пропущено." % st_w
        elif first == 0:
            reps["HID_DELAY_TEXT"] = "Епідемічний період почався на %s-му тижні, і показник того самого тижня (%s) уже був вищим за поріг, тобто запізнення немає." % (st_w, fmn(hv[0][1], 1))
        else:
            below = "; ".join("на %s-му тижні %s" % (w, fmn(v, 1)) for w, v, _ in hv[:first])
            reps["HID_DELAY_TEXT"] = ("Епідемічний період почався на %s-му тижні. Показник %s (%s порогу), на %s-му тижні %s (вище порогу). Перше перевищення сталося на %s-му тижні, тобто запізнення дорівнює %d %s."
                                      % (st_w, below, "нижче" if first > 1 else "нижче", hv[first][0], fmn(hv[first][1], 1), hv[first][0], first, plur(first, "тиждень", "тижні", "тижнів")))
        reps["HID_DELAY_TEXT"] += " " + reps["HID_FP_TEXT"]
    dls = [int(r["delay"]) for r in h["truth"]]
    reps["TRUTH_DMAX"] = "%d %s" % (max(dls), plur(max(dls), "тижня", "тижнів", "тижнів"))
    reps["TRUTH_DMIN"] = "%d" % min(dls)
    reps["T_TRUTH"] = tbl([[c2(r["truth_angle"]), "%s-%s" % (r["start_week"], r["end_week"]), r["fa"], r["delay"]] for r in h["truth"]],
                          ["Кут для визначення епідемічного періоду", "Епідемічний період, тижні", "Хибних сигналів", "Запізнення, тижнів"])
    # --- хибні сигнали (м. Охтирка)
    f = ex["fa_example"]
    rows = []
    for idx, (w, v) in enumerate(zip(f["weeks"], f["values"]), 1):
        if v is None or not (idx <= 12 or 16 <= idx <= 24):
            continue
        per = "до початку епідемічного періоду" if idx < f["start"] else "епідемічний період"
        rows.append([w, fmn(v, 1), per, "так" if v > f["threshold"] else "ні"])
    reps["T_FA"] = tbl(rows, ["Тиждень", "Показник", "Період", "Вище порогу %s" % fmn(f["threshold"], 0)])
    fa_w = [w for idx, (w, v) in enumerate(zip(f["weeks"], f["values"]), 1) if v is not None and idx < f["start"] and v > f["threshold"]]
    first_in = next((w for idx, (w, v) in enumerate(zip(f["weeks"], f["values"]), 1) if v is not None and idx >= f["start"] and v > f["threshold"]), None)
    reps["FA_NAME"] = f["name"]
    reps["FA_SEASON"] = f["season"]
    reps["FA_T"] = fmn(f["threshold"], 0)
    reps["FA_NTRAIN"] = str(f["n_train"])
    reps["FA_ANGLE"] = c2(f.get("angle", t["angle"]))
    reps["FA_TEXT"] = ("На %s показник перевищив поріг, коли епідемічний період ще не почався (він почався на %s-му тижні). Це %d %s. "
                       % (", ".join(w + "-му" for w in fa_w) + " тижн" + ("ях" if len(fa_w) > 1 else "і"), f["start_week"], len(fa_w), plur(len(fa_w), "хибний сигнал", "хибні сигнали", "хибних сигналів")) if fa_w else
                       "До початку епідемічного періоду (він почався на %s-му тижні) показник порогу не перевищував, хибних сигналів не було. " % f["start_week"])
    reps["FA_TEXT"] += ("На %s-му тижні показник вперше перевищив поріг після початку епідемічного періоду: запізнення %d %s." % (first_in, int(f["delay"]), plur(int(f["delay"]), "тиждень", "тижні", "тижнів"))
                        if first_in else "Після початку епідемічного періоду поріг не був перевищений: епідемію пропущено.")
    reps["N_SEASONS"] = str(ex["n_seasons"])
    reps["SEASONS_LIST"] = join_ua(ex["seasons"])
    reps["HID_NTRAIN"] = str(ex["n_seasons"] - 1)
    mt = x["Основні території"].set_index("Назва")
    reps["OBL_ANGLE"] = c2(float(mt.loc["Сумська область", "Кут нахилу"]))
    # --- виключені сезони з config.json (лише читання)
    cfg = json.load(open(os.path.join(HERE, "config.json"), encoding="utf-8"))
    excl = cfg.get("exclude_seasons", {}) or {}
    grp = {}
    for s, r in excl.items():
        grp.setdefault(r, []).append(s)
    reps["EXCL_TEXT"] = "; ".join("%s (%s)" % (join_ua(v), k) for k, v in grp.items())
    # --- вибір кута: м. Охтирка
    sc = x["Кут нахилу"]
    o = sc[sc["Назва"] == "м. Охтирка"]
    lab = {c: c for c in sc.columns}
    rows = []
    for _, r in o.iterrows():
        rows.append([c2(r["Кут нахилу"]), fmn(r["Епідемічний поріг"], 0), c2(r["Хибних сигналів на сезон"]), c2(r["Запізнення, тижнів"]), fmn(r["Пропущено, %"], 1),
                     c2(r["Сумарна помилка (хибні сигнали + запізнення)"]), "так" if r["У допустимому діапазоні"] else "ні", "ОБРАНО" if r["Обрано"] else ""])
    reps["T_OHT"] = tbl(rows, ["Кут нахилу", "Епідемічний поріг", "Хибних сигналів на сезон", "Запізнення, тижнів", "Пропуск, %", "Сумарна помилка", "Допустимо", "Обрано"])
    reps["OHT_TEXT"] = angle_choice_text(o, float(x["Межа кута"].set_index("Назва").loc["м. Охтирка", "Нижня межа за стабільністю"]))
    reps["STAB_TEXT"] = stability_text(sc, x["Межа кута"])
    # --- шаблон ряду
    tp = pd.read_excel(os.path.join(HERE, "templates", "series_template.xlsx"))
    rows = []
    for _, r in tp.iloc[[0, 1, 12, 13, 14, 15]].iterrows():
        rows.append([int(r["Week"])] + ["" if pd.isna(r[c]) else fmn(r[c], 1) for c in tp.columns[1:]])
    rows.insert(2, ["...", "...", "...", "..."])
    reps["T_TEMPLATE"] = tbl(rows, ["Week"] + list(tp.columns[1:]))
    # --- коефіцієнт k
    kk = ex["k"]
    reps["T_K"] = tbl([[s["season"], fmn(s["hromada_sum"], 0), fmn(s["reference_sum"], 0), s["weeks"]] for s in kk["seasons"]],
                      ["Сезон", "Накопичена захворюваність громади", "Накопичена захворюваність еталона за ті самі тижні", "Тижнів з даними"])
    sh = sum(s["hromada_sum"] for s in kk["seasons"])
    sr = sum(s["reference_sum"] for s in kk["seasons"])
    reps["K_REF"] = kk["reference"]
    reps["K_SUM_H"], reps["K_SUM_R"], reps["K_VALUE"], reps["K_PCT"] = fmn(sh, 0), fmn(sr, 0), c2(sh / sr), fmn((1 - sh / sr) * 100, 0)
    # --- малі території
    sm = x["Малі території"]
    ne = sm[sm["Назва"].str.contains("Недригайлівська")].iloc[0]
    reps["EX_W"] = c2(ne["Вага власного порогу"])
    reps["EX_OWN"] = fmn(ne["Власний епідемічний поріг"], 0) if "Власний епідемічний поріг" in ne.index else fmn(ne["Власний базовий рівень"], 0)
    reps["EX_REF"] = fmn(ne["Еталон з коеф.: епідемічний поріг"], 0) if "Еталон з коеф.: епідемічний поріг" in ne.index else fmn(ne["Еталон з коеф.: базовий рівень"], 0)
    reps["EX_REC"] = fmn(ne["Рекомендований епідемічний поріг"], 0) if "Рекомендований епідемічний поріг" in ne.index else fmn(ne["Рекомендований базовий рівень"], 0)
    w = x["Ваги малих територій"]
    ww = w[(w["Вага власного порогу"] > 0.05) & (w["Вага власного порогу"] < 0.95) & (w["Випадків на тиждень"] > 0)]
    cstar = ((1 / ww["Вага власного порогу"] - 1) * ww["Випадків на тиждень"]).median()
    reps["CSTAR"] = fmn(cstar, 1)
    for n in (3, 14, 30, 100):
        reps["W_%d" % n] = c2(1.0 / (1.0 + cstar / n))
    reps["EX_CASES"] = fmn(float(ne["Медіана випадків на тиждень"]), 1)
    pr = x["Пропорційність"]
    reps["BETA_MED"], reps["BETA_SD"] = c2(pr["Показник степеня β"].median()), c2(pr["Показник степеня β"].std())
    ny = pr[pr["Назва"].str.contains("Нижньосироватська")].iloc[0]
    reps["BETA_NYZH"], reps["K_NYZH_STRONG"], reps["K_NYZH_WEAK"] = c2(ny["Показник степеня β"]), c2(ny["k у сильні тижні еталона"]), c2(ny["k у слабкі тижні еталона"])
    mw = x["Багато хвиль"]
    mwv = mw.dropna(subset=["Зміна базового рівня, %"]) if "Зміна базового рівня, %" in mw.columns else mw.dropna(subset=["Зміна епідемічного порогу, %"])
    bcol = [c for c in mwv.columns if c.startswith("Зміна") and "дуже" not in c][0]
    vcol = [c for c in mwv.columns if c.startswith("Зміна") and "дуже" in c][0]
    big = mwv[(mwv[bcol].abs() > 12) | (mwv[vcol].abs() > 12)].copy()
    big["m"] = np.maximum(big[bcol].abs(), big[vcol].abs())
    big = big.sort_values("m", ascending=False).head(4)
    parts = ["%s: епідемічний поріг змінюється на %+.0f%%, дуже високий на %+.0f%%" % (r["Назва"], r[bcol], r[vcol]) for _, r in big.iterrows()]
    unchanged = ((mwv[bcol].abs() <= 10) & (mwv[vcol].abs() <= 10)).mean() * 100
    reps["MW_EXAMPLE"] = "кількість перевірених рядів (області, районів, міст і малих територій з надійними даними) %d. У %.0f%% з них пороги змінюються не більше ніж на 10%%. Найбільші зміни: %s." % (len(mwv), unchanged, "; ".join(parts))
    # --- калібрування
    cal_dir = os.path.join(HERE, "output", "calibration")
    cd = pd.read_excel(os.path.join(cal_dir, "calibration_drop.xlsx"), sheet_name="summary")  # розраховано на наборі з 8 сезонів (див. текст посібника)
    reps["CAL_DROP_TABLE"] = tbl([[("без обмеження" if r["drop"] >= 1 else "%d%%" % round(r["drop"] * 100)), c2(r["fa"]), c2(r["delay"]), fmn(r["pct_miss"], 1), c2(r["loss"]), c2(r["se_boot"])] for _, r in cd.iterrows()],
                                 ["Межа падіння", "Хибних сигналів на сезон", "Запізнення, тижнів", "Пропущено, %", "Сумарна помилка", "Похибка сумарної помилки (бутстреп по територіях)"])
    b = cd.loc[cd["loss"].idxmin()]
    reps["CAL_DROP_TEXT"] = ("Найменшу сумарну помилку (%s) дало значення %d%%. Різниця між усіма значеннями не перевищує %s, тоді як похибка оцінки сумарної помилки близько %s, тому вибір межі падіння на результат впливає слабко. "
                             "Значення 20%% залишено: воно має найменшу сумарну помилку і було обране заздалегідь. Ці числа отримано 20.10.2026 на восьми сезонах і для нинішніх наборів сезонів не перераховувались." % (c2(b["loss"]), round(b["drop"] * 100), c2(cd["loss"].max() - cd["loss"].min()), c2(cd["se_boot"].mean())))
    cc = pd.read_excel(os.path.join(cal_dir, "calibration_cleaning.xlsx"), sheet_name="summary")
    order = ["без очищення"] + [v for v in cc["variant"] if v != "без очищення"]
    cc = cc.set_index("variant").loc[order].reset_index()
    reps["CAL_CLEAN_TABLE"] = tbl([[r["variant"], int(r["weeks_removed"]), c2(r["fa"]), c2(r["delay"]), c2(r["loss"]), fmn(r["pct_changed_threshold"], 0)] for _, r in cc.iterrows()],
                                  ["Варіант", "Прибрано тижнів", "Хибних сигналів на сезон", "Запізнення, тижнів", "Сумарна помилка", "Частка пар, де поріг змінився більш ніж на 2%, %"])
    base_loss = float(cc.loc[cc["variant"] == "без очищення", "loss"].iloc[0])
    d2 = float(cc.loc[cc["variant"] == "MAD x 2.5, порівняння 8", "loss"].iloc[0])
    reps["CAL_CLEAN_TEXT"] = ("Різниця сумарної помилки між варіантами (від %s до %s) значно менша за типову похибку оцінки, тобто вплив очищення початку сезону і вибору множника на якість порогів на відкладених сезонах майже непомітний: "
                              "без очищення сумарна помилка %s, з множником 2,5 %s. Правило прибирає лише %d тижнів на 51 території і змінює поріг більш ніж на 2%% у частини пар. "
                              "Його призначення обмежене: не допустити, щоб явно забруднені початкові тижні піднімали поріг окремої території. Тому взято типове значення 2,5, а інструмент калібрування дозволяє перевірити його на власних даних."
                              % (c2(cc["loss"].min()), c2(cc["loss"].max()), c2(base_loss), c2(d2), int(cc.loc[cc["variant"] == "MAD x 2.5, порівняння 8", "weeks_removed"].iloc[0])))
    cl = ex["cleaning"]["steps"]
    reps["T_CLEAN"] = tbl([[s["week"], fmn(s["value"], 1), fmn(s["median"], 1), fmn(s["mad"], 1), fmn(s["limit"], 1), "так, прибрано" if s["removed"] else "ні, залишено"] for s in cl],
                          ["Тиждень", "Показник", "Медіана наступних 8 тижнів", "MAD цих тижнів", "Межа: медіана + 2,5 × MAD", "Показник більший за межу?"])
    # --- чутливість до кута меж епідемії: розрахунки 21.10.2026 зроблено на попередньому способі (один набір сезонів
    # для всіх територій), для нинішнього способу вони не чинні, тому в посібнику лише опис перевірки
    reps["CAL_TRUTH_TEXT"] = ("Кут, яким визначаються межі епідемії в контрольних сезонах, фіксований (2,8) і не підбирається. Якби його підбирали за тією самою перевіркою, "
                              "він перевіряв би сам себе: вищий кут скорочує епідемічний період і зменшує запізнення будь-якого порогу. Чутливість результату до цього кута перевіряє "
                              "інструмент калібрування (розділ 8.4): він повторює весь розрахунок із кутами 2,5 і 3,0. "
                              "Абсолютні значення сумарної помилки між такими розрахунками не порівнюються, порівнюються лише обрані кути, набори сезонів і порядок способів для малих територій. "
                              "Попередню таку перевірку (21.10.2026) виконано ще на способі з одним набором сезонів для всіх територій, тому для нинішнього розрахунку її треба повторити.")
    out = template
    for k, v in reps.items():
        out = out.replace("{{%s}}" % k, str(v))
    left = re.findall(r"\{\{[A-Z_0-9]+\}\}", out)
    if left:
        raise SystemExit("Не заповнено: " + ", ".join(sorted(set(left))))
    return out


if __name__ == "__main__":
    tpl = open(TEMPLATE, encoding="utf-8").read().replace("\r\n", "\n")
    md = fill(tpl, WORKBOOK)
    open(OUT_MD, "w", encoding="utf-8", newline="\n").write(md)
    print("saved", OUT_MD)
    convert(OUT_MD, OUT_DOCX, "Посібник користувача memopt",
            "Підбір порогів MEM для захворюваності на ГРВІ: від даних до рішення",
            ["Версія 0.3, жовтень 2026", "Автор налаштувань: Сергій Грабовий", "Ліцензія GPL (>= 2), як у пакета mem",
             "Підготовлено разом з ШІ-асистентом Claude (Anthropic)"],
            "Посібник memopt: пороги MEM для ГРВІ")
