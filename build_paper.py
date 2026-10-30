# Збірка статті: docs/paper/СТАТТЯ_пороги_MEM_малі_території.md -> .docx (з рисунками з docs/paper/fig).
# Запуск: python build_paper.py   (після run_optimizer.R, build_gate_report.py і paper_figs.py)
import os
import re
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml.ns import qn
from docx.shared import Cm, Pt, RGBColor

HERE = os.path.dirname(os.path.abspath(__file__))
MD = os.path.join(HERE, "docs", "paper", "СТАТТЯ_пороги_MEM_малі_території.md")
OUT = os.path.join(HERE, "docs", "paper", "СТАТТЯ_пороги_MEM_малі_території.docx")
FONT = "Times New Roman"
INLINE = re.compile(r"(\*\*[^*]+\*\*|\*[^*]+\*|`[^`]+`|\[[0-9, ]+\])")


# Розміри шрифту можна задати змінними середовища MO_BODY_PT і MO_TABLE_PT (типово 11 і 9)
BODY_PT = int(os.environ.get("MO_BODY_PT", "11"))
TABLE_PT = int(os.environ.get("MO_TABLE_PT", "9"))


def set_font(run, size=None, bold=None, italic=None, color=None, name=FONT):
    run.font.name = name
    run._element.rPr.rFonts.set(qn("w:eastAsia"), name)
    run._element.rPr.rFonts.set(qn("w:ascii"), name)
    run._element.rPr.rFonts.set(qn("w:hAnsi"), name)
    if size:
        run.font.size = Pt(size)
    if bold is not None:
        run.font.bold = bold
    if italic is not None:
        run.font.italic = italic
    if color is not None:
        run.font.color.rgb = color


def add_inline(p, text, size=None, bold_all=False, italic_all=False):
    if size is None:
        size = BODY_PT
    for part in INLINE.split(text):
        if not part:
            continue
        if part.startswith("**") and part.endswith("**"):
            set_font(p.add_run(part[2:-2]), size, True, italic_all or None)
        elif part.startswith("*") and part.endswith("*") and len(part) > 2:
            set_font(p.add_run(part[1:-1]), size, bold_all or None, True)
        elif part.startswith("`") and part.endswith("`"):
            set_font(p.add_run(part[1:-1]), size - 1, None, None, RGBColor(0x40, 0x40, 0x40), "Consolas")
        else:
            set_font(p.add_run(part), size, bold_all or None, italic_all or None)


def make_doc():
    doc = Document()
    sec = doc.sections[0]
    sec.page_width, sec.page_height = Cm(21.0), Cm(29.7)
    sec.left_margin = sec.right_margin = Cm(2.0)
    sec.top_margin = sec.bottom_margin = Cm(2.0)
    st = doc.styles["Normal"]
    st.font.name = FONT
    st.font.size = Pt(BODY_PT)
    st.element.rPr.rFonts.set(qn("w:eastAsia"), FONT)
    st.paragraph_format.space_after = Pt(4)
    st.paragraph_format.line_spacing = 1.1
    for name, size, before, after in (("Heading 1", BODY_PT + 3, 12, 4), ("Heading 2", BODY_PT + 1, 8, 3), ("Heading 3", BODY_PT, 6, 2)):
        h = doc.styles[name]
        h.font.name = FONT
        h.font.size = Pt(size)
        h.font.bold = True
        h.font.color.rgb = RGBColor(0, 0, 0)
        for k in ("w:eastAsia", "w:ascii", "w:hAnsi"):
            h.element.rPr.rFonts.set(qn(k), FONT)
        h.paragraph_format.space_before = Pt(before)
        h.paragraph_format.space_after = Pt(after)
        h.paragraph_format.keep_with_next = True
    return doc


def add_table(doc, rows):
    if not rows:
        return
    ncol = max(len(r) for r in rows)
    t = doc.add_table(rows=len(rows), cols=ncol)
    t.style = "Table Grid"
    t.alignment = 1
    for i, row in enumerate(rows):
        for j in range(ncol):
            cell = t.cell(i, j)
            cell.text = ""
            p = cell.paragraphs[0]
            p.paragraph_format.space_after = Pt(0)
            add_inline(p, row[j] if j < len(row) else "", size=TABLE_PT, bold_all=(i == 0))
    doc.add_paragraph().paragraph_format.space_after = Pt(2)


def add_picture(doc, path, caption):
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.add_run().add_picture(os.path.join(HERE, "docs", "paper", path), width=Cm(16))
    c = doc.add_paragraph()
    c.alignment = WD_ALIGN_PARAGRAPH.LEFT
    add_inline(c, caption, size=BODY_PT - 1)
    c.paragraph_format.space_after = Pt(8)


def convert(md_path, out_path):
    src = open(md_path, encoding="utf-8").read().replace("\r\n", "\n").split("\n")
    doc = make_doc()
    i, n = 0, len(src)
    words = 0
    while i < n:
        ln = src[i]
        s = ln.strip()
        if s == "":
            i += 1
            continue
        if s.startswith("# "):
            p = doc.add_paragraph()
            p.alignment = WD_ALIGN_PARAGRAPH.CENTER
            set_font(p.add_run(s[2:].strip()), BODY_PT + 4, True)
            i += 1
            continue
        if s.startswith("## "):
            doc.add_heading(s[3:].strip(), level=1)
            i += 1
            continue
        if s.startswith("### "):
            doc.add_heading(s[4:].strip(), level=2)
            i += 1
            continue
        if s.startswith("#### "):
            doc.add_heading(s[5:].strip(), level=3)
            i += 1
            continue
        m = re.match(r"^!\[(.*)\]\((.*)\)$", s)
        if m:
            add_picture(doc, m.group(2), m.group(1))
            i += 1
            continue
        if s.startswith("$$") and s.endswith("$$") and len(s) > 4:
            p = doc.add_paragraph()
            p.alignment = WD_ALIGN_PARAGRAPH.CENTER
            set_font(p.add_run(s.strip("$").strip()), BODY_PT, None, True)
            i += 1
            continue
        if s.startswith("$$"):
            buf = [s.strip("$").strip()]
            i += 1
            while i < n and not src[i].strip().endswith("$$") and src[i].strip() != "$$":
                buf.append(src[i].strip())
                i += 1
            if i < n and src[i].strip() != s:
                buf.append(src[i].strip().strip("$").strip())
            i += 1
            for f in [b for b in buf if b]:
                p = doc.add_paragraph()
                p.alignment = WD_ALIGN_PARAGRAPH.CENTER
                set_font(p.add_run(f), BODY_PT, None, True)
            continue
        if s.startswith("|"):
            rows = []
            while i < n and src[i].strip().startswith("|"):
                cells = [c.strip() for c in src[i].strip().strip("|").split("|")]
                if not all(re.fullmatch(r":?-{2,}:?", c) for c in cells):
                    rows.append(cells)
                i += 1
            add_table(doc, rows)
            words += sum(len(c.split()) for r in rows for c in r)
            continue
        m = re.match(r"^[-*]\s+(.*)$", s)
        if m:
            p = doc.add_paragraph()
            p.paragraph_format.left_indent = Cm(0.8)
            p.paragraph_format.first_line_indent = Cm(-0.4)
            p.paragraph_format.space_after = Pt(2)
            set_font(p.add_run("• "), BODY_PT)
            add_inline(p, m.group(1))
            words += len(m.group(1).split())
            i += 1
            continue
        m = re.match(r"^(\d+)\.\s+(.*)$", s)
        if m:
            p = doc.add_paragraph()
            p.paragraph_format.left_indent = Cm(0.8)
            p.paragraph_format.first_line_indent = Cm(-0.6)
            p.paragraph_format.space_after = Pt(2)
            set_font(p.add_run(m.group(1) + ". "), BODY_PT)
            add_inline(p, m.group(2))
            words += len(m.group(2).split())
            i += 1
            continue
        buf = [s]
        i += 1
        while i < n and src[i].strip() and not re.match(r"^(#|\||!\[|\$\$|[-*]\s|\d+\.\s)", src[i].strip()):
            buf.append(src[i].strip())
            i += 1
        para = " ".join(buf)
        words += len(para.split())
        p = doc.add_paragraph()
        p.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
        add_inline(p, para)
    doc.core_properties.title = "Пороги MEM для малих територій"
    doc.core_properties.author = "Сергій Грабовий"
    doc.core_properties.language = "uk-UA"
    doc.save(out_path)
    print("saved", out_path, "слів у тексті:", words)


if __name__ == "__main__":
    convert(MD, OUT)
