# Перевірка текстів після збирання документів: застарілі терміни і твердження, заборонена типографіка,
# таблиці порогів, що не збігаються з output/memopt_thresholds.csv, дата розрахунку в тексті, ручні документи,
# старіші за розрахунок. Повертає код 1, якщо знайдено помилки. Запуск: python check_texts.py
import glob
import os
import re
import sys
from datetime import date

import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
os.chdir(HERE)

# Застарілі або заборонені слова: (шаблон, пояснення). Перевіряються тексти для людей і підписи в коді.
TERMS = [
    (r"[Пп]равил[оау]\s+А\b", "«правило А»: назва «правило підтвердження районом»"),
    (r"[Пп]отрібне підтвердження районом", "назва колонки «Правило підтвердження районом»"),
    (r"перевірн", "«перевірні сезони»: «контрольні сезони»"),
    (r"\b[Вв]трат[аиіу]\b", "«втрата»: «сумарна помилка (хибні сигнали + запізнення)»"),
    (r"хибн\w+ спрацюв", "«хибне спрацювання»: «хибний сигнал»"),
    (r"прихован\w+ сезон", "«прихований сезон»: «відкладений сезон»"),
    (r"вікн\w+ епідемі", "«вікно епідемії»: «період епідемії району»"),
    (r"стел\w+ кута", "«стеля кута»: «кут групи як вихідний»"),
    (r"ціна помилок", "«ціна помилок»: «сумарна помилка»"),
    (r"базов\w+ \(епідемічн|базов\w+ поріг|базового порогу", "«базовий поріг»: «епідемічний поріг»"),
    (r"понад 25%|25%: перевірте|change_flag_pct", "позначка змін тепер за найбільшим впливом одного сезону"),
    (r"epidemic_periods|тижні 2-12|тижнів 2-12", "ручні межі епідемії прибрано: межі визначає memtiming кутом 2,8"),
    (r"mcc_guard|−0,05|-0,05 MCC|MCC −0", "запобіжник MCC тепер один помилковий тиждень (guard_weeks)"),
    (r"[Кк]онтрольні сезони без епідемії", "колонка «Сезони без епідемії (пік нижчий за епідемічний поріг)»"),
]
TYPO = [("—", "довге тире"), ("→", "стрілка"), ("·", "середня крапка"), ("…", "три крапки одним знаком")]

# Що перевіряється. Статтю (docs/paper) відкладено, її знахідки лише попередження.
TEXT_FILES = sorted(set(glob.glob("docs/*.md") + ["docs/manual_template.md", "README.md", "1_ЧИТАТИ_СПОЧАТКУ.txt"]
                        + glob.glob("templates/*.txt")))
CODE_FILES = sorted(glob.glob("build_*.py") + glob.glob("R/*.R") + glob.glob("run_*.R") + ["config.json"])
WARN_FILES = sorted(glob.glob("docs/paper/*.md"))
# Документи, написані вручну (числа в них не підставляються автоматично)
HAND = ["docs/ДЛЯ_ЦГЗ_пороги_MEM_коротко.md", "docs/ДЛЯ_КЕРІВНИКА_на_серветці.md", "docs/manual_template.md"]

errors, warns = [], []


def scan(path, typo, sink):
    try:
        lines = open(path, encoding="utf-8").read().splitlines()
    except (OSError, UnicodeDecodeError):
        return
    for i, s in enumerate(lines, 1):
        if "check_texts" in s:
            continue
        for pat, why in TERMS:
            if re.search(pat, s):
                sink.append(f"{path}:{i}: {why}: {s.strip()[:120]}")
        if typo:
            for ch, why in TYPO:
                if ch in s:
                    sink.append(f"{path}:{i}: {why}: {s.strip()[:120]}")


for f in TEXT_FILES:
    scan(f, True, errors)
for f in CODE_FILES:
    scan(f, False, errors)
for f in WARN_FILES:
    scan(f, True, warns)

# Таблиці порогів у документах проти таблиці порогів розрахунку
thr = pd.read_csv("output/memopt_thresholds.csv")
by_name = {r["name"]: r for _, r in thr.iterrows()}
COLS = {"Епідемічний": "base", "Епідемічний поріг": "base", "Середній": "medium", "Середній поріг": "medium",
        "Високий": "high", "Високий поріг": "high", "Дуже високий": "veryhigh", "Дуже високий поріг": "veryhigh"}


def num(x):
    x = x.strip().replace(" ", "").replace(" ", "").replace(",", ".")
    try:
        return float(x)
    except ValueError:
        return None


for f in glob.glob("docs/*.md"):
    lines = open(f, encoding="utf-8").read().splitlines()
    head = None
    for i, s in enumerate(lines, 1):
        if not s.startswith("|"):
            head = None
            continue
        cells = [c.strip() for c in s.strip().strip("|").split("|")]
        if head is None:
            head = cells
            continue
        if set(cells) <= {"---", ""} or not cells:
            continue
        r = by_name.get(cells[0])
        if r is None:
            continue
        for h, c in zip(head, cells):
            k = COLS.get(h)
            v = num(c) if k else None
            if k and v is not None and abs(v - r[k]) > 1:
                errors.append(f"{f}:{i}: {cells[0]}, «{h}» {c} у тексті, у розрахунку {r[k]:.0f}")

# Дата розрахунку в тексті проти дати останнього розрахунку
from thresholds_date import thresholds_date
run_day = thresholds_date()  # день, коли пороги востаннє змінились
for f in TEXT_FILES:
    for i, s in enumerate(open(f, encoding="utf-8").read().splitlines(), 1):
        for m in re.finditer(r"(розрахун\w*|запуск\w*)[^.|]{0,60}?від (\d\d\.\d\d\.\d{4})", s):
            # згадка попереднього розрахунку для порівняння законна
            if re.search(r"[Пп]орівняно з|попередн|старо|раніше", s[max(0, m.start() - 40):m.end()]):
                continue
            if m.group(2) != run_day:
                errors.append(f"{f}:{i}: у тексті розрахунок від {m.group(2)}, останній розрахунок {run_day}")

# Ручні документи, старіші за розрахунок
from datetime import datetime
t_run = datetime.strptime(run_day, "%d.%m.%Y").timestamp()  # ручний текст має бути не старшим за день зміни порогів
for f in HAND:
    if os.path.exists(f) and os.path.getmtime(f) < t_run:
        warns.append(f"{f}: написаний вручну раніше, ніж змінились пороги: перечитайте числа і твердження")

for e in errors:
    print("ПОМИЛКА", e)
for w in warns:
    print("УВАГА", w)
print(f"Перевірка текстів: помилок {len(errors)}, попереджень {len(warns)}")
sys.exit(1 if errors else 0)
