# Збирає всі документи і книги з результатів розрахунку в одному порядку, щоб жоден продукт не лишився застарілим,
# і в кінці перевіряє тексти (check_texts.py). Запуск: python build_all.py (після run_optimizer.R)
import glob
import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
os.chdir(HERE)


def rscript():
    p = os.environ.get("RSCRIPT") or shutil.which("Rscript")
    if p:
        return p
    found = sorted(glob.glob(r"C:\Program Files\R\R-*\bin\Rscript.exe"))
    if not found:
        sys.exit("Rscript не знайдено: встановіть R або задайте змінну середовища RSCRIPT")
    return found[-1]


R = rscript()
STEPS = [
    ("Приклади для посібника", [R, "R/memopt_examples.R"]),
    ("Параметри розрахунку", [sys.executable, "build_params_doc.py"]),
    ("Посібник користувача", [sys.executable, "build_manual.py"]),
    ("Аналіз книги результатів", [sys.executable, "build_report.py"]),
    ("Правило підтвердження районом", [sys.executable, "build_gate_report.py"]),
    ("Ряди для memapp", [R, "build_memapp_series.R"]),
    ("Пороги для гугл-таблиці (xlsx і csv)", [sys.executable, "build_google_sheet.py"]),
    ("Таблиця порогів у Word", [sys.executable, "build_thresholds_table.py"]),
    ("Книга правила для Google Таблиць", [sys.executable, "build_google_rule_sheet.py"]),
    ("Показники за тижнями", [sys.executable, "build_weekly_excel.py"]),
    ("Рисунки", [sys.executable, "build_paper_figs.py"]),
    ("Документи для ЦГЗ і керівника", [sys.executable, "build_cgz_brief.py"]),
]
failed = []
for title, cmd in STEPS:
    print("==", title, flush=True)
    r = subprocess.run(cmd)
    if r.returncode != 0:
        failed.append(title)
print("== Перевірка текстів", flush=True)
chk = subprocess.run([sys.executable, "check_texts.py"]).returncode
if failed:
    print("Не зібрано:", "; ".join(failed))
if failed or chk:
    sys.exit(1)
print("Усе зібрано, перевірка текстів пройдена.")
