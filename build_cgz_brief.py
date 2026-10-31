# Стислий документ для ЦГЗ: проблеми з даними, рішення, результати. Текст написано вручну у
# docs/ДЛЯ_ЦГЗ_пороги_MEM_коротко.md (усі числа з книги результатів output/memopt_results.xlsx; check_texts.py звіряє таблиці порогів і дату розрахунку), тут лише збірка .docx.
# Також збирає коротку версію для керівника без статистики (docs/ДЛЯ_КЕРІВНИКА_на_серветці).
# Запуск: python build_cgz_brief.py
import os

os.environ.setdefault("MO_BODY_PT", "14")
os.environ.setdefault("MO_TABLE_PT", "12")
from build_paper import convert

HERE = os.path.dirname(os.path.abspath(__file__))
MD = os.path.join(HERE, "docs", "ДЛЯ_ЦГЗ_пороги_MEM_коротко.md")
OUT = os.path.join(HERE, "docs", "ДЛЯ_ЦГЗ_пороги_MEM_коротко.docx")
MD2 = os.path.join(HERE, "docs", "ДЛЯ_КЕРІВНИКА_на_серветці.md")
OUT2 = os.path.join(HERE, "docs", "ДЛЯ_КЕРІВНИКА_на_серветці.docx")

if __name__ == "__main__":
    convert(MD, OUT)
    convert(MD2, OUT2)
