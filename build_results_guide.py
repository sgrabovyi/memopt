# Пояснення до книги результатів для ЦГЗ (без чисел конкретної області): docs/Як_читати_книгу_результатів.md, тут лише збірка .docx.
# Запуск: python build_results_guide.py
import os

os.environ.setdefault("MO_BODY_PT", "13")
os.environ.setdefault("MO_TABLE_PT", "12")
from build_paper import convert

HERE = os.path.dirname(os.path.abspath(__file__))

if __name__ == "__main__":
    convert(os.path.join(HERE, "docs", "Як_читати_книгу_результатів.md"), os.path.join(HERE, "docs", "Як_читати_книгу_результатів.docx"))
