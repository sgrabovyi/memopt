# Збірка пакета для передачі (memopt_ЦГЗ поряд із memopt і memopt_ЦГЗ.zip): застосунок memopt, ядро розрахунку, шаблони,
# посібник і звіт; без даних і результатів Сумщини, без Python, збірки документів і експорту в наш дашборд
# (функцію експорту вирізано з ядра і шаблону налаштувань). Запуск: python build_package.py
import io, json, os, shutil, zipfile

HERE = os.path.dirname(os.path.abspath(__file__))
PARENT = os.path.dirname(HERE)
PKG = os.path.join(PARENT, "memopt_ЦГЗ")
ZIP = os.path.join(PARENT, "memopt_ЦГЗ.zip")

FILES = [
    ("templates/ЧИТАТИ_СПОЧАТКУ_для_пакета.txt", "1_ЧИТАТИ_СПОЧАТКУ.txt"),
    ("2_Встановити_пакети.bat", "2_Встановити_пакети.bat"), ("Запустити_memopt.bat", "3_Запустити_memopt.bat"),
    ("_find_r.cmd", "_find_r.cmd"), ("install_packages.R", "install_packages.R"), ("run_optimizer.R", "run_optimizer.R"),
    ("app/app.R", "app/app.R"),
    ("templates/series_template.xlsx", "templates/series_template.xlsx"), ("templates/territories_template.csv", "templates/territories_template.csv"),
    ("docs/ПОСІБНИК_користувача.docx", "docs/ПОСІБНИК_користувача.docx"),
    ("docs/ЗВІТ_аналіз_книги_результатів.docx", "docs/ЗВІТ_аналіз_книги_результатів.docx"),
]

def strip_export(core):
    """Вирізає з ядра експорт у дашборд: рядок налаштування, блок запису, аркуш."""
    def cut(s, a, b=None):
        assert s.count(a) == 1, ("не знайдено або не унікально", a[:60])
        return s.replace(a, "" if b is None else b)
    core = cut(core, '  cfg$export$dashboard_dim_thresholds <- isTRUE(cfg$export$dashboard_dim_thresholds)\n')
    i = core.index("  # необов'язковий експорт під дашборд")
    j = core.index("  hdr <- createStyle(", i)
    core = core[:i] + core[j:]
    core = cut(core, '  if (!is.null(dim)) add(S$dim, dim)\n')
    core = cut(core, ', dim = "Експорт Dim_Thresholds"', '')
    assert "dashboard" not in core.lower() and "Dim_Thresholds" not in core, "залишки експорту в ядрі"
    return core

if __name__ == "__main__":
    # чистий пакет: застосунок, ядро, шаблони, посібник і звіт; без даних і результатів Сумщини, без Python і експорту в дашборд
    if os.path.exists(PKG): shutil.rmtree(PKG)
    for src, dst in FILES:
        s = os.path.join(HERE, src); d = os.path.join(PKG, dst)
        os.makedirs(os.path.dirname(d), exist_ok=True)
        if not os.path.exists(s): raise SystemExit("немає файлу " + s)
        shutil.copy2(s, d)
    os.makedirs(os.path.join(PKG, "projects"), exist_ok=True)
    io.open(os.path.join(PKG, "projects", "ЩО_ТУТ.txt"), "w", encoding="utf-8").write(
        "Тут застосунок memopt створює папку для кожного проєкту області: datasets (ряди), territories.csv, config.json, output (результати).\n")
    os.makedirs(os.path.join(PKG, "R"), exist_ok=True)
    core = io.open(os.path.join(HERE, "R", "memopt_core.R"), encoding="utf-8").read()
    io.open(os.path.join(PKG, "R", "memopt_core.R"), "w", encoding="utf-8", newline="").write(strip_export(core))
    tpl = json.load(io.open(os.path.join(HERE, "templates", "config_template.json"), encoding="utf-8"))
    tpl.pop("export", None)
    io.open(os.path.join(PKG, "templates", "config_template.json"), "w", encoding="utf-8", newline="").write(json.dumps(tpl, ensure_ascii=False, indent=2) + "\n")
    if os.path.exists(ZIP): os.remove(ZIP)
    n = 0
    with zipfile.ZipFile(ZIP, "w", zipfile.ZIP_DEFLATED) as z:
        for root, _, files in os.walk(PKG):
            for fn in files:
                p = os.path.join(root, fn); z.write(p, os.path.relpath(p, PARENT)); n += 1
    print("пакет:", PKG, "| файлів:", n, "| zip:", ZIP, "%.2f МБ" % (os.path.getsize(ZIP) / 1e6))
