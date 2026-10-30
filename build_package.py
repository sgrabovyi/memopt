# Збірка пакета для передачі (memopt_ЦГЗ поряд із memopt і memopt_ЦГЗ.zip): лише R і розрахунок порогів,
# без Python, збірки документів, чернеток і експорту в наш дашборд (функцію експорту вирізано з ядра і конфігу).
# Запуск: python build_package.py   (після run_optimizer.R, щоб у пакет потрапили поточні результати)
import io, json, os, shutil, zipfile

HERE = os.path.dirname(os.path.abspath(__file__))
PARENT = os.path.dirname(HERE)
PKG = os.path.join(PARENT, "memopt_ЦГЗ")
ZIP = os.path.join(PARENT, "memopt_ЦГЗ.zip")

FILES = [
    ("2_Встановити_пакети.bat", "2_Встановити_пакети.bat"), ("3_Розрахувати_пороги.bat", "3_Розрахувати_пороги.bat"),
    ("_find_r.cmd", "_find_r.cmd"), ("install_packages.R", "install_packages.R"), ("run_optimizer.R", "run_optimizer.R"),
    ("run_backtest.R", "run_backtest.R"), ("build_memapp_series.R", "build_memapp_series.R"),
    ("templates/ЧИТАТИ_СПОЧАТКУ_для_пакета.txt", "1_ЧИТАТИ_СПОЧАТКУ.txt"),
    ("R/memopt_backtest.R", "R/memopt_backtest.R"),
    ("input/hromadas.xlsx", "input/hromadas.xlsx"), ("input/territories.csv", "input/territories.csv"),
    ("templates/config_template.json", "templates/config_template.json"), ("templates/series_template.xlsx", "templates/series_template.xlsx"),
    ("templates/territories_template.csv", "templates/territories_template.csv"),
    ("output/memopt_results.xlsx", "output/memopt_results.xlsx"), ("output/memopt_thresholds.csv", "output/memopt_thresholds.csv"),
    ("output/last_run.rds", "output/last_run.rds"), ("output/season_sets.json", "output/season_sets.json"),
    ("output/ряди_для_memapp.xlsx", "output/ряди_для_memapp.xlsx"),
    ("docs/ПОСІБНИК_користувача.docx", "docs/ПОСІБНИК_користувача.docx"), ("docs/ПАРАМЕТРИ_розрахунку_порогів.docx", "docs/ПАРАМЕТРИ_розрахунку_порогів.docx"),
    ("docs/ЗВІТ_аналіз_книги_результатів.docx", "docs/ЗВІТ_аналіз_книги_результатів.docx"), ("docs/ПРАВИЛО_умова_за_районом.docx", "docs/ПРАВИЛО_умова_за_районом.docx"),
    ("docs/ДЛЯ_ЦГЗ_пороги_MEM_коротко.docx", "docs/ДЛЯ_ЦГЗ_пороги_MEM_коротко.docx"),
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
    if os.path.exists(PKG): shutil.rmtree(PKG)
    for src, dst in FILES:
        s = os.path.join(HERE, src); d = os.path.join(PKG, dst)
        os.makedirs(os.path.dirname(d), exist_ok=True)
        if not os.path.exists(s): raise SystemExit("немає файлу " + s)
        shutil.copy2(s, d)
    os.makedirs(os.path.join(PKG, "input", "series"), exist_ok=True)
    import csv
    terr = list(csv.DictReader(io.open(os.path.join(HERE, "input", "territories.csv"), encoding="utf-8-sig")))
    for f in sorted({r["file"] for r in terr}):
        if f == "hromadas.xlsx": continue
        shutil.copy2(os.path.join(HERE, "input", "series", f), os.path.join(PKG, "input", "series", f))
    core = io.open(os.path.join(HERE, "R", "memopt_core.R"), encoding="utf-8").read()
    io.open(os.path.join(PKG, "R", "memopt_core.R"), "w", encoding="utf-8", newline="").write(strip_export(core))
    cfg = json.load(io.open(os.path.join(HERE, "config.json"), encoding="utf-8"))
    cfg.pop("export", None)
    io.open(os.path.join(PKG, "config.json"), "w", encoding="utf-8", newline="").write(json.dumps(cfg, ensure_ascii=False, indent=2) + "\n")
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
