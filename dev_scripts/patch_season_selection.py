# Автоматичний добір сезонів для основних територій на початку кожного запуску memopt.
import io, os, shutil
H = "C:/Диск D/01 - Word/аналіз грві/мем (Грабовий С.Л.)/memopt"
P = os.path.join(H, "R", "memopt_core.R")
shutil.copy2(P, os.path.join("C:/Users/User/AppData/Local/Temp/claude/C-------D-01---Word-------------Dashboard-GRVI---Openclaude/5ec10f60-1ca6-4be3-a4d0-b9a8e5b10045/scratchpad/mem", "memopt_core_v03_before_seasons.R"))
s = io.open(P, encoding="utf-8").read()

def rep(a, b):
    global s
    assert s.count(a) == 1, ("not unique or missing", a[:70], s.count(a))
    s = s.replace(a, b)

# 1. аркуш і підписи
rep('proportionality = "Пропорційність", waves = "Багато хвиль", thresholds = "Пороги", dim = "Експорт Dim_Thresholds", config = "Конфігурація")',
    'proportionality = "Пропорційність", waves = "Багато хвиль", thresholds = "Пороги", dim = "Експорт Dim_Thresholds", config = "Конфігурація",\n  season_pick = "Добір сезонів")')
rep('loss_mean = "Середня сумарна помилка", n = "Спроб", pct_failed = "Поріг не вдалось побудувати, %", cases_week = "Випадків на тиждень", kind = "Вид")',
    'loss_mean = "Середня сумарна помилка", n = "Спроб", pct_failed = "Поріг не вдалось побудувати, %", cases_week = "Випадків на тиждень", kind = "Вид",\n  n_seasons = "Сезонів у наборі", eval_seasons = "Перевірні сезони", note = "Примітка", n_available = "Сезонів доступно")')

# 2. налаштування: блок season_selection і читання season_sets.json
rep('  cfg$paths$workbook_name <- mo_def(cfg$paths$workbook_name, "memopt_results.xlsx")\n  cfg$paths$output_dir <- mo_def(cfg$paths$output_dir, "output")\n  cfg$parallel$workers <- mo_def(cfg$parallel$workers, 1)\n  cfg\n}',
    '  cfg$paths$workbook_name <- mo_def(cfg$paths$workbook_name, "memopt_results.xlsx")\n  cfg$paths$output_dir <- mo_def(cfg$paths$output_dir, "output")\n  cfg$parallel$workers <- mo_def(cfg$parallel$workers, 1)\n'
    '  ss <- cfg$season_selection\n  if (is.null(ss)) ss <- list()\n  ss$enabled <- mo_def(ss$enabled, TRUE)\n  ss$min_seasons <- mo_def(ss$min_seasons, 5)\n  ss$validation_seasons <- mo_def(ss$validation_seasons, 5)\n  ss$tolerance <- mo_def(ss$tolerance, 0)\n  cfg$season_selection <- ss\n'
    '  # набори сезонів, обрані попереднім запуском (output/season_sets.json): їх бачать ретроспектива, приклади і книга для memapp\n'
    '  sf <- file.path(cfg$.path(cfg$paths$output_dir), "season_sets.json")\n  cfg$.season_sets <- if (file.exists(sf)) fromJSON(sf, simplifyVector = FALSE) else NULL\n  cfg\n}')

# 3. mo_series: набір сезонів території (якщо обрано), all_seasons = TRUE його ігнорує
rep('mo_series <- function(row, cfg, log = NULL, clean = TRUE, multiplier = NULL, compare_weeks = NULL) {\n  d <- mo_read_raw(row, cfg)\n  d <- d[, !(names(d) %in% names(cfg$exclude_seasons)), drop = FALSE]\n  d <- d[, colSums(!is.na(d)) > 0, drop = FALSE]\n  d <- d[, order(names(d)), drop = FALSE]\n  if (NCOL(d) > cfg$season$max_seasons) d <- d[, tail(names(d), cfg$season$max_seasons), drop = FALSE]\n',
    'mo_series <- function(row, cfg, log = NULL, clean = TRUE, multiplier = NULL, compare_weeks = NULL, all_seasons = FALSE) {\n  d <- mo_read_raw(row, cfg)\n  d <- d[, !(names(d) %in% names(cfg$exclude_seasons)), drop = FALSE]\n  d <- d[, colSums(!is.na(d)) > 0, drop = FALSE]\n  d <- d[, order(names(d)), drop = FALSE]\n  if (NCOL(d) > cfg$season$max_seasons) d <- d[, tail(names(d), cfg$season$max_seasons), drop = FALSE]\n'
    '  ssel <- if (all_seasons) NULL else cfg$.season_sets[[row$id]]\n  if (!is.null(ssel)) d <- d[, names(d) %in% unlist(ssel), drop = FALSE]\n')

# 4. перевірні сезони в оцінці на відкладених сезонах і в скані кутів
rep('mo_loso_eval <- function(d, angle, truth, tms = NULL, eval_d = NULL) {\n  S <- names(d); out <- list()\n  ed <- if (is.null(eval_d)) d else eval_d\n  for (s in S) {',
    'mo_loso_eval <- function(d, angle, truth, tms = NULL, eval_d = NULL, eval_seasons = NULL) {\n  S <- names(d); out <- list()\n  ed <- if (is.null(eval_d)) d else eval_d\n  for (s in if (is.null(eval_seasons)) S else S[S %in% eval_seasons]) {')
rep('mo_scan_angles <- function(d, cfg) {\n  grid <- mo_angle_grid(cfg); truth <- cfg$angle$truth_angle',
    'mo_scan_angles <- function(d, cfg, eval_seasons = NULL) {\n  grid <- mo_angle_grid(cfg); truth <- cfg$angle$truth_angle')
rep('    ev <- mo_loso_eval(d, p, truth, tms)\n    data.frame(angle = p,', '    ev <- mo_loso_eval(d, p, truth, tms, eval_seasons = eval_seasons)\n    data.frame(angle = p,')
rep('mo_choose_angle <- function(d, cfg) mo_select_angle(mo_scan_angles(d, cfg), cfg)',
    'mo_choose_angle <- function(d, cfg, eval_seasons = NULL) mo_select_angle(mo_scan_angles(d, cfg, eval_seasons), cfg)\n\n'
    '# ---------------------------------------------------------------- добір сезонів\n'
    '# Кандидати: N останніх сезонів для N від min_seasons до всіх доступних (відкидаються лише найстаріші).\n'
    '# Усі кандидати оцінюються на тих самих перевірних сезонах (validation_seasons останніх, не більше min_seasons):\n'
    '# для кожного перевірного сезону поріг будується на наборі без нього, рахуються хибні сигнали і запізнення, кут\n'
    '# обирається за звичайними правилами. Береться набір із найменшою сумарною помилкою; при рівності (у межах tolerance) довший.\n'
    'mo_select_seasons <- function(d, cfg) {\n'
    '  ss <- cfg$season_selection; S <- names(d); N <- length(S)\n'
    '  one <- function(note) list(seasons = S, eval_seasons = NULL, note = note,\n'
    '    table = data.frame(n_available = N, n_seasons = N, seasons_used = paste(S, collapse = ", "), eval_seasons = "усі", angle = NA, base = NA, medium = NA, fa = NA, delay = NA, miss_pct = NA, loss = NA, allowed = NA, chosen = TRUE, note = note))\n'
    '  if (!isTRUE(ss$enabled)) return(one("добір сезонів вимкнено, взято всі"))\n'
    '  if (N <= ss$min_seasons) return(one(paste0("доступно ", N, " сезонів, не більше мінімуму ", ss$min_seasons, ": взято всі")))\n'
    '  V <- tail(S, min(ss$validation_seasons, ss$min_seasons))\n'
    '  rows <- lapply(ss$min_seasons:N, function(n) {\n'
    '    sub <- tail(S, n); ch <- mo_choose_angle(d[, sub, drop = FALSE], cfg, eval_seasons = V)\n'
    '    sc <- ch$scan[ch$scan$chosen, ][1, ]\n'
    '    data.frame(n_available = N, n_seasons = n, seasons_used = paste(sub, collapse = ", "), eval_seasons = paste(V, collapse = ", "), angle = ch$angle,\n'
    '               base = sc$base, medium = sc$medium, fa = sc$fa, delay = sc$delay, miss_pct = sc$miss_pct, loss = sc$loss,\n'
    '               allowed = !grepl("немає допустимого", ch$note), stringsAsFactors = FALSE)\n'
    '  })\n'
    '  tab <- do.call(rbind, rows)\n'
    '  cand <- tab$allowed & !is.na(tab$loss)\n'
    '  if (!any(cand)) { pick <- N; note <- "жоден набір не дав допустимого кута, взято всі сезони" } else {\n'
    '    best <- min(tab$loss[cand]); pick <- max(tab$n_seasons[cand & tab$loss <= best + ss$tolerance + 1e-9])\n'
    '    note <- "мінімум сумарної помилки на перевірних сезонах; при рівності довший набір"\n'
    '  }\n'
    '  tab$chosen <- tab$n_seasons == pick; tab$note <- ifelse(tab$chosen, note, "")\n'
    '  list(seasons = tail(S, pick), eval_seasons = V, note = note, table = tab)\n'
    '}')

# 5. mo_territory: перевірні сезони з добору
rep('mo_territory <- function(item, cfg) {\n  t <- item$row; d <- item$d\n  ch <- mo_choose_angle(d, cfg)',
    'mo_territory <- function(item, cfg) {\n  t <- item$row; d <- item$d\n  ch <- mo_choose_angle(d, cfg, item$eval_seasons)')

# 6. mo_run: крок 1а
rep('  cat("1. Читання і підготовка рядів\\n")\n  ser <- list()\n  for (i in seq_len(nrow(terr))) ser[[terr$id[i]]] <- mo_series(terr[i, ], cfg, log)\n  cens <- if (length(log$rows)) do.call(rbind, log$rows) else data.frame()\n\n  cat("2. Основні території: кут, пороги, вплив сезонів, стабільність\\n"); flush.console()\n  series_terr <- terr[terr$kind == "main", ]\n  items <- lapply(seq_len(nrow(series_terr)), function(i) list(row = series_terr[i, ], d = ser[[series_terr$id[i]]]))\n',
    '  cat("1. Читання і підготовка рядів\\n")\n  cfg$.season_sets <- NULL\n  ser <- list()\n  for (i in seq_len(nrow(terr))) ser[[terr$id[i]]] <- mo_series(terr[i, ], cfg, log, all_seasons = TRUE)\n  cens <- if (length(log$rows)) do.call(rbind, log$rows) else data.frame()\n\n'
    '  series_terr <- terr[terr$kind == "main", ]\n'
    '  cat("1а. Добір сезонів основних територій\\n"); flush.console()\n'
    '  sel <- mo_pmap(lapply(series_terr$id, function(id) ser[[id]]), mo_select_seasons, cfg)\n'
    '  names(sel) <- series_terr$id\n'
    '  season_pick <- do.call(rbind, lapply(seq_along(sel), function(i) data.frame(id = series_terr$id[i], name = series_terr$name[i], sel[[i]]$table, row.names = NULL)))\n'
    '  sets <- list()\n'
    '  for (id in series_terr$id) {\n'
    '    ser[[id]] <- ser[[id]][, sel[[id]]$seasons, drop = FALSE]; sets[[id]] <- sel[[id]]$seasons\n'
    '    cat("  ", series_terr$name[match(id, series_terr$id)], ":", length(sel[[id]]$seasons), "з", sel[[id]]$table$n_available[1], "сезонів (", sel[[id]]$note, ")\\n")\n'
    '  }\n'
    '  cfg$.season_sets <- sets\n'
    '  writeLines(jsonlite::toJSON(sets, auto_unbox = FALSE, pretty = TRUE), file.path(out_dir, "season_sets.json"), useBytes = TRUE)\n\n'
    '  cat("2. Основні території: кут, пороги, вплив сезонів, стабільність\\n"); flush.console()\n'
    '  items <- lapply(seq_len(nrow(series_terr)), function(i) list(row = series_terr[i, ], d = ser[[series_terr$id[i]]], eval_seasons = sel[[series_terr$id[i]]]$eval_seasons))\n')

# 7. аркуш і рядок конфігурації
rep('  add(S$thresholds, thr_all)\n  add(S$main, tabs)\n', '  add(S$thresholds, thr_all)\n  add(S$main, tabs)\n  add(S$season_pick, season_pick)\n')
rep('                                   "максимум сезонів у розрахунку", "випуск"),', '                                   "максимум сезонів у розрахунку", "добір сезонів основних територій", "випуск"),')
rep('                 paste(cfg$small_areas$groups$reliable_min, cfg$small_areas$groups$limited_min, sep = ", "), cfg$season$max_seasons,\n                 paste0("memopt 0.3, ",',
    '                 paste(cfg$small_areas$groups$reliable_min, cfg$small_areas$groups$limited_min, sep = ", "), cfg$season$max_seasons,\n'
    '                 if (isTRUE(cfg$season_selection$enabled)) paste0("увімкнено: набори з N останніх сезонів, N від ", cfg$season_selection$min_seasons, " до всіх доступних; оцінка на ", min(cfg$season_selection$validation_seasons, cfg$season_selection$min_seasons), " останніх (перевірних) сезонах; мінімум сумарної помилки, при рівності довший набір") else "вимкнено: усі сезони після виключень",\n'
    '                 paste0("memopt 0.3, ",')
rep('saveRDS(list(tabs = tabs, hr = hr, prop = prop, mw = mw, scans = scans, floors = floors, thr = thr_all), file.path(out_dir, "last_run.rds"))',
    'saveRDS(list(tabs = tabs, hr = hr, prop = prop, mw = mw, scans = scans, floors = floors, thr = thr_all, season_pick = season_pick, season_sets = sets), file.path(out_dir, "last_run.rds"))')

io.open(P, "w", encoding="utf-8", newline="").write(s)
print("core patched")

# config.json і шаблон: блок season_selection
for rel in ("config.json", "templates/config_template.json"):
    p = os.path.join(H, rel); c = io.open(p, encoding="utf-8").read()
    old = '  "season_subsets": {\n    "min_seasons": 5\n  },\n'
    assert c.count(old) == 1, rel
    c = c.replace(old, '  "season_selection": {\n    "enabled": true,\n    "min_seasons": 5,\n    "validation_seasons": 5,\n    "tolerance": 0\n  },\n' + old)
    io.open(p, "w", encoding="utf-8", newline="").write(c); print("config patched", rel)

# тестовий конфіг: без ручного виключення 2012-2015, окрема папка виходу
import json
c = json.load(io.open(os.path.join(H, "config.json"), encoding="utf-8"))
for k in ("2012/2013", "2013/2014", "2014/2015"):
    c["exclude_seasons"].pop(k)
c["paths"]["output_dir"] = "output/auto_seasons_test"
c["project"] = c["project"] + " (тест добору сезонів: 2012-2015 не виключені вручну)"
io.open(os.path.join(H, "config_auto_seasons_test.json"), "w", encoding="utf-8", newline="").write(json.dumps(c, ensure_ascii=False, indent=2) + "\n")
print("test config written; exclude:", list(c["exclude_seasons"]))
