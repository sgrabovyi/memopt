# memopt: підбір порогів MEM (метод рухомої епідемії) для захворюваності на ГРВІ, версія 0.3
# Працює поверх пакета mem (Vega, Lozano та ін.), сам mem не змінює.
# Автор налаштувань: Сергій Грабовий. Ліцензія: GPL (>= 2), як у mem.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(mem)
  library(readxl)
  library(jsonlite)
  library(openxlsx)
})

# Назви аркушів книги результатів
MO_SHEETS <- list(main = "Основні території", small = "Малі території", quality = "Якість малих територій", angle = "Кут нахилу",
  floor = "Межа кута", group_angle = "Кут за групами малих територій", influence = "Вплив сезонів", stability = "Стабільність наборів",
  cleaning = "Очищення початку", hold_groups = "Перевірка відкладений сезон", hold_all = "Перевірка усі разом", weights = "Ваги малих територій",
  proportionality = "Пропорційність", waves = "Багато хвиль", thresholds = "Пороги", dim = "Експорт Dim_Thresholds", config = "Конфігурація")

# Підписи колонок у книзі результатів
MO_LABELS <- c(id = "Код", name = "Назва", level = "Рівень", seasons = "Сезонів", angle = "Кут нахилу", base = "Епідемічний поріг", medium = "Середній поріг",
  high = "Високий поріг", veryhigh = "Дуже високий поріг", sens = "Чутливість", spec = "Специфічність", ppv = "Прогностична цінність позитивного результату",
  npv = "Прогностична цінність негативного результату", mcc = "Коефіцієнт кореляції Метьюза", youden = "Індекс Юдена",
  balance = "Баланс (менше з чутливості та специфічності)", group = "Група", type = "Тип", population = "Населення",
  median_cases_week = "Медіана випадків на тиждень", tier = "Група за кількістю випадків", reference = "Територія-еталон",
  k = "Коефіцієнт до еталона", rec_base = "Рекомендований епідемічний поріг", rec_medium = "Рекомендований середній поріг", rec_high = "Рекомендований високий поріг",
  rec_veryhigh = "Рекомендований дуже високий поріг", how = "Як отримано", own_base = "Власний епідемічний поріг", own_medium = "Власний середній поріг",
  own_high = "Власний високий поріг", own_veryhigh = "Власний дуже високий поріг", ref_k_base = "Еталон з коеф.: епідемічний поріг", ref_k_medium = "Еталон з коеф.: середній поріг",
  ref_k_high = "Еталон з коеф.: високий поріг", ref_k_veryhigh = "Еталон з коеф.: дуже високий поріг", r_min = "Мін. кореляція з еталоном",
  r_mean = "Середня кореляція з еталоном", fa_total = "Хибних сигналів за сезони (власний поріг)", delay_mean = "Середнє запізнення, тижнів",
  missed = "Пропущено сезонів", seasons_ok = "Сезонів без проблем", season = "Сезон", base_without = "Епідемічний поріг без цього сезону",
  pct_base = "Зміна епідемічного порогу, %", pct_veryhigh = "Зміна дуже високого порогу, %", influence = "Вплив сезону", cutoff = "Межа впливу",
  flagged = "Позначено для розгляду", n_sets = "Наборів сезонів", base_min = "Епідемічний поріг мін", base_p10 = "Епідемічний поріг 10%", base_median = "Епідемічний поріг медіана",
  base_p90 = "Епідемічний поріг 90%", base_max = "Епідемічний поріг макс", veryhigh_min = "Дуже високий мін", veryhigh_median = "Дуже високий медіана",
  veryhigh_max = "Дуже високий макс", weeks_censored = "Прибрано тижнів", weeks = "Тижні", method = "Спосіб", held = "Відкладений сезон",
  trials = "Спроб", pct_threshold_failed = "Поріг не вдалось побудувати, %", false_alerts_per_season = "Хибних сигналів на сезон",
  pct_seasons_with_false_alert = "Сезонів із хибним сигналом, %", mean_delay_weeks = "Запізнення, тижнів (без пропусків)",
  pct_missed_incl_failed = "Пропущено з урахуванням невдач, %", pct_without_problems = "Сезонів без проблем, %",
  fa = "Хибних сигналів на сезон", delay = "Запізнення, тижнів", miss_pct = "Пропущено, %", loss = "Сумарна помилка (хибні сигнали + запізнення)",
  allowed = "У допустимому діапазоні", chosen = "Обрано", w_own = "Вага власного порогу", floor = "Нижня межа за стабільністю",
  beta = "Показник степеня β", beta_se = "Похибка β", k_low = "k у слабкі тижні еталона", k_mid = "k у середні тижні", k_high = "k у сильні тижні еталона",
  k_ratio = "k сильні / k слабкі", z_ratio = "z різниці k", beta_z = "z (β ≠ 1)", waves = "Хвиль знайдено", base_mw = "Епідемічний поріг (багато хвиль)",
  pct_base_mw = "Зміна епідемічного порогу, %", pct_veryhigh_mw = "Зміна дуже високого порогу, %", veryhigh_mw = "Дуже високий поріг (багато хвиль)",
  seasons_used = "Сезони в розрахунку", angle_own = "Кут нахилу (власний)", own_usable = "Власний поріг придатний", angle_ref = "Кут нахилу (еталон)", angle_txt = "Кут нахилу", loss_mean = "Середня сумарна помилка", n = "Спроб", pct_failed = "Поріг не вдалось побудувати, %", cases_week = "Випадків на тиждень", kind = "Вид")

# ---------------------------------------------------------------- налаштування
mo_def <- function(x, d) if (is.null(x) || (length(x) == 1 && is.na(x))) d else x

mo_is_abs <- function(p) grepl("^([A-Za-z]:|/)", p)

mo_load_config <- function(path) {
  cfg <- fromJSON(path, simplifyVector = TRUE)
  cfg$.dir <- dirname(normalizePath(path, winslash = "/"))
  cfg$.path <- function(p) if (mo_is_abs(p)) p else file.path(cfg$.dir, p)
  cfg$.find <- function(f) {
    if (mo_is_abs(f) && file.exists(f)) return(f)
    sd <- cfg$paths$series_dir; if (!is.null(sd)) sd <- cfg$.path(sd)
    for (b in c(sd, cfg$.dir, file.path(cfg$.dir, "input"))) {
      if (is.null(b)) next
      p <- file.path(b, f)
      if (file.exists(p)) return(p)
    }
    stop("Не знайдено файл з рядами: ", f, ". Перевірте paths.series_dir у config.json і колонку file у переліку територій.")
  }
  a <- cfg$angle
  a$truth_angle <- mo_def(a$truth_angle, 2.8)
  a$criterion <- mo_def(a$criterion, "loss")
  if (is.null(a$weights)) a$weights <- list(false_alert = 1, delay = 1)
  a$stability_drop <- mo_def(a$stability_drop, 0.2)
  a$default <- mo_def(a$default, 2.8)
  a$min_seasons_goodness <- mo_def(a$min_seasons_goodness, 5)
  cfg$angle <- a
  cfg$season$max_seasons <- mo_def(cfg$season$max_seasons, 10)
  sa <- cfg$small_areas
  if (is.null(sa)) sa <- list()
  sa$angle <- mo_def(sa$angle, "auto")
  sa$method <- mo_def(sa$method, "mixture")
  sa$angle_rule <- mo_def(sa$angle_rule, "base_below_medium")
  if (is.null(sa$groups)) sa$groups <- list(reliable_min = 14, limited_min = 6)
  cfg$small_areas <- sa
  sc <- cfg$start_cleaning
  if (is.null(sc)) sc <- list(enabled = FALSE)
  sc$enabled <- isTRUE(sc$enabled)
  sc$apply_to <- mo_def(sc$apply_to, "small")
  sc$mad_multiplier <- mo_def(sc$mad_multiplier, 2)
  sc$compare_weeks <- mo_def(sc$compare_weeks, 8)
  cfg$start_cleaning <- sc
  cfg$season_subsets$min_seasons <- mo_def(cfg$season_subsets$min_seasons, 5)
  cfg$validation$hidden_season_check <- mo_def(cfg$validation$hidden_season_check, TRUE)
  cfg$diagnostics$proportionality <- mo_def(cfg$diagnostics$proportionality, TRUE)
  cfg$diagnostics$multiple_waves <- mo_def(cfg$diagnostics$multiple_waves, TRUE)
  cfg$export$dashboard_dim_thresholds <- isTRUE(cfg$export$dashboard_dim_thresholds)
  cfg$paths$workbook_name <- mo_def(cfg$paths$workbook_name, "memopt_results.xlsx")
  cfg$paths$output_dir <- mo_def(cfg$paths$output_dir, "output")
  cfg$parallel$workers <- mo_def(cfg$parallel$workers, 1)
  cfg
}

mo_weeks <- function(cfg) c(cfg$season$start_week:52, 1:cfg$season$end_week)

# Паралельне відображення: якщо створено кластер MO_CL, використовується він
mo_pmap <- function(items, fun, ...) {
  cl <- get0("MO_CL", envir = .GlobalEnv, ifnotfound = NULL)
  if (is.null(cl)) lapply(items, fun, ...) else parallel::parLapply(cl, items, fun, ...)
}

# ---------------------------------------------------------------- читання рядів
# Єдиний формат для всіх територій: аркуш Excel, перша колонка номери тижнів,
# решта колонок сезони (назви на кшталт 2024/2025), значення показника за тиждень.
mo_read_raw <- function(row, cfg) {
  weeks <- mo_weeks(cfg)
  f <- cfg$.find(row$file)
  x <- as.data.frame(read_excel(f, sheet = row$sheet, col_types = "numeric", .name_repair = "minimal"))
  rn <- x[[1]]
  x <- x[, -1, drop = FALSE]
  rownames(x) <- rn
  x <- x[, !(names(x) %in% cfg$placeholder_seasons), drop = FALSE]
  x <- x[match(weeks, rn), , drop = FALSE]
  rownames(x) <- weeks
  x
}

# Очищення початку сезону: початкові тижні прибираються, поки показник перевищує
# медіану наступних look тижнів більш ніж на k MAD (MAD у масштабі стандартного відхилення).
mo_censor_start <- function(y, k, look) {
  y <- as.numeric(y)
  weeks_cens <- integer(0)
  repeat {
    idx <- which(!is.na(y))
    if (length(idx) < look + 2) break
    i <- idx[1]
    ref <- y[idx[2:(look + 1)]]
    if (y[i] > median(ref) + k * mad(ref)) {
      weeks_cens <- c(weeks_cens, i)
      y[i] <- NA
    } else break
  }
  list(y = y, idx = weeks_cens)
}

# Повний підготовлений ряд території: виключені сезони, свята, очищення початку, не більше max_seasons останніх сезонів.
mo_series <- function(row, cfg, log = NULL, clean = TRUE, multiplier = NULL, compare_weeks = NULL) {
  d <- mo_read_raw(row, cfg)
  d <- d[, !(names(d) %in% names(cfg$exclude_seasons)), drop = FALSE]
  d <- d[, colSums(!is.na(d)) > 0, drop = FALSE]
  d <- d[, order(names(d)), drop = FALSE]
  if (NCOL(d) > cfg$season$max_seasons) d <- d[, tail(names(d), cfg$season$max_seasons), drop = FALSE]
  d[as.character(cfg$season$holiday_weeks), ] <- NA
  cs <- cfg$start_cleaning
  k <- if (is.null(multiplier)) cs$mad_multiplier else multiplier
  cw <- if (is.null(compare_weeks)) cs$compare_weeks else compare_weeks
  if (clean && isTRUE(cs$enabled) && (cs$apply_to == "all" || row$kind == "small")) {
    for (s in names(d)) {
      r <- mo_censor_start(d[, s], k, cw)
      if (length(r$idx) && !is.null(log)) {
        log$rows[[length(log$rows) + 1]] <- data.frame(id = row$id, name = row$name, season = s, weeks_censored = length(r$idx),
          weeks = paste(rownames(d)[r$idx], collapse = ", "), stringsAsFactors = FALSE)
      }
      d[, s] <- r$y
    }
  }
  d
}

# ---------------------------------------------------------------- модель MEM
mo_thr_model <- function(d, angle, n.max = -1) {
  d <- d[, colSums(!is.na(d)) > 0, drop = FALSE]
  if (NCOL(d) < 2) return(NULL)
  tryCatch(suppressWarnings(memmodel(d, i.seasons = NA, i.type.threshold = 5, i.tails.threshold = 1, i.type.intensity = 6,
        i.level.intensity = c(0.4, 0.9, 0.975), i.tails.intensity = 1, i.type.curve = 2, i.level.curve = 0.95,
        i.type.other = 3, i.level.other = 0.95, i.method = 2, i.param = angle, i.centering = -1, i.n.max = n.max)),
        error = function(e) NULL)
}

mo_thr_vec <- function(m) {
  if (is.null(m)) return(c(base = NA, medium = NA, high = NA, veryhigh = NA))
  c(base = unname(m$pre.post.intervals[1, 3]), medium = unname(m$epi.intervals[1, 4]),
    high = unname(m$epi.intervals[2, 4]), veryhigh = unname(m$epi.intervals[3, 4]))
}

mo_good_vec <- function(d, angle, min.seasons = 5) {
  na <- c(sens = NA, spec = NA, ppv = NA, npv = NA, mcc = NA, youden = NA)
  g <- tryCatch(suppressWarnings(memgoodness(d, i.seasons = NA, i.param = angle, i.method = 2, i.goodness.method = "cross",
        i.graph = FALSE, i.min.seasons = min.seasons, i.output = tempdir())), error = function(e) NULL)
  if (is.null(g)) return(na)
  x <- g$results
  c(sens = unname(x["Sensitivity"]), spec = unname(x["Specificity"]), ppv = unname(x["Positive predictive value"]),
    npv = unname(x["Negative predictive value"]), mcc = unname(x["Matthews correlation coefficient"]), youden = unname(x["Youdens Index"]))
}

# ---------------------------------------------------------------- перевірка порогу на сезоні
mo_eval <- function(y, T, st, en) {
  st <- unname(st); en <- unname(en); T <- unname(T)
  ok <- !is.na(y)
  idx <- which(ok)
  fa <- sum(ok & seq_along(y) < st & y > T)
  ine <- idx[idx >= st & idx <= en & y[idx] > T]
  c(fa = fa, delay = if (length(ine)) ine[1] - st else NA, miss = as.numeric(length(ine) == 0))
}

# Початок і кінець епідемічного періоду сезону за memtiming (для "істини" береться фіксований кут truth)
mo_timing <- function(y, angle) {
  tm <- tryCatch(memtiming(data.frame(y), i.method = 2, i.param = angle), error = function(e) NULL)
  if (is.null(tm)) return(NULL)
  c(start = tm$optimum.map[4], end = tm$optimum.map[5])
}

# Результат порогу T на одному сезоні: хибні сигнали, запізнення (якщо поріг не перевищено, запізнення дорівнює тривалості епідемічного періоду), пропуск
mo_season_result <- function(y, T, tm) {
  len <- unname(tm[2] - tm[1] + 1)
  if (is.na(T)) return(c(failed = 1, fa = 0, delay = len, miss = 1, len = len))
  e <- mo_eval(y, T, tm[1], tm[2])
  c(failed = 0, fa = unname(e["fa"]), delay = if (e["miss"] == 1) len else unname(e["delay"]), miss = unname(e["miss"]), len = len)
}

# Перевірка "поріг з решти сезонів на відкладеному сезоні" для заданого кута побудови порогу.
# eval_d (необов'язково): ряди для оцінки схованого сезону (наприклад, без очищення початку).
mo_loso_eval <- function(d, angle, truth, tms = NULL, eval_d = NULL) {
  S <- names(d); out <- list()
  ed <- if (is.null(eval_d)) d else eval_d
  for (s in S) {
    tm <- if (!is.null(tms)) tms[[s]] else {
      y0 <- ed[[s]]; if (sum(!is.na(y0)) >= 10) mo_timing(y0, truth) else NULL
    }
    if (is.null(tm) || any(is.na(tm))) next
    T <- unname(mo_thr_vec(mo_thr_model(d[, setdiff(S, s), drop = FALSE], angle))["base"])
    out[[length(out) + 1]] <- data.frame(season = s, as.list(mo_season_result(ed[[s]], T, tm)))
  }
  if (length(out)) do.call(rbind, out) else NULL
}

mo_loss <- function(ev, cfg) {
  if (is.null(ev) || !NROW(ev)) return(NA_real_)
  cfg$angle$weights$false_alert * mean(ev$fa) + cfg$angle$weights$delay * mean(ev$delay)
}

# ---------------------------------------------------------------- вибір кута
mo_angle_grid <- function(cfg) seq(cfg$angle$min, cfg$angle$max, by = cfg$angle$step)

# Найнижчий кут, до якого можна опускатись від стандартного кута (2,8) вниз: базовий рівень
# будується і на кроці вниз не падає більш ніж на drop відносно попереднього кроку.
mo_stable_floor <- function(scan, drop, start) {
  s <- scan[order(-scan$angle), ]
  s <- s[s$angle <= start + 1e-9, ]
  if (!nrow(s) || is.na(s$base[1])) return(NA_real_)
  low <- s$angle[1]
  for (i in seq_len(nrow(s))[-1]) {
    if (is.na(s$base[i]) || s$base[i] < (1 - drop) * s$base[i - 1]) break
    low <- s$angle[i]
  }
  low
}

# Крок 1: для кожного кута пороги, якість (перехресна перевірка memgoodness) і результати перевірки на відкладених сезонах
mo_scan_angles <- function(d, cfg) {
  grid <- mo_angle_grid(cfg); truth <- cfg$angle$truth_angle
  S <- names(d)
  tms <- lapply(S, function(s) { y <- d[[s]]; if (sum(!is.na(y)) >= 10) mo_timing(y, truth) else NULL }); names(tms) <- S
  rows <- lapply(grid, function(p) {
    tv <- mo_thr_vec(mo_thr_model(d, p))
    gv <- mo_good_vec(d, p, cfg$angle$min_seasons_goodness)
    ev <- mo_loso_eval(d, p, truth, tms)
    data.frame(angle = p, as.list(tv), as.list(gv), fa = if (is.null(ev)) NA else mean(ev$fa), delay = if (is.null(ev)) NA else mean(ev$delay),
               miss_pct = if (is.null(ev)) NA else 100 * mean(ev$miss), loss = mo_loss(ev, cfg))
  })
  scan <- do.call(rbind, rows)
  scan$balance <- pmin(scan$sens, scan$spec)
  scan
}

# Крок 2: вибір кута зі скану за правилами (межа стабільності, MCC більше 0, базовий рівень нижчий за середній, мінімум сумарної помилки)
mo_select_angle <- function(scan, cfg, drop = NULL) {
  if (is.null(drop)) drop <- cfg$angle$stability_drop
  floor_a <- mo_stable_floor(scan, drop, cfg$angle$default)
  if (is.na(floor_a)) floor_a <- cfg$angle$min
  scan$allowed <- scan$angle >= floor_a & !is.na(scan$base) & !is.na(scan$medium) & scan$base < scan$medium
  ok <- scan$allowed & !is.na(scan$mcc) & scan$mcc > 0
  note <- NULL; ang <- cfg$angle$default
  if (cfg$angle$criterion == "balance") {
    ok <- ok & !is.na(scan$balance)
    if (any(ok)) { best <- max(scan$balance[ok]); ang <- max(scan$angle[ok & scan$balance >= best - 1e-9]); note <- "максимум меншого з чутливості та специфічності при MCC більше 0" }
  } else {
    ok <- ok & !is.na(scan$loss)
    if (any(ok)) { best <- min(scan$loss[ok]); ang <- max(scan$angle[ok & scan$loss <= best + 1e-9]); note <- "мінімум сумарної помилки (хибні сигнали + запізнення) при MCC більше 0" }
  }
  if (is.null(note)) note <- "немає допустимого кута, взято стандартний"
  scan$chosen <- scan$angle == ang
  list(angle = ang, scan = scan, floor = floor_a, note = note)
}

mo_choose_angle <- function(d, cfg) mo_select_angle(mo_scan_angles(d, cfg), cfg)

# Кут власного порогу малої території: найвищий кут не вище стелі (кута групи), при якому епідемічний поріг
# нижчий за середній. Вищий кут скорочує епідемічний період, і найвищі доепідемічні значення піднімають епідемічний поріг
# над середнім; тоді власний поріг не має сенсу. NA, якщо такого кута немає.
mo_pick_angle_below <- function(d, ceiling_angle, cfg) {
  grid <- mo_angle_grid(cfg)
  for (a in rev(grid[grid <= ceiling_angle + 1e-9])) {
    tv <- mo_thr_vec(mo_thr_model(d, a))
    if (!is.na(tv["base"]) && !is.na(tv["medium"]) && tv["base"] < tv["medium"]) return(a)
  }
  NA_real_
}

# ---------------------------------------------------------------- вплив сезонів і стабільність
mo_influence <- function(d, angle) {
  S <- names(d)
  full <- mo_thr_vec(mo_thr_model(d, angle))
  loo <- t(sapply(S, function(s) mo_thr_vec(mo_thr_model(d[, setdiff(S, s), drop = FALSE], angle))))
  n <- length(S)
  se <- sqrt((n - 1) / n * sum((loo[, "base"] - mean(loo[, "base"], na.rm = TRUE))^2, na.rm = TRUE))
  infl <- abs(full["base"] - loo[, "base"]) / se
  data.frame(season = S, base_without = loo[, "base"], pct_base = (loo[, "base"] / full["base"] - 1) * 100,
             pct_veryhigh = (loo[, "veryhigh"] / full["veryhigh"] - 1) * 100, influence = infl, cutoff = 2 / sqrt(n),
             flagged = infl > 2 / sqrt(n), row.names = NULL)
}

mo_stability <- function(d, angle, min_n = 5) {
  S <- names(d)
  if (length(S) < min_n) return(NULL)
  res <- list()
  for (k in min_n:length(S)) for (cmb in combn(S, k, simplify = FALSE)) {
    tv <- mo_thr_vec(mo_thr_model(d[, cmb, drop = FALSE], angle))
    res[[length(res) + 1]] <- data.frame(n = k, as.list(tv))
  }
  r <- do.call(rbind, res)
  data.frame(n_sets = nrow(r), base_min = min(r$base, na.rm = TRUE), base_p10 = quantile(r$base, .1, na.rm = TRUE), base_median = median(r$base, na.rm = TRUE),
             base_p90 = quantile(r$base, .9, na.rm = TRUE), base_max = max(r$base, na.rm = TRUE),
             veryhigh_min = min(r$veryhigh, na.rm = TRUE), veryhigh_median = median(r$veryhigh, na.rm = TRUE), veryhigh_max = max(r$veryhigh, na.rm = TRUE), row.names = NULL)
}

# Усе для однієї основної території: кут, пороги, вплив, стабільність
mo_territory <- function(item, cfg) {
  t <- item$row; d <- item$d
  ch <- mo_choose_angle(d, cfg)
  tv <- mo_thr_vec(mo_thr_model(d, ch$angle)); gv <- mo_good_vec(d, ch$angle, cfg$angle$min_seasons_goodness)
  inf <- mo_influence(d, ch$angle)
  st <- mo_stability(d, ch$angle, cfg$season_subsets$min_seasons)
  list(res = data.frame(id = t$id, name = t$name, level = t$level, seasons = NCOL(d), seasons_used = paste(names(d), collapse = ", "), angle = ch$angle, as.list(tv), as.list(gv), row.names = NULL),
       scan = data.frame(id = t$id, name = t$name, ch$scan, row.names = NULL),
       infl = data.frame(id = t$id, name = t$name, inf, row.names = NULL),
       stab = if (!is.null(st)) data.frame(id = t$id, name = t$name, st, row.names = NULL) else NULL, floor = ch$floor, note = ch$note)
}

# ---------------------------------------------------------------- малі території
mo_tier <- function(med_cases, cfg) {
  g <- cfg$small_areas$groups
  ifelse(med_cases >= g$reliable_min, "надійні дані", ifelse(med_cases >= g$limited_min, "обмежені дані", "ненадійні дані"))
}

# Часткове об'єднання (empirical Bayes) власного порогу малої території і порогу еталона з коефіцієнтом.
# Квадрат різниці логарифмів пояснюється сталою частиною tau2 (справжня відмінність малої території
# від пропорції до еталона) і шумом a / (випадків на тиждень). Вага власного порогу w = tau2 / (tau2 + a / cases).
mo_pool_fit <- function(lo, lr, cases, tau_min = 0.01) {
  ok <- is.finite(lo) & is.finite(lr) & is.finite(cases) & cases > 0
  if (sum(ok) < 8) return(list(tau2 = NA_real_, a = NA_real_, cstar = NA_real_))
  d2 <- (lo[ok] - lr[ok])^2; x <- 1 / cases[ok]
  cf <- coef(lm(d2 ~ x))
  tau2 <- max(cf[1], tau_min)
  a <- if (cf[1] < tau_min) max(sum(x * (d2 - tau_min)) / sum(x^2), 0) else max(cf[2], 0)
  list(tau2 = unname(tau2), a = unname(a), cstar = unname(a / tau2))
}

mo_pool_w <- function(fit, cases) if (is.na(fit$tau2)) rep(NA_real_, length(cases)) else fit$tau2 / (fit$tau2 + fit$a / pmax(cases, 1e-6))

mo_blend_log <- function(w, own, refk) {
  w <- ifelse(is.na(w), 0, w)
  out <- exp(w * log(own) + (1 - w) * log(refk))
  ifelse(is.finite(out), out, refk)
}

# Квантильне відображення: поріг еталона переноситься на малу територію через однаковий рівень у розподілі тижневих значень
mo_qmap <- function(thr_ref, rv, hv) {
  ok <- !is.na(rv) & !is.na(hv)
  if (sum(ok) < 10) return(rep(NA_real_, length(thr_ref)))
  Fd <- ecdf(rv[ok])
  vapply(thr_ref, function(t) if (is.na(t)) NA_real_ else as.numeric(quantile(hv[ok], min(Fd(t), 0.999), type = 7, names = FALSE)), numeric(1))
}

mo_k <- function(hv, rv, which = "all") {
  ok <- !is.na(hv) & !is.na(rv)
  if (which == "high") ok <- ok & rv >= median(rv[ok])
  sum(hv[ok]) / sum(rv[ok])
}

# Дані однієї малої території для всіх етапів (ряд, ряд еталона, випадки)
mo_hrom_prep <- function(h, ser, cfg, terr) {
  d <- ser[[h$id]]
  refid <- h$reference_id
  if (is.na(refid) || !(refid %in% names(ser))) stop("Для малої території ", h$id, " не вказано або не знайдено територію-еталон (колонка reference_id).")
  if (is.na(h$population)) stop("Для малої території ", h$id, " не вказано населення (колонка population).")
  dm <- d
  wn <- cfg$small_areas$groups_window_seasons
  if (!is.null(wn) && !is.na(wn) && NCOL(d) > wn) dm <- d[, tail(names(d), wn), drop = FALSE]
  list(h = h, d = d, refid = refid, refname = terr$name[match(refid, terr$id)], refd = ser[[refid]],
       med_cases = median(unlist(dm) * h$population / 1e5, na.rm = TRUE))
}

# Крок 1: результати власного порогу для кожного кута (для вибору кута за групою)
mo_hrom_angle_scan <- function(p, cfg) {
  d <- p$d; S <- names(d); truth <- cfg$angle$truth_angle
  tms <- lapply(S, function(s) { y <- d[[s]]; if (sum(!is.na(y)) >= 10) mo_timing(y, truth) else NULL }); names(tms) <- S
  do.call(rbind, lapply(mo_angle_grid(cfg), function(a) {
    ev <- mo_loso_eval(d, a, truth, tms)
    if (is.null(ev)) return(NULL)
    data.frame(id = p$h$id, angle = a, ev, row.names = NULL)
  }))
}

# Крок 2: пороги всіх способів на повних даних і на кожному відкладеному сезоні
mo_hrom_thresholds <- function(j, cfg) {
  p <- j$p; own_angle <- j$own_angle; ref_angle <- j$ref_angle
  d <- p$d; S <- names(d); truth <- cfg$angle$truth_angle; refd <- p$refd
  cache <- new.env()
  ref_thr <- function(drop_season = NULL) {
    key <- paste0("k_", paste(drop_season, collapse = "|"))
    if (!is.null(cache[[key]])) return(cache[[key]])
    dd <- refd
    if (!is.null(drop_season)) dd <- dd[, setdiff(names(dd), drop_season), drop = FALSE]
    v <- mo_thr_vec(mo_thr_model(dd, ref_angle)); cache[[key]] <- v; v
  }
  build <- function(tr, drop) {
    own <- mo_thr_vec(mo_thr_model(d[, tr, drop = FALSE], own_angle))
    rt <- ref_thr(drop)
    hv <- unlist(d[, tr, drop = FALSE]); rv <- unlist(refd[rownames(d), tr, drop = FALSE])
    k <- mo_k(hv, rv, "all"); kh <- mo_k(hv, rv, "high")
    cases <- median(unlist(d[, tr, drop = FALSE]) * p$h$population / 1e5, na.rm = TRUE)
    list(own = own, ref = rt, k = k, k_high = kh, refk = rt * k, refk_high = rt * kh, qmap = mo_qmap(rt, rv, hv), cases = cases)
  }
  full <- build(S, NULL)
  hold <- list()
  for (hs in S) {
    y <- d[[hs]]
    tm <- if (sum(!is.na(y)) >= 10) mo_timing(y, truth) else NULL
    if (is.null(tm) || any(is.na(tm))) next
    hold[[length(hold) + 1]] <- list(held = hs, tm = tm, th = build(setdiff(S, hs), hs))
  }
  list(p = p, full = full, hold = hold)
}

# Способи отримання порогу малої території з компонентів
mo_methods <- function(th, tier, w) {
  gm <- sqrt(th$own * th$refk)
  tiers_rule <- if (tier == "надійні дані") th$own else if (tier == "обмежені дані") gm else th$refk
  list("власний" = th$own, "еталон" = th$ref, "еталон з коефіцієнтом" = th$refk, "еталон з коефіцієнтом (сильні тижні)" = th$refk_high,
       "квантильне відображення" = th$qmap, "середнє геометричне" = gm, "правило груп" = tiers_rule,
       "часткове об'єднання" = mo_blend_log(w, th$own, th$refk))
}

mo_hrom_job <- function(j, cfg) mo_hrom_thresholds(j, cfg)

mo_small_areas <- function(cfg, terr, ser, tabs, log) {
  hs <- terr[terr$kind == "small", ]
  preps <- lapply(seq_len(nrow(hs)), function(i) mo_hrom_prep(hs[i, ], ser, cfg, terr))
  tier <- vapply(preps, function(p) mo_tier(p$med_cases, cfg), character(1))
  ang_map <- setNames(tabs$angle, tabs$id)
  cat("  3.1 кут за групами малих територій\n"); flush.console()
  scans <- mo_pmap(preps, mo_hrom_angle_scan, cfg)
  sc <- do.call(rbind, Map(function(s, tr) if (is.null(s)) NULL else data.frame(s, tier = tr), scans, tier))
  sc$loss <- cfg$angle$weights$false_alert * sc$fa + cfg$angle$weights$delay * sc$delay
  gsum <- function(df) df %>% group_by(angle) %>% summarise(n = n(), fa = mean(fa), delay = mean(delay), miss_pct = 100 * mean(miss), pct_failed = 100 * mean(failed),
                                                           loss = mean(loss), .groups = "drop")
  g_all <- gsum(sc)
  pick <- function(g) { b <- min(g$loss); max(g$angle[g$loss <= b + 1e-9]) }
  fixed_angle <- is.numeric(cfg$small_areas$angle)
  group_scan <- list(); angle_by_tier <- c()
  for (tr in unique(tier)) {
    g <- if (sum(tier == tr) >= 5) gsum(sc[sc$tier == tr, ]) else g_all
    group_scan[[tr]] <- data.frame(tier = tr, g)
    angle_by_tier[tr] <- if (fixed_angle) cfg$small_areas$angle else pick(g)
  }
  group_scan <- do.call(rbind, group_scan)
  group_scan$chosen <- mapply(function(t, a) a == angle_by_tier[[t]], group_scan$tier, group_scan$angle)
  cat("  кути за групами:", paste(names(angle_by_tier), angle_by_tier, sep = " = ", collapse = "; "), "\n"); flush.console()

  cat("  3.2 пороги всіх способів і перевірка на відкладеному сезоні\n"); flush.console()
  # Кут власного порогу кожної громади: не вище кута групи і такий, при якому епідемічний поріг нижчий за середній
  # (та сама умова допустимості, що для основних територій). Якщо такого кута немає, власний поріг непридатний (вага 0).
  rule <- isTRUE(cfg$small_areas$angle_rule == "base_below_medium")
  own_angle <- mapply(function(p, tr) if (rule) mo_pick_angle_below(p$d, angle_by_tier[[tr]], cfg) else angle_by_tier[[tr]], preps, tier)
  usable <- !is.na(own_angle)
  own_angle_run <- ifelse(usable, own_angle, angle_by_tier[tier])
  cat("  кут за умовою: знижено для", sum(usable & abs(own_angle_run - angle_by_tier[tier]) > 1e-9), "громад, власний поріг непридатний у", sum(!usable), "\n"); flush.console()
  jobs <- Map(function(p, a) list(p = p, own_angle = a, ref_angle = ang_map[[p$refid]]), preps, own_angle_run)
  res <- mo_pmap(jobs, mo_hrom_job, cfg)

  fitfun <- function(ths, cases, use = rep(TRUE, length(ths))) {
    lo <- log(vapply(ths, function(t) t$own[1], numeric(1))); lr <- log(vapply(ths, function(t) t$refk[1], numeric(1)))
    mo_pool_fit(lo[use], lr[use], cases[use])
  }
  full_fit <- fitfun(lapply(res, function(r) r$full), vapply(res, function(r) r$full$cases, numeric(1)), usable)
  seasons_all <- unique(unlist(lapply(res, function(r) vapply(r$hold, function(x) x$held, character(1)))))
  hold_fit <- lapply(seasons_all, function(s) {
    ths <- list(); use <- logical(0)
    for (i in seq_along(res)) for (x in res[[i]]$hold) if (x$held == s) { ths[[length(ths) + 1]] <- x$th; use <- c(use, usable[i]) }
    fitfun(ths, vapply(ths, function(t) t$cases, numeric(1)), use)
  }); names(hold_fit) <- seasons_all

  use_mix <- cfg$small_areas$method == "mixture"
  main <- list(); qual <- list(); hold <- list(); wtab <- list()
  for (i in seq_along(res)) {
    r <- res[[i]]; p <- r$p; h <- p$h; th <- r$full
    w <- if (usable[i]) mo_pool_w(full_fit, th$cases) else 0
    meth <- mo_methods(th, tier[i], w)
    sel <- if (use_mix) meth[["часткове об'єднання"]] else meth[["правило груп"]]
    fixed <- FALSE
    if (!is.na(sel[1]) && !is.na(sel[2]) && sel[1] >= sel[2]) { sel[1] <- sel[2] / 1.05; fixed <- TRUE }
    how <- if (use_mix) paste0("часткове об'єднання: вага власного порогу ", round(w, 2), ", решта за еталоном (", p$refname, ") з коефіцієнтом")
           else if (tier[i] == "надійні дані") "власні пороги"
           else if (tier[i] == "обмежені дані") paste0("середнє геометричне власного порогу і порогу еталона (", p$refname, ") з коефіцієнтом")
           else paste0("еталон (", p$refname, ") з коефіцієнтом: власний поріг ненадійний")
    if (!usable[i]) how <- paste0("власний поріг непридатний: епідемічний поріг не нижчий за середній при жодному куті від ", cfg$angle$min, " до ", angle_by_tier[[tier[i]]], ", узято еталон (", p$refname, ") з коефіцієнтом")
    if (fixed) how <- paste0(how, "; епідемічний поріг знижено до 95% середнього порогу, бо перевищував його")
    rk <- th$refk
    main[[i]] <- data.frame(id = h$id, name = h$name, group = h$group, type = h$type, population = h$population, seasons_used = paste(names(p$d), collapse = ", "), angle_own = own_angle_run[i], own_usable = usable[i], angle_ref = ang_map[[p$refid]],
      median_cases_week = p$med_cases, tier = tier[i], w_own = w, reference = p$refname, k = th$k,
      rec_base = sel[1], rec_medium = sel[2], rec_high = sel[3], rec_veryhigh = sel[4], how = how,
      own_base = th$own[1], own_medium = th$own[2], own_high = th$own[3], own_veryhigh = th$own[4],
      ref_k_base = rk[1], ref_k_medium = rk[2], ref_k_high = rk[3], ref_k_veryhigh = rk[4], row.names = NULL)
    wtab[[i]] <- data.frame(id = h$id, name = h$name, tier = tier[i], cases_week = p$med_cases, w_own = w, row.names = NULL)
    d <- p$d; S <- names(d); refd <- p$refd
    r_s <- c(); fa <- c(); dl <- c(); ms <- c()
    for (s in S) {
      y <- d[, s]; z <- refd[rownames(d), s]; ok <- !is.na(y) & !is.na(z)
      r_s <- c(r_s, if (sum(ok) >= 8) cor(y[ok], z[ok]) else NA)
      tm <- if (sum(!is.na(y)) >= 10) mo_timing(y, cfg$angle$truth_angle) else NULL
      if (is.null(tm) || any(is.na(tm))) { fa <- c(fa, NA); dl <- c(dl, NA); ms <- c(ms, NA); next }
      e <- mo_eval(y, th$own[1], tm[1], tm[2]); fa <- c(fa, e["fa"]); dl <- c(dl, e["delay"]); ms <- c(ms, e["miss"])
    }
    qual[[i]] <- data.frame(id = h$id, name = h$name, r_min = suppressWarnings(min(r_s, na.rm = TRUE)), r_mean = mean(r_s, na.rm = TRUE),
      fa_total = sum(fa, na.rm = TRUE), delay_mean = mean(dl, na.rm = TRUE), missed = sum(ms, na.rm = TRUE),
      seasons_ok = sum(fa == 0 & ms == 0 & (is.na(dl) | dl <= 2), na.rm = TRUE), row.names = NULL)
    if (isTRUE(cfg$validation$hidden_season_check)) for (x in r$hold) {
      wh <- if (usable[i]) mo_pool_w(hold_fit[[x$held]], x$th$cases) else 0
      ms_ <- mo_methods(x$th, tier[i], wh)
      y <- p$d[[x$held]]
      for (m in names(ms_)) {
        res_ <- mo_season_result(y, unname(ms_[[m]][1]), x$tm)
        hold[[length(hold) + 1]] <- data.frame(id = h$id, tier = tier[i], held = x$held, method = m, as.list(res_), row.names = NULL)
      }
    }
  }
  list(main = do.call(rbind, main), quality = do.call(rbind, qual), holdout = if (length(hold)) do.call(rbind, hold) else NULL,
       group_scan = group_scan, angle_by_tier = angle_by_tier, pool = full_fit, weights = do.call(rbind, wtab))
}

# ---------------------------------------------------------------- діагностики
# Перевірка пропорційності: чи сталий коефіцієнт мала територія / еталон на слабких і сильних тижнях еталона
mo_proportionality <- function(cfg, terr, ser) {
  hs <- terr[terr$kind == "small", ]
  rows <- lapply(seq_len(nrow(hs)), function(i) {
    h <- hs[i, ]; d <- ser[[h$id]]
    refd <- ser[[h$reference_id]]
    hv <- unlist(d); rv <- unlist(refd[rownames(d), names(d), drop = FALSE])
    ok <- !is.na(hv) & !is.na(rv) & rv > 0
    hv <- hv[ok]; rv <- rv[ok]
    y <- hv * h$population / 1e5
    off <- rep(log(h$population / 1e5), length(y)); lrv <- log(rv)
    fit <- tryCatch(suppressWarnings(glm(y ~ offset(off) + lrv, family = quasipoisson)), error = function(e) NULL)
    beta <- se <- NA
    if (!is.null(fit)) { cf <- summary(fit)$coefficients; beta <- cf[2, 1]; se <- cf[2, 2] }
    tt <- cut(rv, quantile(rv, c(0, 1/3, 2/3, 1)), include.lowest = TRUE, labels = c("low", "mid", "high"))
    kk <- sapply(c("low", "mid", "high"), function(l) sum(hv[tt == l]) / sum(rv[tt == l]))
    cl <- sum(hv[tt == "low"]) * h$population / 1e5; ch <- sum(hv[tt == "high"]) * h$population / 1e5
    se_lr <- sqrt(1 / max(cl, 1) + 1 / max(ch, 1))
    data.frame(id = h$id, name = h$name, median_cases_week = median(unlist(d) * h$population / 1e5, na.rm = TRUE), beta = beta, beta_se = se,
               beta_z = (beta - 1) / se, k_low = kk[["low"]], k_mid = kk[["mid"]], k_high = kk[["high"]], k_ratio = kk[["high"]] / kk[["low"]],
               z_ratio = log(kk[["high"]] / kk[["low"]]) / se_lr, row.names = NULL)
  })
  do.call(rbind, rows)
}

# Діагностика "багато хвиль": пороги за автоматичним поділом ряду на хвилі і без нього
mo_multiwave_one <- function(item, cfg) {
  d <- item$d; ang <- item$angle
  base <- mo_thr_vec(mo_thr_model(d, ang))
  mw <- tryCatch(suppressWarnings(suppressMessages(mem:::transformseries.multiple(d, i.output = tempdir(), i.prefix = "mw"))), error = function(e) NULL)
  if (is.null(mw)) return(data.frame(id = item$id, name = item$name, level = item$level, seasons = NCOL(d), waves = NA, base = base[1], base_mw = NA, pct_base_mw = NA,
                                    veryhigh = base[4], veryhigh_mw = NA, pct_veryhigh_mw = NA, row.names = NULL))
  tv <- mo_thr_vec(mo_thr_model(as.data.frame(mw$data.final), ang))
  data.frame(id = item$id, name = item$name, level = item$level, seasons = NCOL(d), waves = NCOL(mw$data.final), base = base[1], base_mw = tv[1],
             pct_base_mw = (tv[1] / base[1] - 1) * 100, veryhigh = base[4], veryhigh_mw = tv[4], pct_veryhigh_mw = (tv[4] / base[4] - 1) * 100, row.names = NULL)
}

# ---------------------------------------------------------------- заповнення пропусків
# Пропуски всередині сезону заповнюються так само, як у пакеті mem і застосунку memapp (функція fill.missing, регресія згладжування loess).
# Початкові і кінцеві пропуски не заповнюються. Результат потрібен для правил, що порівнюють тижневі значення території-еталона з її порогом.
mo_fill_missing_matrix <- function(d) as.data.frame(apply(d, 2, function(v) mem:::fill.missing(v)), row.names = rownames(d), check.names = FALSE)

mo_export_filled <- function(cfg, terr, out_file) {
  ids <- unique(c(terr$id[terr$kind == "main"], terr$reference_id[terr$kind == "small" & !is.na(terr$reference_id)]))
  rows <- list()
  for (id in ids) {
    r <- terr[terr$id == id, ]
    raw <- mo_read_raw(r, cfg)
    raw <- raw[, !(names(raw) %in% names(cfg$exclude_seasons)), drop = FALSE]
    fl <- mo_fill_missing_matrix(raw)
    for (s in names(raw)) rows[[length(rows) + 1]] <- data.frame(id = id, season = s, week = rownames(raw), rate_raw = raw[[s]], rate_filled = fl[[s]])
  }
  write.csv(do.call(rbind, rows), out_file, row.names = FALSE, fileEncoding = "UTF-8")
}

# ---------------------------------------------------------------- основний запуск
mo_summ <- function(g, cfg) {
  g %>% summarise(trials = n(), pct_threshold_failed = 100 * mean(failed),
    false_alerts_per_season = mean(fa), pct_seasons_with_false_alert = 100 * mean(fa[failed == 0] > 0),
    mean_delay_weeks = mean(delay[miss == 0], na.rm = TRUE), pct_missed_incl_failed = 100 * mean(miss),
    pct_without_problems = 100 * mean(failed == 0 & fa == 0 & miss == 0 & delay <= 2),
    loss_mean = mean(cfg$angle$weights$false_alert * fa + cfg$angle$weights$delay * delay), .groups = "drop")
}

mo_read_territories <- function(cfg) {
  terr <- read.csv(cfg$.path(cfg$paths$territories), stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"))
  if (names(terr)[1] != "id") names(terr)[1] <- "id"
  need <- c("id", "name", "kind", "file", "sheet")
  miss <- setdiff(need, names(terr))
  if (length(miss)) stop("У переліку територій немає обов'язкових колонок: ", paste(miss, collapse = ", "))
  for (nm in c("level", "population", "reference_id", "type", "group", "export_dim")) if (!(nm %in% names(terr))) terr[[nm]] <- NA
  terr$level <- ifelse(is.na(terr$level), terr$kind, terr$level)
  terr$population <- suppressWarnings(as.numeric(terr$population))
  terr
}

mo_start_cluster <- function(cfg) {
  nw <- cfg$parallel$workers
  if (nw > 1 && is.null(get0("MO_CL", envir = .GlobalEnv, ifnotfound = NULL))) {
    core <- get0("MO_CORE", envir = .GlobalEnv)
    cl <- parallel::makeCluster(nw)
    parallel::clusterExport(cl, "core", envir = environment())
    invisible(parallel::clusterEvalQ(cl, source(core, encoding = "UTF-8")))
    assign("MO_CL", cl, envir = .GlobalEnv)
    return(cl)
  }
  NULL
}

mo_run <- function(cfg_path) {
  cfg <- mo_load_config(cfg_path)
  out_dir <- cfg$.path(cfg$paths$output_dir)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  terr <- mo_read_territories(cfg)
  cl <- mo_start_cluster(cfg)
  if (!is.null(cl)) on.exit({ parallel::stopCluster(cl); rm("MO_CL", envir = .GlobalEnv) }, add = TRUE)
  has_small <- any(terr$kind == "small")
  log <- new.env(); log$rows <- list()
  cat("1. Читання і підготовка рядів\n")
  ser <- list()
  for (i in seq_len(nrow(terr))) ser[[terr$id[i]]] <- mo_series(terr[i, ], cfg, log)
  cens <- if (length(log$rows)) do.call(rbind, log$rows) else data.frame()

  cat("2. Основні території: кут, пороги, вплив сезонів, стабільність\n"); flush.console()
  series_terr <- terr[terr$kind == "main", ]
  items <- lapply(seq_len(nrow(series_terr)), function(i) list(row = series_terr[i, ], d = ser[[series_terr$id[i]]]))
  tr <- mo_pmap(items, mo_territory, cfg)
  tabs <- do.call(rbind, lapply(tr, `[[`, "res"))
  scans <- do.call(rbind, lapply(tr, `[[`, "scan"))
  infl <- do.call(rbind, lapply(tr, `[[`, "infl"))
  stab <- do.call(rbind, Filter(Negate(is.null), lapply(tr, `[[`, "stab")))
  floors <- data.frame(id = tabs$id, name = tabs$name, floor = vapply(tr, `[[`, numeric(1), "floor"), angle = tabs$angle)
  for (i in seq_len(nrow(tabs))) cat("  ", tabs$name[i], "кут", tabs$angle[i], "\n")

  hr <- NULL; prop <- NULL
  if (has_small) {
    cat("3. Малі території\n"); flush.console()
    hr <- mo_small_areas(cfg, terr, ser, tabs, log)
    if (isTRUE(cfg$diagnostics$proportionality)) prop <- mo_proportionality(cfg, terr, ser)
  }
  mw <- NULL
  if (isTRUE(cfg$diagnostics$multiple_waves)) {
    cat("4. Діагностика: багато хвиль\n"); flush.console()
    mwitems <- lapply(seq_len(nrow(series_terr)), function(i) list(id = series_terr$id[i], name = series_terr$name[i], level = series_terr$level[i],
                                                                   d = ser[[series_terr$id[i]]], angle = tabs$angle[i]))
    if (has_small) {
      rel <- hr$main[hr$main$tier == "надійні дані", ]
      if (NROW(rel)) mwitems <- c(mwitems, lapply(seq_len(nrow(rel)), function(i) list(id = rel$id[i], name = rel$name[i], level = terr$level[match(rel$id[i], terr$id)],
                                                                                       d = ser[[rel$id[i]]], angle = hr$angle_by_tier[["надійні дані"]])))
    }
    mw <- do.call(rbind, mo_pmap(mwitems, mo_multiwave_one, cfg))
  }

  cat("5. Запис результатів\n"); flush.console()
  holdsum <- NULL
  if (has_small && !is.null(hr$holdout)) {
    h <- hr$holdout
    holdsum <- h %>% group_by(tier, method) %>% mo_summ(cfg)
    holdall <- h %>% group_by(method) %>% mo_summ(cfg)
  }
  # універсальна таблиця порогів: усі території
  thr_main <- data.frame(id = tabs$id, name = tabs$name, level = tabs$level, kind = "основна", how = paste0("MEM, кут нахилу ", tabs$angle), seasons_used = tabs$seasons_used, angle_txt = as.character(tabs$angle),
                         base = tabs$base, medium = tabs$medium, high = tabs$high, veryhigh = tabs$veryhigh)
  thr_all <- thr_main
  if (has_small) thr_all <- rbind(thr_main, data.frame(id = hr$main$id, name = hr$main$name, level = terr$level[match(hr$main$id, terr$id)], kind = "мала",
                                                       how = hr$main$how, seasons_used = hr$main$seasons_used, angle_txt = paste0("власний ", hr$main$angle_own, "; еталон ", hr$main$angle_ref), base = hr$main$rec_base, medium = hr$main$rec_medium, high = hr$main$rec_high,
                                                       veryhigh = hr$main$rec_veryhigh))
  thr_all[, c("base", "medium", "high", "veryhigh")] <- round(thr_all[, c("base", "medium", "high", "veryhigh")], 2)
  write.csv(thr_all, file.path(out_dir, "memopt_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")

  # необов'язковий експорт під дашборд (формат input/Dim_Thresholds.xlsx ETL): лише території з export_dim = yes
  dim <- NULL
  if (isTRUE(cfg$export$dashboard_dim_thresholds)) {
    sel <- terr$id[!is.na(terr$export_dim) & tolower(terr$export_dim) == "yes"]
    d0 <- thr_all[thr_all$id %in% sel, ]
    d0 <- d0[match(sel[sel %in% d0$id], d0$id), ]
    dim <- data.frame(PCODE = d0$id, Name_in_Dataset = d0$name, Base_Threshold = d0$base, Medium_Threshold = d0$medium, High_Threshold = d0$high, VeryHigh_Threshold = d0$veryhigh)
    ex_dir <- file.path(out_dir, "dashboard_export"); dir.create(ex_dir, showWarnings = FALSE)
    write.xlsx(dim, file.path(ex_dir, "Dim_Thresholds.xlsx"), sheetName = "Dim_Thresholds", overwrite = TRUE)
    write.csv(dim, file.path(ex_dir, "Dim_Thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }

  hdr <- createStyle(textDecoration = "bold", fgFill = "#1F4E78", fontColour = "#FFFFFF", wrapText = TRUE, valign = "center")
  wb <- createWorkbook()
  add <- function(name, df, widths = NULL) {
    addWorksheet(wb, name)
    if (is.null(df) || !NROW(df)) { writeData(wb, name, "немає даних"); return(invisible()) }
    num <- sapply(df, is.numeric); df[num] <- lapply(df[num], function(v) round(v, 2))
    nm <- names(df); hit <- nm %in% names(MO_LABELS); nm[hit] <- unname(MO_LABELS[nm[hit]]); names(df) <- nm
    writeData(wb, name, df, headerStyle = hdr)
    setColWidths(wb, name, cols = seq_len(ncol(df)), widths = if (is.null(widths)) "auto" else widths)
    freezePane(wb, name, firstRow = TRUE, firstCol = TRUE)
  }
  S <- MO_SHEETS
  add(S$thresholds, thr_all)
  add(S$main, tabs)
  if (has_small) add(S$small, hr$main)
  if (has_small) add(S$quality, hr$quality)
  add(S$angle, scans)
  add(S$floor, floors)
  if (has_small) add(S$group_angle, hr$group_scan)
  add(S$influence, infl)
  add(S$stability, stab)
  add(S$cleaning, cens)
  if (!is.null(holdsum)) { add(S$hold_groups, as.data.frame(holdsum)); add(S$hold_all, as.data.frame(holdall)) }
  if (has_small) add(S$weights, hr$weights)
  if (!is.null(prop)) add(S$proportionality, prop)
  if (!is.null(mw)) add(S$waves, mw)
  if (!is.null(dim)) add(S$dim, dim)
  cfgdf <- data.frame(параметр = c("область, сезон", "виключені сезони", "кут: мін, макс, крок (діапазон Vega 2013: 2-4)", "критерій кута", "ваги: хибний сигнал, тиждень запізнення",
                                   "кут для визначення справжнього початку епідемії", "максимальна межа падіння епідемічного порогу на кроці вниз", "кут для малих територій за групами",
                                   "кут власного порогу громади",
                                   "спосіб для малих територій", "очищення початку сезону", "групи за медіаною випадків (надійні від, обмежені від)",
                                   "максимум сезонів у розрахунку", "випуск"),
    значення = c(mo_def(cfg$project, ""),
                 paste(paste0(names(cfg$exclude_seasons), " (", unlist(cfg$exclude_seasons), ")"), collapse = "; "),
                 paste(cfg$angle$min, cfg$angle$max, cfg$angle$step, sep = ", "), cfg$angle$criterion,
                 paste(cfg$angle$weights$false_alert, cfg$angle$weights$delay, sep = " : "), cfg$angle$truth_angle, cfg$angle$stability_drop,
                 if (has_small) paste(names(hr$angle_by_tier), hr$angle_by_tier, sep = " = ", collapse = "; ") else "немає малих територій",
                 if (cfg$small_areas$angle_rule == "base_below_medium") "найвищий кут не вище кута групи, при якому епідемічний поріг нижчий за середній; інакше власний поріг непридатний (вага 0)" else "кут групи",
                 cfg$small_areas$method,
                 paste0(cfg$start_cleaning$enabled, ", множник MAD ", cfg$start_cleaning$mad_multiplier, ", порівняння з ", cfg$start_cleaning$compare_weeks, " наступними тижнями"),
                 paste(cfg$small_areas$groups$reliable_min, cfg$small_areas$groups$limited_min, sep = ", "), cfg$season$max_seasons,
                 paste0("memopt 0.3, ", format(Sys.time(), "%Y-%m-%d %H:%M"))))
  add(S$config, cfgdf, widths = c(55, 120))
  f <- file.path(out_dir, cfg$paths$workbook_name)
  saveWorkbook(wb, f, overwrite = TRUE)
  saveRDS(list(tabs = tabs, hr = hr, prop = prop, mw = mw, scans = scans, floors = floors, thr = thr_all), file.path(out_dir, "last_run.rds"))
  mo_export_filled(cfg, terr, file.path(out_dir, "reference_series_filled.csv"))
  cat("Готово:", f, "\n")
  invisible(list(cfg = cfg, tabs = tabs, hromadas = hr, thr = thr_all))
}
