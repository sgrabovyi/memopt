# memopt: підбір порогів MEM (метод рухомої епідемії) для захворюваності на ГРВІ, версія 0.4
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
  cleaning = "Очищення початку", hold_groups = "Відкладені сезони за групами", hold_all = "Відкладені сезони усі разом", weights = "Ваги малих територій",
  proportionality = "Пропорційність", waves = "Багато хвиль", thresholds = "Пороги", dim = "Експорт Dim_Thresholds", config = "Конфігурація",
  season_pick = "Добір сезонів", changes = "Зміни порогів", input_check = "Перевірка даних")

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
  seasons_used = "Сезони в розрахунку", angle_own = "Кут нахилу (власний)", own_usable = "Власний поріг придатний", angle_ref = "Кут нахилу (еталон)", angle_txt = "Кут нахилу", loss_mean = "Середня сумарна помилка", n = "Спроб", pct_failed = "Поріг не вдалось побудувати, %", cases_week = "Випадків на тиждень", kind = "Вид",
  n_seasons = "Сезонів у наборі", eval_seasons = "Контрольні сезони", mcc_floor = "Найнижчий дозволений MCC",
  ctl_tp = "Тижні епідемії над порогом", ctl_fp = "Тижні поза епідемією над порогом", ctl_fn = "Тижні епідемії під порогом", ctl_tn = "Тижні поза епідемією під порогом", note = "Примітка", n_available = "Сезонів доступно",
  seasons_used_prev = "Сезони попереднього запуску", base_prev = "Попередній епідемічний поріг", medium_prev = "Попередній середній поріг",
  pct_medium = "Зміна середнього порогу, %", flag_limit = "Найбільший вплив одного сезону, %", mcc_ctl = "MCC на контрольних сезонах", fa_ctl_sum = "Хибних сигналів за контрольні сезони", delay_ctl_sum = "Запізнення за контрольні сезони, тижнів", loss_ctl_sum = "Сумарна помилка за контрольні сезони (хибні сигнали + запізнення)", truth_angle = "Кут для меж епідемії", noepi = "Сезони без епідемії (пік нижчий за епідемічний поріг)", infl_max_pct = "Найбільший вплив одного сезону, %", weeks_missing = "Пропущено тижнів усередині сезону", weeks_zero = "Нульових тижнів",
  peak_ratio = "Пік до звичного піку")

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
  a$min_seasons_goodness <- mo_def(a$min_seasons_goodness, 3)
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
  ss <- cfg$season_selection
  if (is.null(ss)) ss <- list()
  ss$enabled <- mo_def(ss$enabled, TRUE)
  ss$min_seasons <- mo_def(ss$min_seasons, 3)
  ss$era_start <- mo_def(ss$era_start, "2023/2024")
  ss$era_full <- mo_def(ss$era_full, 5)
  ss$not_validation <- mo_def(ss$not_validation, "2023/2024")
  ss$guard_weeks <- mo_def(ss$guard_weeks, 1)
  ss$recommended_min <- mo_def(ss$recommended_min, 5)
  cfg$season_selection <- ss
  # виключення сезону лише для однієї території (вади даних): {"код території": {"сезон": "причина"}}
  if (is.null(cfg$exclude_territory_seasons)) cfg$exclude_territory_seasons <- list()
  # набори сезонів, обрані попереднім запуском (output/season_sets.json): їх бачать ретроспектива, приклади і книга для memapp
  sf <- file.path(cfg$.path(cfg$paths$output_dir), "season_sets.json")
  cfg$.season_sets <- if (file.exists(sf)) fromJSON(sf, simplifyVector = FALSE) else NULL
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
  x <- x[, !(names(x) %in% c(cfg$placeholder_seasons, mo_unfinished(names(x), cfg))), drop = FALSE]
  out <- x[match(weeks, rn), , drop = FALSE]
  rownames(out) <- weeks
  hol <- cfg$season$holiday_weeks
  # Святкові тижні робляться порожніми тут, для кожного сезону за його календарем. Сезони вирівнюються за номером
  # тижня: рядок «3» у кожному сезоні це справжній 3-й тиждень, сезон завжди закінчується тижнем end_week.
  # «Провал» звернень припадає на останній тиждень року, 1-й і 2-й тижні. У сезоні, де рік має 53 тижні, останній
  # тиждень року це 53-й: порожніми робляться 53, 1 і 2, а 52-й іде в розрахунок зі справжніми даними. Сам 53-й тиждень
  # до таблиці для mem не потрапляє (він святковий), тому всі сезони мають однакові 33 рядки без зсуву.
  for (s in names(x)) {
    h <- if (mo_has_week53(s)) ifelse(hol == 52, 53, hol) else hol
    out[[s]][weeks %in% h] <- NA
  }
  out
}

# Сезон «РРРР/РРРР+1» має 53-й тиждень, якщо ISO-рік РРРР має 53 тижні (28 грудня припадає на 53-й тиждень)
mo_has_week53 <- function(s) {
  if (!grepl("^[0-9]{4}/[0-9]{4}$", s)) return(FALSE)
  format(as.Date(sprintf("%s-12-28", substr(s, 1, 4))), "%V") == "53"
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

# Сезони, які ще тривають за календарем: сезон "РРРР/РРРР+1" завершується в неділю тижня end_week (ISO) року РРРР+1.
# Незавершений сезон у пороги не береться, навіть якщо в рядах уже є його перші тижні (дата відліку cfg$season$as_of, інакше сьогодні).
mo_unfinished <- function(seasons, cfg) {
  today <- as.Date(mo_def(cfg$season$as_of, as.character(Sys.Date())))
  y2 <- ifelse(grepl("^[0-9]{4}/[0-9]{4}$", seasons), suppressWarnings(as.integer(substr(seasons, 6, 9))), NA_integer_)
  end <- vapply(y2, function(y) {
    if (is.na(y)) return(NA_real_)
    j4 <- as.Date(sprintf("%d-01-04", y)); mon1 <- j4 - (as.integer(format(j4, "%u")) - 1)
    as.numeric(mon1 + cfg$season$end_week * 7 - 1)
  }, numeric(1))
  seasons[!is.na(end) & as.numeric(today) <= end]
}

# Сезони, які програма відсікає завжди (користувач не може їх повернути, лише додатково виключити інші)
MO_AUTO_EXCLUDE <- c("2015/2016" = "пандемічний грип A(H1N1)", "2016/2017" = "пандемічний грип A(H1N1)",
  "2019/2020" = "початок пандемії COVID-19", "2020/2021" = "пандемія COVID-19",
  "2021/2022" = "хвиля омікрону", "2022/2023" = "продовження омікрону, масова міграція, збої медичної мережі")

mo_excluded <- function(cfg) c(names(MO_AUTO_EXCLUDE), names(cfg$exclude_seasons))

# Повний підготовлений ряд території: виключені сезони, свята, очищення початку, не більше max_seasons останніх сезонів.
mo_series <- function(row, cfg, log = NULL, clean = TRUE, multiplier = NULL, compare_weeks = NULL, all_seasons = FALSE) {
  d <- mo_read_raw(row, cfg)
  d <- d[, !(names(d) %in% mo_excluded(cfg)), drop = FALSE]
  d <- d[, !(names(d) %in% names(cfg$exclude_territory_seasons[[row$id]])), drop = FALSE]
  d <- d[, colSums(!is.na(d)) > 0, drop = FALSE]
  d <- d[, order(names(d)), drop = FALSE]
  if (NCOL(d) > cfg$season$max_seasons) d <- d[, tail(names(d), cfg$season$max_seasons), drop = FALSE]
  ssel <- if (all_seasons) NULL else cfg$.season_sets[[row$id]]
  if (!is.null(ssel)) d <- d[, names(d) %in% unlist(ssel), drop = FALSE]
  # святкові тижні вже порожні після mo_read_raw (з урахуванням сезонів з 53-м тижнем)
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
  # хибні сигнали: тижні вище порогу і до початку, і після кінця епідемічного періоду
  fa <- sum(ok & (seq_along(y) < st | seq_along(y) > en) & y > T)
  ine <- idx[idx >= st & idx <= en & y[idx] > T]
  inp <- ok & seq_along(y) >= st & seq_along(y) <= en
  c(fa = fa, delay = if (length(ine)) ine[1] - st else NA, miss = as.numeric(length(ine) == 0),
    tp = sum(inp & y > T), fn = sum(inp & y <= T), tn = sum(ok & !inp & y <= T))
}

# Початок і кінець епідемічного періоду сезону за memtiming (для "істини" береться фіксований кут truth)
mo_timing <- function(y, angle) {
  tm <- tryCatch(memtiming(data.frame(y), i.method = 2, i.param = angle), error = function(e) NULL)
  if (is.null(tm)) return(NULL)
  c(start = tm$optimum.map[4], end = tm$optimum.map[5])
}

# Межі епідемії сезону: memtiming стандартним кутом truth_angle (2,8), однаково для всіх територій і сезонів
mo_season_timing <- function(y, cfg) {
  if (sum(!is.na(y)) >= 10) mo_timing(y, cfg$angle$truth_angle) else NULL
}

# Результат порогу T на одному сезоні: хибні сигнали, запізнення (якщо поріг не перевищено, запізнення дорівнює тривалості епідемічного періоду), пропуск
mo_season_result <- function(y, T, tm, noepi = FALSE) {
  if (noepi) {
    ok <- !is.na(y)
    if (is.na(T)) return(c(failed = 1, fa = 0, delay = 0, miss = 0, len = 0, tp = 0, fn = 0, tn = NA))
    return(c(failed = 0, fa = sum(ok & y > T), delay = 0, miss = 0, len = 0, tp = 0, fn = 0, tn = sum(ok & y <= T)))
  }
  len <- unname(tm[2] - tm[1] + 1)
  if (is.na(T)) return(c(failed = 1, fa = 0, delay = len, miss = 1, len = len, tp = NA, fn = NA, tn = NA))
  e <- mo_eval(y, T, tm[1], tm[2])
  c(failed = 0, fa = unname(e["fa"]), delay = if (e["miss"] == 1) len else unname(e["delay"]), miss = unname(e["miss"]), len = len,
    tp = unname(e["tp"]), fn = unname(e["fn"]), tn = unname(e["tn"]))
}

# Сезони без епідемії для території: пік сезону не досягає епідемічного порогу (визначення MEM: нижче епідемічного
# порогу епідемії немає; між епідемічним і середнім порогом це епідемія низької інтенсивності). Поріг будується один раз
# стандартним кутом за всіма іншими доступними сезонами території, тому рішення не залежить від кута і набору сезонів,
# які перевіряються (інакше високий поріг сам оголошував би сезони неепідемічними і здавався б кращим).
mo_noepi_seasons <- function(d, cfg, seasons = names(d)) {
  a <- cfg$angle$truth_angle
  S <- names(d); out <- character(0)
  for (s in intersect(seasons, S)) {
    y <- d[[s]]; if (sum(!is.na(y)) < 10 || length(S) < 3) next
    T <- unname(mo_thr_vec(mo_thr_model(d[, setdiff(S, s), drop = FALSE], as.numeric(a)))["base"])
    if (!is.na(T) && max(y, na.rm = TRUE) < T) out <- c(out, s)
  }
  out
}

# Перевірка "поріг з решти сезонів на відкладеному сезоні" для заданого кута побудови порогу
mo_loso_eval <- function(d, angle, truth, tms = NULL, eval_seasons = NULL, noepi = character(0)) {
  S <- names(d); out <- list()
  ed <- d
  for (s in if (is.null(eval_seasons)) S else S[S %in% eval_seasons]) {
    tv <- mo_thr_vec(mo_thr_model(d[, setdiff(S, s), drop = FALSE], angle))
    T <- unname(tv["base"]); y <- ed[[s]]
    # Сезон без епідемії (mo_noepi_seasons): MEM і в такому сезоні знаходить "епідемічний період", тому запізнення
    # не рахується: увесь сезон неепідемічний, помилками є лише тижні над епідемічним порогом (хибні сигнали).
    if (s %in% noepi && !is.na(T)) {
      ok <- !is.na(y)
      out[[length(out) + 1]] <- data.frame(season = s, failed = 0, fa = sum(ok & y > T), delay = 0, miss = 0, len = 0,
                                           tp = 0, fn = 0, tn = sum(ok & y <= T), noepi = 1)
      next
    }
    tm <- if (!is.null(tms)) tms[[s]] else {
      if (sum(!is.na(y)) >= 10) mo_timing(y, truth) else NULL
    }
    if (is.null(tm) || any(is.na(tm))) next
    out[[length(out) + 1]] <- data.frame(season = s, as.list(mo_season_result(y, T, tm)), noepi = 0)
  }
  if (length(out)) do.call(rbind, out) else NULL
}

# MCC за тижнями: епідемія (у межах епідемічного періоду) проти "вище порогу"
mo_mcc <- function(tp, fp, fn, tn) {
  den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  if (is.na(den) || den == 0) NA_real_ else (tp * tn - fp * fn) / den
}
mo_ev_mcc <- function(ev) {
  if (is.null(ev) || !NROW(ev) || any(is.na(ev$tp))) return(NA_real_)
  mo_mcc(sum(ev$tp), sum(ev$fa), sum(ev$fn), sum(ev$tn))
}
# Найнижчий MCC, який дала б основа, якби помилилась ще на w тижнів (будь-якого виду: пропущений тиждень епідемії
# або тиждень над порогом поза епідемією). Набір з більшою кількістю сезонів допускається, якщо його MCC не нижчий.
mo_mcc_floor <- function(tp, fp, fn, tn, w) {
  if (any(is.na(c(tp, fp, fn, tn)))) return(NA_real_)
  v <- sapply(0:w, function(j) { k <- w - j; if (j > tp || k > tn) NA_real_ else mo_mcc(tp - j, fp + k, fn + j, tn - k) })
  min(v, na.rm = TRUE)
}

mo_loss <- function(ev, cfg) {
  if (is.null(ev) || !NROW(ev)) return(NA_real_)
  cfg$angle$weights$false_alert * mean(ev$fa) + cfg$angle$weights$delay * mean(ev$delay)
}

# ---------------------------------------------------------------- вибір кута
mo_angle_grid <- function(cfg) seq(cfg$angle$min, cfg$angle$max, by = cfg$angle$step)

# Найнижчий кут, до якого можна опускатись від стандартного кута (2,8) вниз: епідемічний поріг
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
mo_scan_angles <- function(d, cfg, eval_seasons = NULL) {
  grid <- mo_angle_grid(cfg); truth <- cfg$angle$truth_angle
  S <- names(d)
  tms <- lapply(S, function(s) mo_season_timing(d[[s]], cfg)); names(tms) <- S
  rows <- lapply(grid, function(p) {
    tv <- mo_thr_vec(mo_thr_model(d, p))
    gv <- mo_good_vec(d, p, cfg$angle$min_seasons_goodness)
    ev <- mo_loso_eval(d, p, truth, tms, eval_seasons = eval_seasons, noepi = cfg$.noepi_cur)
    data.frame(angle = p, as.list(tv), as.list(gv), fa = if (is.null(ev)) NA else mean(ev$fa), delay = if (is.null(ev)) NA else mean(ev$delay),
               miss_pct = if (is.null(ev)) NA else 100 * mean(ev$miss), loss = mo_loss(ev, cfg),
               noepi = if (is.null(ev)) "" else paste(ev$season[ev$noepi == 1], collapse = ", "),
               ctl_tp = if (is.null(ev)) NA else sum(ev$tp), ctl_fp = if (is.null(ev)) NA else sum(ev$fa),
               ctl_fn = if (is.null(ev)) NA else sum(ev$fn), ctl_tn = if (is.null(ev)) NA else sum(ev$tn), mcc_ctl = mo_ev_mcc(ev), n_ctl = if (is.null(ev)) 0 else nrow(ev))
  })
  scan <- do.call(rbind, rows)
  scan$balance <- pmin(scan$sens, scan$spec)
  scan
}

# Крок 2: вибір кута зі скану за правилами (межа стабільності, MCC більше 0, епідемічний поріг нижчий за середній, мінімум сумарної помилки)
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

mo_choose_angle <- function(d, cfg, eval_seasons = NULL) mo_select_angle(mo_scan_angles(d, cfg, eval_seasons), cfg)

# ---------------------------------------------------------------- добір сезонів
# Сезони впорядковано від найстарішого до найновішого, виключені вже прибрано, лишилось не більше max_seasons найновіших.
# Основа набору: усі сезони від початку нинішньої епохи (era_start, зараз воєнні сезони з 2023/2024).
# Поки їх менше era_full, до основи по одному додаються старші сезони, підряд від найновішого; кандидати це набори
# з k найновіших сезонів, від основи до всіх доступних. Коли сезонів епохи вже era_full і більше, вибір триває:
# порівнюються набори з recommended_min (5) до всіх доступних (не більше max_seasons, 10) найновіших сезонів.
# Контрольні сезони: сезони епохи, крім not_validation. Для кожного кандидата кут обирається за звичайними правилами,
# а помилка рахується так: поріг з набору без контрольного сезону, хибні сигнали до і після епідемії плюс запізнення,
# сума за всіма контрольними сезонами. MCC кожного кандидата рахується на тих самих контрольних сезонах і тим самим
# способом (тижні над порогом проти тижнів епідемії). Запобіжник: кандидат відкидається, якщо його MCC нижчий, ніж
# був би MCC основи з guard_weeks додатковими помилковими тижнями (типово один тиждень за всі контрольні сезони разом).
# Обирається найменша сумарна помилка; при рівності вищий MCC; при рівності і MCC набір з recommended_min (5) і більше
# сезонів, бо автори MEM радять 5-10 сезонів; далі коротший набір.
mo_select_seasons <- function(d, cfg) {
  cfg$angle$truth_angle <- as.numeric(cfg$angle$truth_angle)
  cfg$.noepi_cur <- mo_noepi_seasons(d, cfg)
  ss <- cfg$season_selection; S <- names(d); N <- length(S)
  era <- S[S >= ss$era_start]; nE <- length(era)
  V <- setdiff(era, ss$not_validation)
  one <- function(set, note) list(seasons = set, eval_seasons = if (length(V)) V else NULL, note = note, truth_angle = cfg$angle$truth_angle, noepi = cfg$.noepi_cur,
    table = data.frame(n_available = N, n_seasons = length(set), seasons_used = paste(set, collapse = ", "), eval_seasons = paste(V, collapse = ", "),
                       angle = NA, base = NA, medium = NA, mcc_ctl = NA, fa_ctl_sum = NA, delay_ctl_sum = NA, loss_ctl_sum = NA,
                       ctl_tp = NA, ctl_fp = NA, ctl_fn = NA, ctl_tn = NA, noepi = NA, allowed = NA, mcc_floor = NA, chosen = TRUE, note = note))
  if (!isTRUE(ss$enabled)) return(one(S, "добір сезонів вимкнено, взято всі"))
  # поки сезонів нинішнього періоду менше era_full, основа це всі вони; коли їх era_full і більше, щороку порівнюються
  # набори з recommended_min (5) до N (не більше 10) найновіших сезонів
  kmin <- if (nE >= ss$era_full) ss$recommended_min else max(ss$min_seasons, nE)
  if (N <= kmin || !length(V)) return(one(S, paste0("доступно ", N, " сезонів, вибирати немає з чого: взято всі")))
  rows <- lapply(kmin:N, function(k) {
    sub <- tail(S, k); ch <- mo_choose_angle(d[, sub, drop = FALSE], cfg, eval_seasons = V)
    sc <- ch$scan[ch$scan$chosen, ][1, ]
    data.frame(n_available = N, n_seasons = k, seasons_used = paste(sub, collapse = ", "), eval_seasons = paste(V, collapse = ", "), angle = ch$angle,
               base = sc$base, medium = sc$medium, mcc_ctl = sc$mcc_ctl, fa_ctl_sum = sc$fa * sc$n_ctl, delay_ctl_sum = sc$delay * sc$n_ctl, loss_ctl_sum = sc$loss * sc$n_ctl,
               ctl_tp = sc$ctl_tp, ctl_fp = sc$ctl_fp, ctl_fn = sc$ctl_fn, ctl_tn = sc$ctl_tn, noepi = sc$noepi,
               allowed = !grepl("немає допустимого", ch$note), stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  m0 <- with(tab[1, ], mo_mcc_floor(ctl_tp, ctl_fp, ctl_fn, ctl_tn, ss$guard_weeks))
  tab$mcc_floor <- m0
  guard <- if (is.na(m0)) rep(TRUE, nrow(tab)) else (!is.na(tab$mcc_ctl) & tab$mcc_ctl >= m0 - 1e-9)
  guard[1] <- TRUE
  cand <- tab$allowed & !is.na(tab$loss_ctl_sum) & guard
  if (!any(cand)) { pick <- kmin; note <- "жоден набір не дав допустимого кута, взято основу" } else {
    o <- order(tab$loss_ctl_sum, -round(ifelse(is.na(tab$mcc_ctl), -Inf, tab$mcc_ctl), 6), tab$n_seasons < ss$recommended_min, tab$n_seasons)
    pick <- tab$n_seasons[o[cand[o]][1]]
    note <- paste0("найменша сумарна помилка на контрольних сезонах; при рівності вищий MCC; при рівності і MCC набір з ", ss$recommended_min, " і більше сезонів (рекомендація авторів MEM), далі коротший")
  }
  tab$allowed <- tab$allowed & guard
  tab$chosen <- tab$n_seasons == pick; tab$note <- ifelse(tab$chosen, note, ifelse(guard, "", paste0("MCC нижчий, ніж в основи з ", ss$guard_weeks, " додатковим помилковим тижнем")))
  list(seasons = tail(S, pick), eval_seasons = V, note = note, table = tab, truth_angle = cfg$angle$truth_angle, noepi = cfg$.noepi_cur)
}

# Кут власного порогу малої території: найвищий кут не вище кута групи, при якому епідемічний поріг
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
  if (!is.null(item$truth_angle)) cfg$angle$truth_angle <- item$truth_angle
  cfg$.noepi_cur <- item$noepi
  ch <- mo_choose_angle(d, cfg, item$eval_seasons)
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

# Кут меж епідемії для малої території: той самий, що в її еталона (ряди громад надто шумні, щоб обирати власний)
mo_p_truth <- function(p, cfg) {
  a <- cfg$.truth[[p$refid]]
  if (is.null(a)) a <- cfg$angle$truth_angle
  as.numeric(a)
}

# Крок 1: результати власного порогу для кожного кута (для вибору кута за групою)
mo_hrom_angle_scan <- function(p, cfg) {
  d <- p$d; S <- names(d); truth <- mo_p_truth(p, cfg)
  tms <- lapply(S, function(s) { y <- d[[s]]; if (sum(!is.na(y)) >= 10) mo_timing(y, truth) else NULL }); names(tms) <- S
  noepi <- mo_noepi_seasons(d, cfg)
  do.call(rbind, lapply(mo_angle_grid(cfg), function(a) {
    ev <- mo_loso_eval(d, a, truth, tms, noepi = noepi)
    if (is.null(ev)) return(NULL)
    data.frame(id = p$h$id, angle = a, ev, row.names = NULL)
  }))
}

# Крок 2: пороги всіх способів на повних даних і на кожному відкладеному сезоні
mo_hrom_thresholds <- function(j, cfg) {
  p <- j$p; own_angle <- j$own_angle; ref_angle <- j$ref_angle
  d <- p$d; S <- names(d); truth <- mo_p_truth(p, cfg); refd <- p$refd
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
  # сезони без епідемії громади (за її власним рядом): у них рахуються лише хибні сигнали
  ne <- mo_noepi_seasons(d, cfg)
  hold <- list()
  for (hs in S) {
    y <- d[[hs]]
    tm <- if (sum(!is.na(y)) >= 10) mo_timing(y, truth) else NULL
    if (!(hs %in% ne) && (is.null(tm) || any(is.na(tm)))) next
    hold[[length(hold) + 1]] <- list(held = hs, tm = tm, noepi = hs %in% ne, th = build(setdiff(S, hs), hs))
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
  cat("  Підбір кута нахилу для груп громад за кількістю випадків\n"); flush.console()
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
  g <- cfg$small_areas$groups
  tier_txt <- c("надійні дані" = sprintf("з надійними даними (у середньому %s і більше випадків на тиждень", g$reliable_min),
                "обмежені дані" = sprintf("з обмеженими даними (від %s до %s випадків", g$limited_min, g$reliable_min),
                "ненадійні дані" = sprintf("з ненадійними даними (менше %s випадків", g$limited_min))
  ord <- intersect(names(tier_txt), names(angle_by_tier))
  cat(paste0("  Кут нахилу для громад ", paste0(tier_txt[ord], ", громад: ", vapply(ord, function(t) sum(tier == t), integer(1)), "): ",
                                                 gsub(".", ",", as.character(angle_by_tier[ord]), fixed = TRUE), collapse = "; "), "\n")); flush.console()

  cat("  Розрахунок порогів громад і перевірка на сезоні, відкладеному для контролю\n"); flush.console()
  # Кут власного порогу кожної громади: не вище кута групи і такий, при якому епідемічний поріг нижчий за середній
  # (та сама умова допустимості, що для основних територій). Якщо такого кута немає, власний поріг непридатний (вага 0).
  rule <- isTRUE(cfg$small_areas$angle_rule == "base_below_medium")
  own_angle <- mapply(function(p, tr) if (rule) mo_pick_angle_below(p$d, angle_by_tier[[tr]], cfg) else angle_by_tier[[tr]], preps, tier)
  usable <- !is.na(own_angle)
  own_angle_run <- ifelse(usable, own_angle, angle_by_tier[tier])
  cat(sprintf("  Для %d громад кут зменшено, щоб епідемічний поріг лишався нижчим за середній. У %d громад власні дані не дають придатного порогу, тому їхні пороги розраховано за даними району без районного центру (а якщо його немає, усього району).\n",
              sum(usable & abs(own_angle_run - angle_by_tier[tier]) > 1e-9), sum(!usable))); flush.console()
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
    ne <- mo_noepi_seasons(d, cfg)
    for (s in S) {
      y <- d[, s]; z <- refd[rownames(d), s]; ok <- !is.na(y) & !is.na(z)
      r_s <- c(r_s, if (sum(ok) >= 8) cor(y[ok], z[ok]) else NA)
      if (s %in% ne) { fa <- c(fa, sum(!is.na(y) & y > th$own[1])); dl <- c(dl, NA); ms <- c(ms, 0); next }
      tm <- if (sum(!is.na(y)) >= 10) mo_timing(y, mo_p_truth(p, cfg)) else NULL
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
        res_ <- mo_season_result(y, unname(ms_[[m]][1]), x$tm, isTRUE(x$noepi))
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
    raw <- raw[, !(names(raw) %in% mo_excluded(cfg)), drop = FALSE]
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
    if (is.null(core)) stop("Для паралельного розрахунку потрібна змінна MO_CORE зі шляхом до memopt_core.R (її задає run_optimizer.R і застосунок)")
    cl <- parallel::makeCluster(nw)
    parallel::clusterExport(cl, "core", envir = environment())
    invisible(parallel::clusterEvalQ(cl, source(core, encoding = "UTF-8")))
    assign("MO_CL", cl, envir = .GlobalEnv)
    return(cl)
  }
  NULL
}

# Перевірка вхідних даних: для кожної території і сезону пропущені тижні всередині сезону (без святкових),
# нульові тижні і пік сезону відносно медіани піків інших сезонів. Позначка підказує, що варто перевірити в даних.
mo_check_inputs <- function(cfg, terr) {
  hol <- as.character(cfg$season$holiday_weeks)
  rows <- list()
  for (i in seq_len(nrow(terr))) {
    raw <- tryCatch(mo_read_raw(terr[i, ], cfg), error = function(e) NULL)
    if (is.null(raw)) { rows[[length(rows) + 1]] <- data.frame(id = terr$id[i], name = terr$name[i], season = NA, weeks_missing = NA, weeks_zero = NA, peak_ratio = NA, note = "ряд не прочитано"); next }
    raw <- raw[!(rownames(raw) %in% hol), , drop = FALSE]
    peaks <- vapply(raw, function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE), numeric(1))
    for (s in names(raw)) {
      v <- raw[[s]]; obs <- which(!is.na(v))
      if (!length(obs)) next
      inside <- v[min(obs):max(obs)]
      miss <- sum(is.na(inside)); zero <- sum(v == 0, na.rm = TRUE)
      others <- peaks[names(peaks) != s & !(names(peaks) %in% mo_excluded(cfg))]
      ratio <- if (sum(!is.na(others)) >= 2) peaks[[s]] / median(others, na.rm = TRUE) else NA_real_
      note <- c(if (miss > 3) "багато пропущених тижнів", if (zero > 3) "багато нульових тижнів",
                if (!is.na(ratio) && ratio > 3) "пік утричі вищий за звичний", if (!is.na(ratio) && ratio < 1 / 3) "пік утричі нижчий за звичний",
                if (s %in% names(MO_AUTO_EXCLUDE)) "сезон відсікається автоматично")
      rows[[length(rows) + 1]] <- data.frame(id = terr$id[i], name = terr$name[i], season = s, weeks_missing = miss, weeks_zero = zero,
                                             peak_ratio = ratio, note = paste(note, collapse = "; "))
    }
  }
  do.call(rbind, rows)
}

# Зміни порогів проти попереднього запуску; попередня таблиця зберігається в output/archive з датою
mo_changes <- function(thr_new, out_dir, cfg) {
  f <- file.path(out_dir, "memopt_thresholds.csv")
  if (!file.exists(f)) return(NULL)
  old <- read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8")
  arch <- file.path(out_dir, "archive"); dir.create(arch, showWarnings = FALSE)
  file.copy(f, file.path(arch, paste0("memopt_thresholds_", format(file.mtime(f), "%Y-%m-%d_%H%M"), ".csv")), overwrite = TRUE)
  if (!("infl_max_pct" %in% names(old))) old$infl_max_pct <- NA
  m <- merge(thr_new[, c("id", "name", "seasons_used", "base", "medium", "infl_max_pct")], old[, c("id", "seasons_used", "base", "medium", "infl_max_pct")],
             by = "id", all.x = TRUE, suffixes = c("", "_prev"))
  m$pct_base <- (m$base / m$base_prev - 1) * 100
  m$pct_medium <- (m$medium / m$medium_prev - 1) * 100
  # Звичайна щорічна зміна порогу (додано новий сезон, набір сезонів обрано заново) порівнянна з найбільшим впливом
  # одного сезону (на скільки змінюється поріг, якщо прибрати з набору один сезон). Більша зміна позначається для перевірки
  # даних. Межа береться з попереднього запуску, якщо вона там є.
  m$flag_limit <- ifelse(is.na(m$infl_max_pct_prev), m$infl_max_pct, m$infl_max_pct_prev)
  big <- !is.na(m$flag_limit) & abs(m$pct_base) > m$flag_limit
  m$note <- ifelse(is.na(m$base_prev), "нова територія",
    ifelse(big, paste0("зміна більша за вплив будь-якого одного сезону (до ", round(m$flag_limit), "%): перевірте дані"), ""))
  m$infl_max_pct <- NULL; m$infl_max_pct_prev <- NULL
  m[order(-pmax(abs(m$pct_base), abs(m$pct_medium), na.rm = TRUE)), ]
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
  found <- unique(unlist(lapply(seq_len(nrow(terr)), function(i) names(mo_read_raw(terr[i, ], cfg)))))
  raw_names <- unique(unlist(lapply(seq_len(nrow(terr)), function(i) { f <- cfg$.find(terr$file[i]); names(read_excel(f, sheet = terr$sheet[i], n_max = 0))[-1] })))
  unf <- mo_unfinished(raw_names, cfg)
  if (length(unf)) cat("  Сезон", paste(unf, collapse = ", "), "ще триває, у розрахунок не береться
")
  auto <- intersect(names(MO_AUTO_EXCLUDE), found)
  if (length(auto)) cat("  Увага: сезони", paste0(auto, " (", MO_AUTO_EXCLUDE[auto], ")", collapse = ", "), "відсічено автоматично, вони не придатні для порогів\n")
  checks <- mo_check_inputs(cfg, terr)
  nflag <- sum(nzchar(checks$note) & !grepl("^сезон відсікається автоматично$", checks$note))
  if (nflag) cat(sprintf("  Незвичних сезонів у даних територій: %d (багато нульових чи пропущених тижнів або пік, утричі вищий чи нижчий за звичний). Розрахунку це не заважає, ці сезони беруться в розрахунок. Перелік буде в застосунку на кроці «Результати» і в книзі результатів на аркуші «%s»: перевірте, чи правильно внесено ці дані.\n", nflag, MO_SHEETS$input_check))
  cfg$.season_sets <- NULL
  ser <- list()
  for (i in seq_len(nrow(terr))) ser[[terr$id[i]]] <- mo_series(terr[i, ], cfg, log, all_seasons = TRUE)
  cens <- if (length(log$rows)) do.call(rbind, log$rows) else data.frame()

  series_terr <- terr[terr$kind == "main", ]
  cat("2. Вибір сезонів для області, районів і міст\n")
  cat("  Для кожної території програма порівнює набори з кількох останніх сезонів і бере той, з яким пороги найточніше відділяють епідемію від спокійних тижнів у сезонах, відкладених для перевірки.\n"); flush.console()
  sel <- mo_pmap(lapply(series_terr$id, function(id) ser[[id]]), mo_select_seasons, cfg)
  names(sel) <- series_terr$id
  season_pick <- do.call(rbind, lapply(seq_along(sel), function(i) data.frame(id = series_terr$id[i], name = series_terr$name[i], sel[[i]]$table, row.names = NULL)))
  sets <- list()
  for (id in series_terr$id) {
    ser[[id]] <- ser[[id]][, sel[[id]]$seasons, drop = FALSE]; sets[[id]] <- sel[[id]]$seasons
    k <- length(sel[[id]]$seasons)
    cat(sprintf("   %s: %d %s з %d наявних (%s)%s\n", series_terr$name[match(id, series_terr$id)], k,
                if (k %% 10 == 1 && k %% 100 != 11) "сезон" else if (k %% 10 %in% 2:4 && !(k %% 100 %in% 12:14)) "сезони" else "сезонів",
                sel[[id]]$table$n_available[1], paste(sel[[id]]$seasons, collapse = ", "),
                if (grepl("^найменша сумарна помилка", sel[[id]]$note)) "" else paste0("; ", sel[[id]]$note)))
  }
  nera <- sum(unique(unlist(lapply(ser, names))) >= cfg$season_selection$era_start)
  if (nera < cfg$season_selection$era_full) cat(sprintf("  Увага: сезонів від %s лише %d. Пороги громад, які мають дані тільки за ці сезони, менш надійні, ніж були б на %d і більше сезонах.\n", cfg$season_selection$era_start, nera, cfg$season_selection$era_full))
  cfg$.season_sets <- sets
  cfg$.truth <- lapply(sel, `[[`, "truth_angle")
  writeLines(jsonlite::toJSON(sets, auto_unbox = FALSE, pretty = TRUE), file.path(out_dir, "season_sets.json"), useBytes = TRUE)

  cat("3. Пороги області, районів і міст\n")
  cat("  Кут нахилу визначає, де в минулих сезонах починалась і закінчувалась епідемія; від цього залежать пороги. Програма підбирає його для кожної території.\n"); flush.console()
  items <- lapply(seq_len(nrow(series_terr)), function(i) list(row = series_terr[i, ], d = ser[[series_terr$id[i]]], eval_seasons = sel[[series_terr$id[i]]]$eval_seasons,
                                                         truth_angle = sel[[series_terr$id[i]]]$truth_angle, noepi = sel[[series_terr$id[i]]]$noepi))
  tr <- mo_pmap(items, mo_territory, cfg)
  tabs <- do.call(rbind, lapply(tr, `[[`, "res"))
  scans <- do.call(rbind, lapply(tr, `[[`, "scan"))
  infl <- do.call(rbind, lapply(tr, `[[`, "infl"))
  stab <- do.call(rbind, Filter(Negate(is.null), lapply(tr, `[[`, "stab")))
  floors <- data.frame(id = tabs$id, name = tabs$name, floor = vapply(tr, `[[`, numeric(1), "floor"), angle = tabs$angle)
  for (i in seq_len(nrow(tabs))) cat(sprintf("   %s: кут нахилу %s\n", tabs$name[i], format(tabs$angle[i], decimal.mark = ",")))

  hr <- NULL; prop <- NULL
  if (has_small) {
    cat("4. Пороги громад\n"); flush.console()
    hr <- mo_small_areas(cfg, terr, ser, tabs, log)
    if (isTRUE(cfg$diagnostics$proportionality)) prop <- mo_proportionality(cfg, terr, ser)
  }
  mw <- NULL
  if (isTRUE(cfg$diagnostics$multiple_waves)) {
    cat("5. Перевірка сезонів, у яких захворюваність зростала кількома хвилями\n"); flush.console()
    mwitems <- lapply(seq_len(nrow(series_terr)), function(i) list(id = series_terr$id[i], name = series_terr$name[i], level = series_terr$level[i],
                                                                   d = ser[[series_terr$id[i]]], angle = tabs$angle[i]))
    if (has_small) {
      rel <- hr$main[hr$main$tier == "надійні дані", ]
      if (NROW(rel)) mwitems <- c(mwitems, lapply(seq_len(nrow(rel)), function(i) list(id = rel$id[i], name = rel$name[i], level = terr$level[match(rel$id[i], terr$id)],
                                                                                       d = ser[[rel$id[i]]], angle = hr$angle_by_tier[["надійні дані"]])))
    }
    mw <- do.call(rbind, mo_pmap(mwitems, mo_multiwave_one, cfg))
    # та сама межа 12%, що й у звіті про книгу результатів
    mwbig <- mw$name[(!is.na(mw$pct_base_mw) & abs(mw$pct_base_mw) > 12) | (!is.na(mw$pct_veryhigh_mw) & abs(mw$pct_veryhigh_mw) > 12)]
    if (length(mwbig)) cat(sprintf("  Якби кожну хвилю рахувати окремим сезоном, пороги змінились би більш ніж на 12%% у %d %s: %s. Розраховані пороги від цього не змінюються. Якщо в цих територіях були сезони з двома піками, порівняйте пороги на аркуші «%s» книги результатів.\n",
                                    length(mwbig), if (length(mwbig) == 1) "території" else "територіях", paste(mwbig, collapse = ", "), MO_SHEETS$waves))
    else cat("  Сезони з кількома хвилями на пороги помітно не впливають.\n")
  }

  cat("6. Запис результатів\n"); flush.console()
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
  # паспорт порогу основної території: контрольні сезони, якість, помилка і як обрано набір
  pk <- season_pick[season_pick$chosen, ]
  thr_all$eval_seasons <- pk$eval_seasons[match(thr_all$id, pk$id)]
  thr_all$mcc_ctl <- round(pk$mcc_ctl[match(thr_all$id, pk$id)], 3)
  # найбільший вплив одного сезону на епідемічний поріг, %: для основної території з її набору, для малої з її еталона
  imax <- tapply(abs(infl$pct_base), infl$id, max, na.rm = TRUE)
  rid <- ifelse(thr_all$kind == "основна", thr_all$id, terr$reference_id[match(thr_all$id, terr$id)])
  thr_all$infl_max_pct <- round(as.numeric(imax[rid]), 1)
  thr_all$loss_ctl_sum <- pk$loss_ctl_sum[match(thr_all$id, pk$id)]
  thr_all$note <- pk$note[match(thr_all$id, pk$id)]
  thr_all$noepi <- vapply(thr_all$id, function(id) if (!is.null(sel[[id]])) paste(sel[[id]]$noepi, collapse = ", ") else NA_character_, character(1))
  thr_all$truth_angle <- vapply(thr_all$id, function(id) if (!is.null(sel[[id]])) as.numeric(sel[[id]]$truth_angle) else NA_real_, numeric(1))
  changes <- mo_changes(thr_all, out_dir, cfg)
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
    # еталони малих територій і правило підтвердження районом: ETL рахує, чи еталон не нижче свого епідемічного порогу
    sm <- terr[terr$kind == "small", ]
    dc <- if ("district_confirm" %in% names(sm)) ifelse(tolower(trimws(sm$district_confirm)) %in% c("так", "yes", "1"), 1L, 0L) else 0L
    write.csv(data.frame(PCODE = sm$id, Ref_PCODE = sm$reference_id, District_Confirm = dc),
              file.path(ex_dir, "Dim_Reference.csv"), row.names = FALSE, fileEncoding = "UTF-8")
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
  # перший аркуш: перелік аркушів простими словами, щоб у великій книзі було видно, що потрібно завжди, а що лише для докладної перевірки
  toc <- data.frame(Аркуш = unlist(S[c("thresholds", "changes", "input_check", "main", "season_pick", "small", "quality", "angle", "floor", "group_angle", "influence",
                                       "stability", "cleaning", "hold_groups", "hold_all", "weights", "proportionality", "waves", "config")]),
    Потрібно = c(rep("завжди", 3), rep("для докладної перевірки", 16)),
    `Що на аркуші` = c("Чотири пороги кожної території на 100 тис. населення за тиждень: епідемічний, середній, високий, дуже високий. У колонці «Примітка» сказано, якщо з порогом щось не так.",
      "На скільки відсотків змінився кожен поріг порівняно з попереднім розрахунком. Після першого розрахунку аркуш порожній.",
      "Сезони, які помітно відрізняються від звичних для території (позначено в колонці «Примітка»): перевірте в Excel, чи правильно внесено ці дані.",
      "Пороги області, районів і міст і те, наскільки точно вони відділяли епідемічні тижні від спокійних у минулих сезонах.",
      "Чому для території взято саме такий набір сезонів (обраний набір позначено в колонці «Обрано»).",
      "З чого складено поріг кожної громади: власний поріг, поріг за даними району і частка власного порогу.",
      "Наскільки схожі криві громади і району та як часто власний поріг громади давав хибну тривогу.",
      "Чому для території обрано саме такий кут нахилу (від нього залежить, які тижні минулих сезонів вважаються епідемічними).",
      "Найменший кут нахилу, який ще можна брати для території.",
      "Кут нахилу для громад з багатьма, середньою і малою кількістю випадків.",
      "На скільки змінився б поріг, якби прибрати з розрахунку один сезон.",
      "Як змінюється поріг, якщо брати різні набори сезонів.",
      "Громади і сезони, у яких прибрано перші тижні, набагато вищі за наступні.",
      "Порівняння способів розрахунку порогів громад на сезонах, яких розрахунок не бачив, окремо для груп громад.",
      "Те саме для всіх громад разом.",
      "Частка власного порогу в порозі кожної громади (1 повністю власний, 0 повністю за даними району).",
      "Чи стала частка захворюваності громади від захворюваності району в спокійні і в напружені тижні.",
      "Як змінились би пороги, якби кожну хвилю в сезоні з кількома хвилями рахувати окремо.",
      "З якими налаштуваннями і коли зроблено розрахунок."), check.names = FALSE)
  add(S$thresholds, thr_all)
  add(S$changes, changes)
  add(S$input_check, checks)
  add(S$main, tabs)
  add(S$season_pick, season_pick)
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
                                   "кут для меж епідемії в контрольних сезонах (memtiming)", "максимальна межа падіння епідемічного порогу на кроці вниз", "кут для малих територій за групами",
                                   "кут власного порогу громади",
                                   "спосіб для малих територій", "очищення початку сезону", "групи за медіаною випадків (надійні від, обмежені від)",
                                   "максимум сезонів у розрахунку", "добір сезонів основних територій", "випуск"),
    значення = c(mo_def(cfg$project, ""),
                 paste(c(paste0(names(MO_AUTO_EXCLUDE), " (", MO_AUTO_EXCLUDE, ", завжди)"),
                         if (length(cfg$exclude_seasons)) paste0(names(cfg$exclude_seasons), " (", unlist(cfg$exclude_seasons), ", виключено користувачем)"),
                         if (length(cfg$exclude_territory_seasons)) unlist(lapply(names(cfg$exclude_territory_seasons), function(id) { e <- cfg$exclude_territory_seasons[[id]]
                           paste0(names(e), " лише для ", id, " (", unlist(e), ", виключено користувачем)") }))), collapse = "; "),
                 paste(cfg$angle$min, cfg$angle$max, cfg$angle$step, sep = ", "), cfg$angle$criterion,
                 paste(cfg$angle$weights$false_alert, cfg$angle$weights$delay, sep = " : "), cfg$angle$truth_angle, cfg$angle$stability_drop,
                 if (has_small) paste(names(hr$angle_by_tier), hr$angle_by_tier, sep = " = ", collapse = "; ") else "немає малих територій",
                 if (cfg$small_areas$angle_rule == "base_below_medium") "найвищий кут не вище кута групи, при якому епідемічний поріг нижчий за середній; інакше власний поріг непридатний (вага 0)" else "кут групи",
                 cfg$small_areas$method,
                 paste0(cfg$start_cleaning$enabled, ", множник MAD ", cfg$start_cleaning$mad_multiplier, ", порівняння з ", cfg$start_cleaning$compare_weeks, " наступними тижнями"),
                 paste(cfg$small_areas$groups$reliable_min, cfg$small_areas$groups$limited_min, sep = ", "), cfg$season$max_seasons,
                 with(cfg$season_selection, if (isTRUE(enabled)) paste0("увімкнено: основа всі сезони від ", era_start, ", поки їх менше ", era_full,
                   " додаються старші підряд від найновішого (не більше ", cfg$season$max_seasons, " разом, мінімум ", min_seasons, "); контрольні сезони від ", era_start,
                   " крім ", paste(not_validation, collapse = ", "), "; найменша сумарна помилка, при рівності вищий MCC на контрольних сезонах, далі набір з ", recommended_min, " і більше сезонів; набір відкидається, якщо його MCC нижчий, ніж в основи з ", guard_weeks, " додатковим помилковим тижнем; коли сезонів від ", era_start, " стане ", era_full, " і більше, порівнюються набори від ", recommended_min, " до ", cfg$season$max_seasons, " найновіших сезонів; сезон, пік якого нижчий за епідемічний поріг (кут ", cfg$angle$truth_angle, ", інші сезони території), вважається сезоном без епідемії: у ньому рахуються лише хибні сигнали")
                   else "вимкнено: усі сезони після виключень"),
                 paste0("memopt 0.4, ", format(Sys.time(), "%Y-%m-%d %H:%M"))))
  add(S$config, cfgdf, widths = c(55, 120))
  toc <- toc[toc$Аркуш %in% names(wb), ]
  add("Зміст", toc, widths = c(32, 24, 120))
  addStyle(wb, "Зміст", createStyle(wrapText = TRUE, valign = "top"), rows = seq_len(nrow(toc)) + 1, cols = 1:3, gridExpand = TRUE)
  worksheetOrder(wb) <- c(length(names(wb)), seq_len(length(names(wb)) - 1))
  f <- file.path(out_dir, cfg$paths$workbook_name)
  saveWorkbook(wb, f, overwrite = TRUE)
  saveRDS(list(tabs = tabs, hr = hr, prop = prop, mw = mw, scans = scans, floors = floors, thr = thr_all, season_pick = season_pick, season_sets = sets), file.path(out_dir, "last_run.rds"))
  mo_export_filled(cfg, terr, file.path(out_dir, "reference_series_filled.csv"))
  cat("Готово. Книга результатів:", f, "\n")
  invisible(list(cfg = cfg, tabs = tabs, hromadas = hr, thr = thr_all))
}
