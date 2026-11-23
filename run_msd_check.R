# Порівняння порогів MSD (mean standard deviation, UKHSA: Sinnathamby et al., Euro Surveill 2024;29(45):2400696)
# з порогами memopt і простого MEM для області, районів і громад.
#
# MSD: середнє і стандартне відхилення тижневої захворюваності за рік від 27-го до 26-го тижня (усі тижні року, разом з літом);
#   епідемічний поріг = середнє, середній = середнє + 1 SD, високий = середнє + 3 SD, дуже високий = середнє + 5 SD.
#   Річні ряди беруться з ETL (Fact_Diseases.csv, з 42-го тижня 2023 року) і зводяться до масштабу рядів memopt.
#
# Перевірка проспективна, як у статті: поріг будується лише з даних до сезону, що перевіряється.
#   2025/26: MSD за рік 27/2024-26/2025 (повний рік), MSD за два роки (з 42/2023), memopt без 2025/26 (лише попередні сезони),
#            простий MEM на попередніх сезонах (кут 2,8).
#   2024/25: MSD за неповний рік 42/2023-26/2024 (бракує тижнів 27-41); memopt без 2024/25 тут не проспективний (бачив 2025/26),
#            тому сезон 2024/25 допоміжний.
# Межі епідемії у сезоні: за MEM (memtiming, кут 2,8) і за моделлю двох станів (R/memopt_hmm.R).
# Показники за тижнями (епідемічний тиждень проти тижня над порогом): чутливість (POD), специфічність, PPV, NPV, F1, MCC, Юден,
#   точність, частка хибних тривог (FAR); і за сезонами: хибні тижні, тижні запізнення, пропущені епідемії; для середнього порогу
#   тижні над ним поза епідемією і запізнення до нього.
# Запуск: Rscript run_msd_check.R [шлях до output/csv ETL]. Результат: output/msd_check/порівняння_MSD_MEM.xlsx
# Не входить у пакет для ЦГЗ, робочих порогів не змінює.
args <- commandArgs(trailingOnly = TRUE)
here <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])), winslash = "/")
etl_csv <- if (length(args)) args[1] else file.path(here, "..", "..", "Dashboard GRVI", "GRVI_Dashboard_ETL", "output", "csv")
MO_CORE <- normalizePath(file.path(here, "R", "memopt_core.R"), winslash = "/")
source(MO_CORE, encoding = "UTF-8")
source(file.path(here, "R", "memopt_hmm.R"), encoding = "UTF-8")
cfg <- mo_load_config(file.path(here, "config.json"))
cfg$angle$truth_angle <- as.numeric(cfg$angle$truth_angle)
cfg$.season_sets <- NULL
out_dir <- file.path(cfg$.path(cfg$paths$output_dir), "msd_check"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
terr <- mo_read_territories(cfg)
terr <- terr[terr$kind == "small" | terr$level %in% c("область", "район"), ]
hrom <- terr[terr$kind == "small", ]

# ---------------------------------------------------------------- 1. річні ряди з ETL
fd <- read.csv(file.path(etl_csv, "Fact_Diseases.csv"), stringsAsFactors = FALSE)
fd <- fd[fd$MetricCode == "grvi_cases" & !is.na(fd$Priority) & fd$Priority == 0 & fd$Age_Group_Std != "Students", ]
cases <- aggregate(Value ~ ADM3_PCODE + TimeKey, fd, sum)
rate_of <- function(ids) {
  x <- cases[cases$ADM3_PCODE %in% ids, ]
  s <- aggregate(Value ~ TimeKey, x, sum)
  data.frame(TimeKey = s$TimeKey, rate = s$Value / sum(hrom$population[hrom$id %in% ids]) * 1e5)
}
members <- function(t) if (t$kind == "small") t$id else if (t$level == "область") hrom$id else hrom$id[hrom$group == t$id]
weeks <- mo_weeks(cfg)
yr <- function(tk) tk %/% 100; wk <- function(tk) tk %% 100
season_of <- function(tk) { y <- ifelse(wk(tk) >= 27, yr(tk), yr(tk) - 1); paste0(y, "/", y + 1) }   # рік 27-26 позначено сезоном
block_of <- function(tk) ifelse(wk(tk) >= 27, yr(tk), yr(tk) - 1)                                     # рік MSD: 27/y-26/(y+1)

ser <- list(); yrr <- list(); scale <- c()
for (i in seq_len(nrow(terr))) {
  t <- terr[i, ]
  d <- mo_series(t, cfg, all_seasons = TRUE); ser[[t$id]] <- d
  r <- rate_of(members(t)); r$block <- block_of(r$TimeKey)
  # масштаб до рядів memopt за спільними тижнями сезонів (населення району в memopt трохи інше)
  r$season <- season_of(r$TimeKey); r$row <- as.character(wk(r$TimeKey))
  m <- mapply(function(s, w) if (s %in% names(d) && w %in% rownames(d)) d[w, s] else NA_real_, r$season, r$row)
  ok <- !is.na(m) & r$rate > 0
  k <- if (sum(ok) >= 10) median(m[ok] / r$rate[ok]) else 1
  scale[t$id] <- k; r$rate <- r$rate * k; yrr[[t$id]] <- r
}
cat("Масштаб річних рядів до memopt: від", round(min(scale), 4), "до", round(max(scale), 4), "\n")

msd <- function(v) { v <- v[!is.na(v)]; m <- mean(v); s <- sd(v); c(base = m, medium = m + s, high = m + 3 * s, veryhigh = m + 5 * s, n_weeks = length(v)) }
msd_thr <- function(id, blocks) msd(yrr[[id]]$rate[yrr[[id]]$block %in% blocks])

# ---------------------------------------------------------------- 2. пороги для кожного сезону перевірки
read_mo <- function(s) read.csv(file.path(cfg$.path(cfg$paths$output_dir), "quest_check", paste0("memopt_без_", gsub("/", "_", s)), "memopt_thresholds.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
plans <- list(
  "2025/2026" = list("MSD, 1 рік (27/2024-26/2025)" = 2024, "MSD, 2 роки (42/2023-26/2025)" = c(2023, 2024)),
  "2024/2025" = list("MSD, неповний рік (42/2023-26/2024)" = 2023))
rows <- list(); thr_rows <- list()
for (s in names(plans)) {
  mo <- read_mo(s)
  for (i in seq_len(nrow(terr))) {
    t <- terr[i, ]; d <- ser[[t$id]]
    if (!(s %in% names(d))) next
    prior <- names(d)[names(d) < s]
    th <- list()
    for (nm in names(plans[[s]])) th[[nm]] <- msd_thr(t$id, plans[[s]][[nm]])
    k <- match(t$id, mo$id); th[["memopt"]] <- c(base = mo$base[k], medium = mo$medium[k], high = mo$high[k], veryhigh = mo$veryhigh[k], n_weeks = NA)
    th[["простий MEM, попередні сезони"]] <- if (length(prior) >= 2) c(mo_thr_vec(mo_thr_model(d[, prior, drop = FALSE], 2.8)), n_weeks = NA) else c(base = NA, medium = NA, high = NA, veryhigh = NA, n_weeks = NA)
    for (nm in names(th)) thr_rows[[length(thr_rows) + 1]] <- data.frame(id = t$id, name = t$name, level = t$level, held = s, method = nm, as.list(round(th[[nm]], 1)))
    rows[[length(rows) + 1]] <- list(t = t, s = s, th = th)
  }
}

# ---------------------------------------------------------------- 3. розмітка епідемії і оцінка
truth_cache <- list()
hmm_truth <- function(id) {
  if (is.null(truth_cache[[id]])) { h <- mo_hmm_fit(ser[[id]]); truth_cache[[id]] <<- if (is.null(h)) NA else mo_hmm_truth(h) }
  truth_cache[[id]]
}
judge <- function(y, Tb, Tm, tm, noepi) {
  ok <- !is.na(y); n <- length(y)
  inp <- if (noepi || is.null(tm) || any(is.na(tm))) rep(FALSE, n) else seq_len(n) >= tm[1] & seq_len(n) <= tm[2]
  len <- sum(inp)
  one <- function(T) {
    if (is.na(T)) return(c(tp = NA, fp = NA, fn = NA, tn = NA, fa = NA, delay = NA, miss = NA))
    up <- ok & y > T; first <- which(inp & up)
    c(tp = sum(up & inp), fp = sum(up & !inp), fn = sum(ok & inp & !up), tn = sum(ok & !inp & !up), fa = sum(up & !inp),
      delay = if (!len) 0 else if (length(first)) first[1] - which(inp)[1] else len, miss = as.numeric(len > 0 && !length(first)))
  }
  b <- one(Tb); m <- one(Tm)
  c(epi_weeks = len, noepi = as.numeric(noepi), b, fa_med = unname(m["fa"]), delay_med = unname(m["delay"]), miss_med = unname(m["miss"]))
}
cat("Оцінка на сезонах для", length(rows), "територій-сезонів\n"); flush.console()
det <- list()
for (r in rows) {
  t <- r$t; s <- r$s; d <- ser[[t$id]]; y <- d[[s]]
  truths <- list("за MEM" = list(tm = mo_season_timing(y, cfg), noepi = s %in% mo_noepi_seasons(d, cfg)))
  ht <- hmm_truth(t$id)
  if (is.list(ht)) truths[["за моделлю двох станів"]] <- list(tm = ht$tms[[s]], noepi = s %in% ht$noepi)
  for (tn in names(truths)) for (mn in names(r$th)) {
    v <- judge(y, r$th[[mn]]["base"], r$th[[mn]]["medium"], truths[[tn]]$tm, truths[[tn]]$noepi)
    det[[length(det) + 1]] <- data.frame(id = t$id, name = t$name, level = t$level, held = s, truth = tn, method = mn,
                                         thr_base = round(unname(r$th[[mn]]["base"]), 1), thr_medium = round(unname(r$th[[mn]]["medium"]), 1), as.list(v))
  }
}
det <- do.call(rbind, det)
det$group <- ifelse(det$level == "громада", "громади", ifelse(det$level == "район", "райони", "область"))

metrics <- function(x) {
  x <- x[!is.na(x$tp), ]
  tp <- sum(x$tp); fp <- sum(x$fp); fn <- sum(x$fn); tn <- sum(x$tn)
  se <- tp / (tp + fn); sp <- tn / (tn + fp); ppv <- tp / (tp + fp); npv <- tn / (tn + fn)
  den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  data.frame(territory_seasons = nrow(x), sens = se, spec = sp, ppv = ppv, npv = npv, f1 = 2 * ppv * se / (ppv + se),
             mcc = if (den > 0) (tp * tn - fp * fn) / den else NA, youden = se + sp - 1, accuracy = (tp + tn) / (tp + fp + fn + tn),
             far = fp / (tp + fp), fa = sum(x$fa), delay = sum(x$delay), missed = sum(x$miss),
             fa_med = sum(x$fa_med), delay_med = sum(x$delay_med), missed_med = sum(x$miss_med))
}
key <- unique(det[, c("held", "truth", "group", "method")])
key <- key[order(-xtfrm(key$held), key$truth, match(key$group, c("область", "райони", "громади")), key$method), ]
sm <- do.call(rbind, lapply(seq_len(nrow(key)), function(i) {
  x <- det[det$held == key$held[i] & det$truth == key$truth[i] & det$group == key$group[i] & det$method == key$method[i], ]
  data.frame(key[i, ], no_threshold = sum(is.na(x$thr_base)), metrics(x), row.names = NULL)
}))
num <- sapply(sm, is.numeric); sm[num] <- lapply(sm[num], function(v) round(v, 3))

# ---------------------------------------------------------------- 4. пороги 2026/27: MSD за рік 27/2025-26/2026 проти робочих memopt
wk_thr <- read.csv(file.path(cfg$.path(cfg$paths$output_dir), "memopt_thresholds.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
cmp <- do.call(rbind, lapply(seq_len(nrow(terr)), function(i) {
  t <- terr[i, ]; m <- msd_thr(t$id, 2025); k <- match(t$id, wk_thr$id)
  data.frame(id = t$id, name = t$name, level = t$level, weeks_in_year = m[["n_weeks"]],
             mem_base = wk_thr$base[k], msd_base = round(m[["base"]], 1), mem_medium = wk_thr$medium[k], msd_medium = round(m[["medium"]], 1),
             mem_high = wk_thr$high[k], msd_high = round(m[["high"]], 1), mem_veryhigh = wk_thr$veryhigh[k], msd_veryhigh = round(m[["veryhigh"]], 1),
             ratio_base = round(m[["base"]] / wk_thr$base[k], 2), ratio_medium = round(m[["medium"]] / wk_thr$medium[k], 2))
}))

ru <- c(held = "Сезон перевірки", truth = "Межі епідемії", group = "Рівень", method = "Спосіб", no_threshold = "Поріг не побудовано",
        territory_seasons = "Територій", sens = "Чутливість (POD)", spec = "Специфічність", ppv = "PPV", npv = "NPV", f1 = "F1", mcc = "MCC",
        youden = "Юден", accuracy = "Точність", far = "Частка хибних тривог (FAR)", fa = "Хибних тижнів", delay = "Тижнів запізнення",
        missed = "Пропущено епідемій", fa_med = "Тижнів над середнім порогом поза епідемією", delay_med = "Запізнення до середнього порогу, тижнів",
        missed_med = "Епідемій, що не досягли середнього порогу")
sm_out <- sm; names(sm_out) <- ifelse(names(sm_out) %in% names(ru), ru[names(sm_out)], names(sm_out))
wb <- openxlsx::createWorkbook()
for (nm in c("Зведення", "Пороги 2026-27", "Пороги за сезонами", "Докладно")) openxlsx::addWorksheet(wb, nm)
openxlsx::writeData(wb, "Зведення", sm_out); openxlsx::writeData(wb, "Пороги 2026-27", cmp)
openxlsx::writeData(wb, "Пороги за сезонами", do.call(rbind, thr_rows)); openxlsx::writeData(wb, "Докладно", det)
f <- file.path(out_dir, "порівняння_MSD_MEM.xlsx"); openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
write.csv(sm, file.path(out_dir, "зведення.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cmp, file.path(out_dir, "пороги_2026_27.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(det, file.path(out_dir, "докладно.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("Готово:", f, "\n")
