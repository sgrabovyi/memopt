# Порівняння порогів memopt, QUEST і простого MEM на сезонах, відкладених для контролю.
# Для кожного контрольного сезону (сезони від era_start, крім not_validation) пороги будуються без нього:
#   memopt: повний розрахунок memopt (добір сезонів, кут, громади з еталоном) з цим сезоном у виключених;
#   QUEST: модель двох станів на всіх придатних сезонах території, пороги 5% і 70% (як epiquest, Sciensano 2025);
#   простий MEM: усі придатні сезони, стандартний кут 2,8, без добору.
# Пороги прикладаються до відкладеного сезону. Межі епідемії в ньому визначаються двома способами:
#   за MEM (memtiming, кут 2,8, як у memopt) і за моделлю двох станів (R/memopt_hmm.R). Перша розмітка ближча до MEM,
#   друга до QUEST, тому висновок вартий довіри, лише якщо він однаковий за обома.
# Запуск: Rscript run_quest_check.R [config.json]. Результат: output/quest_check/порівняння_memopt_QUEST_MEM.xlsx
# Не входить у пакет для ЦГЗ. Потрібен пакет depmixS4. Робочих порогів не змінює.
args <- commandArgs(trailingOnly = TRUE)
here <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])), winslash = "/")
cfg_path <- if (length(args)) args[1] else file.path(here, "config.json")
MO_CORE <- normalizePath(file.path(here, "R", "memopt_core.R"), winslash = "/")
source(MO_CORE, encoding = "UTF-8")
source(file.path(here, "R", "memopt_hmm.R"), encoding = "UTF-8")

cfg <- mo_load_config(cfg_path)
cfg$angle$truth_angle <- as.numeric(cfg$angle$truth_angle)
out_dir <- file.path(cfg$.path(cfg$paths$output_dir), "quest_check")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
terr <- mo_read_territories(cfg)
cfg$.season_sets <- NULL
ss <- cfg$season_selection
all_seasons <- unique(unlist(lapply(seq_len(nrow(terr)), function(i) names(mo_series(terr[i, ], cfg, all_seasons = TRUE)))))
V <- sort(setdiff(all_seasons[all_seasons >= ss$era_start], ss$not_validation))
cat("Контрольні сезони:", paste(V, collapse = ", "), "\n"); flush.console()

# ---------------------------------------------------------------- 1. memopt без контрольного сезону
raw <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
abs_p <- function(p) if (mo_is_abs(p)) p else file.path(dirname(normalizePath(cfg_path, winslash = "/")), p)
memopt_thr <- list()
for (s in V) {
  od <- file.path(out_dir, paste0("memopt_без_", gsub("/", "_", s)))
  f <- file.path(od, "memopt_thresholds.csv")
  if (!file.exists(f)) {
    cat("1. memopt без сезону", s, "\n"); flush.console()
    dir.create(od, showWarnings = FALSE, recursive = TRUE)
    c2 <- raw
    c2$paths$output_dir <- od
    c2$exclude_seasons <- c(raw$exclude_seasons, setNames(list("відкладено для перевірки"), s))
    c2$export$dashboard_dim_thresholds <- FALSE
    # тимчасовий конфіг поруч із робочим, щоб ряди шукались так само (series_dir і папка input)
    cf <- file.path(here, paste0(".quest_check_", gsub("/", "_", s), ".json"))
    writeLines(jsonlite::toJSON(c2, auto_unbox = TRUE, pretty = TRUE, null = "null"), cf, useBytes = TRUE)
    invisible(capture.output(mo_run(cf)))
    file.copy(cf, file.path(od, "config.json"), overwrite = TRUE); unlink(cf)
  } else cat("1. memopt без сезону", s, ": узято готовий розрахунок\n")
  memopt_thr[[s]] <- read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------- 2. QUEST, простий MEM і перевірка
quest_thr <- function(d, Q = c(0.05, 0.70)) {
  h <- mo_hmm_fit(d)
  if (is.null(h)) return(c(base = NA, medium = NA))
  y <- unlist(d, use.names = FALSE); p <- as.vector(h$prob); ok <- !is.na(y) & !is.na(p)
  w <- tapply(p[ok], y[ok], sum); r <- as.numeric(names(w))
  cw <- cumsum(w) / sum(w); keep <- cw > .001
  if (sum(keep) < 2) return(c(base = NA, medium = NA))
  th <- approx(cw[keep], r[keep], xout = Q, ties = mean)$y
  c(base = th[1], medium = th[2])
}

# Результат порогів (епідемічний і середній) на одному сезоні при заданих межах епідемії
judge <- function(y, Tb, Tm, tm, noepi) {
  ok <- !is.na(y); n <- length(y)
  inp <- if (noepi || is.null(tm) || any(is.na(tm))) rep(FALSE, n) else seq_len(n) >= tm[1] & seq_len(n) <= tm[2]
  len <- sum(inp)
  one <- function(T) {
    if (is.na(T)) return(c(fa = NA, delay = if (len) len else 0, miss = as.numeric(len > 0), tp = 0, fp = NA, fn = len, tn = NA))
    up <- ok & y > T
    first <- which(inp & up)
    c(fa = sum(up & !inp), delay = if (!len) 0 else if (length(first)) first[1] - which(inp)[1] else len,
      miss = as.numeric(len > 0 && !length(first)), tp = sum(up & inp), fp = sum(up & !inp), fn = sum(ok & inp & !up), tn = sum(ok & !inp & !up))
  }
  b <- one(Tb); m <- one(Tm)
  c(epi_weeks = len, noepi = as.numeric(noepi), b, setNames(m[c("fa", "delay", "miss")], c("fa_med", "delay_med", "miss_med")))
}

job <- function(it) {
  t <- it$row; d <- it$d; V <- it$V; cfg <- it$cfg; mthr <- it$mthr
  S <- names(d)
  h <- if (NCOL(d) >= 2) mo_hmm_fit(d) else NULL
  ht <- if (!is.null(h)) mo_hmm_truth(h) else NULL
  noepi_mem <- mo_noepi_seasons(d, cfg)
  rows <- list()
  for (s in intersect(V, S)) {
    y <- d[[s]]; rest <- d[, setdiff(S, s), drop = FALSE]
    q <- if (NCOL(rest) >= 2) quest_thr(rest) else c(base = NA, medium = NA)
    m <- if (NCOL(rest) >= 2) mo_thr_vec(mo_thr_model(rest, 2.8)) else c(base = NA, medium = NA)
    mt <- mthr[[s]]; k <- match(t$id, mt$id)
    thr <- list("memopt" = c(mt$base[k], mt$medium[k]), "QUEST" = unname(q[c("base", "medium")]), "простий MEM" = unname(m[c("base", "medium")]))
    truths <- list("за MEM" = list(tm = mo_season_timing(y, cfg), noepi = s %in% noepi_mem))
    if (!is.null(ht)) truths[["за моделлю двох станів"]] <- list(tm = ht$tms[[s]], noepi = s %in% ht$noepi)
    for (tn in names(truths)) for (mn in names(thr)) {
      r <- judge(y, thr[[mn]][1], thr[[mn]][2], truths[[tn]]$tm, truths[[tn]]$noepi)
      rows[[length(rows) + 1]] <- data.frame(id = t$id, name = t$name, level = t$level, held = s, truth = tn, method = mn,
        seasons_quest = NCOL(rest), thr_base = round(thr[[mn]][1], 1), thr_medium = round(thr[[mn]][2], 1), as.list(r), row.names = NULL)
    }
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}

cat("2. QUEST і простий MEM для", nrow(terr), "територій\n"); flush.console()
items <- lapply(seq_len(nrow(terr)), function(i) list(row = terr[i, ], d = mo_series(terr[i, ], cfg, all_seasons = TRUE), V = V, cfg = cfg, mthr = memopt_thr))
nw <- cfg$parallel$workers
cl <- parallel::makeCluster(nw)
parallel::clusterExport(cl, c("MO_CORE", "here"), envir = environment())
invisible(parallel::clusterEvalQ(cl, { source(MO_CORE, encoding = "UTF-8"); source(file.path(here, "R", "memopt_hmm.R"), encoding = "UTF-8") }))
parallel::clusterExport(cl, c("quest_thr", "judge"), envir = environment())
res <- parallel::parLapply(cl, items, job)
parallel::stopCluster(cl)
det <- do.call(rbind, res)

# ---------------------------------------------------------------- 3. зведення
lv <- c("область", "район", "район без центру", "місто", "громада")
det$group <- ifelse(det$level %in% lv, det$level, "громада")
mcc <- function(tp, fp, fn, tn) { num <- tp * tn - fp * fn; den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)); ifelse(den > 0, num / den, NA) }
summ <- function(x) {
  data.frame(territory_seasons = nrow(x), no_threshold = sum(is.na(x$thr_base)),
    fa = sum(x$fa, na.rm = TRUE), delay = sum(x$delay, na.rm = TRUE), loss = sum(x$fa, na.rm = TRUE) + sum(x$delay, na.rm = TRUE),
    missed = sum(x$miss, na.rm = TRUE), mcc = round(mcc(sum(x$tp, na.rm = TRUE), sum(x$fp, na.rm = TRUE), sum(x$fn, na.rm = TRUE), sum(x$tn, na.rm = TRUE)), 3),
    fa_med = sum(x$fa_med, na.rm = TRUE), delay_med = sum(x$delay_med, na.rm = TRUE), missed_med = sum(x$miss_med, na.rm = TRUE))
}
key <- unique(det[, c("group", "truth", "method")])
key$group <- factor(key$group, levels = lv); key <- key[order(key$truth, key$group, key$method), ]
sm <- do.call(rbind, lapply(seq_len(nrow(key)), function(i) {
  x <- det[det$group == key$group[i] & det$truth == key$truth[i] & det$method == key$method[i], ]
  data.frame(key[i, ], summ(x), row.names = NULL)
}))
# переможець за сумарною помилкою на кожній території і сезоні
w <- do.call(rbind, lapply(split(det, list(det$id, det$held, det$truth), drop = TRUE), function(x) {
  l <- ifelse(is.na(x$thr_base), Inf, x$fa + x$delay); b <- min(l)
  data.frame(id = x$id[1], group = x$group[1], held = x$held[1], truth = x$truth[1], winners = paste(x$method[l == b], collapse = " і "))
}))
wins <- as.data.frame.matrix(table(paste(w$truth, w$group, sep = ": "), w$winners))

ru <- c(group = "Рівень", truth = "Межі епідемії", method = "Спосіб", territory_seasons = "Територій-сезонів", no_threshold = "Поріг не побудовано",
        fa = "Хибних тижнів (епідемічний поріг)", delay = "Тижнів запізнення (епідемічний поріг)", loss = "Сумарна помилка",
        missed = "Пропущено епідемій", mcc = "MCC", fa_med = "Тижнів над середнім порогом поза епідемією", delay_med = "Тижнів запізнення до середнього порогу",
        missed_med = "Епідемій, що не досягли середнього порогу")
names(sm) <- ifelse(names(sm) %in% names(ru), ru[names(sm)], names(sm))
wb <- openxlsx::createWorkbook()
for (nm in c("Зведення", "Хто кращий", "Докладно")) openxlsx::addWorksheet(wb, nm)
openxlsx::writeData(wb, "Зведення", sm)
openxlsx::writeData(wb, "Хто кращий", data.frame(`Межі і рівень` = rownames(wins), wins, check.names = FALSE))
openxlsx::writeData(wb, "Докладно", det)
f <- file.path(out_dir, "порівняння_memopt_QUEST_MEM.xlsx")
openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
write.csv(det, file.path(out_dir, "докладно.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(sm[, 1:10], row.names = FALSE)
cat("\nХто кращий (найменша сумарна помилка на території і сезоні):\n"); print(wins)
cat("Готово:", f, "\n")
