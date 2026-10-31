# Приклади на реальних даних для документації: розрахунок зберігається у output/examples.json
# Запуск: Rscript R/memopt_examples.R [config.json]  (з папки memopt)
args <- commandArgs(trailingOnly = TRUE)
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
setwd(normalizePath(file.path(here, ".."), winslash = "/"))
cfgp <- if (length(args)) args[1] else "config.json"
suppressMessages(source("R/memopt_core.R", encoding = "UTF-8"))
cfg <- mo_load_config(cfgp); terr <- mo_read_territories(cfg)
ex <- list()
lab <- function(i, d) rownames(d)[i]
d <- mo_series(terr[terr$name == "Сумська область", ], cfg)
ex$territory <- "Сумська область"; ex$seasons <- names(d); ex$n_seasons <- NCOL(d)
# кут, обраний для області в останньому розрахунку (з набором сезонів, яким він справді рахувався)
thr_csv <- read.csv(file.path(cfg$.path(cfg$paths$output_dir), "memopt_thresholds.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
A <- as.numeric(thr_csv$angle_txt[thr_csv$id == "UA59"][1])

# 1. кут нахилу: крива MAP і кут на реальному сезоні
angle_ex <- function(season, angles) {
  y <- d[[season]]
  tm <- memtiming(data.frame(y), i.method = 2, i.param = 2.8)
  mc <- tm$map.curve; sl <- tm$slope.curve
  list(season = season, weeks = rownames(d), values = round(y, 1),
       map = data.frame(duration = mc[-1, 1], share = round(mc[-1, 2], 1), slope = round(sl[seq_len(nrow(mc) - 1), 2], 2)),
       angles = do.call(rbind, lapply(angles, function(a) {
         t <- memtiming(data.frame(y), i.method = 2, i.param = a)$optimum.map
         data.frame(angle = a, duration = t[1], share = round(t[2], 1), start_week = lab(t[4], d), end_week = lab(t[5], d), start = t[4], end = t[5])
       })))
}
ex$angle_a <- angle_ex("2024/2025", c(2.25, 2.5, 2.8, 3.25, 3.5))
ex$angle_b <- angle_ex("2025/2026", c(2.25, 2.5, 2.8, 3.25, 3.5))

# 2. пороги: як складаються з даних
m <- mo_thr_model(d, A)
pre <- unlist(m$pre.data.nona); epi <- unlist(m$epi.data.nona)
ex$thr <- list(angle = A, n_values_per_season = m$n.max, pre_n = length(pre), pre_mean = mean(pre), pre_sd = sd(pre), pre_sorted = round(sort(pre, decreasing = TRUE)),
               base = unname(m$pre.post.intervals[1, 3]),
               epi_n = length(epi), epi_geomean = exp(mean(log(epi))), epi_sdlog = sd(log(epi)), epi_sorted = round(sort(epi, decreasing = TRUE)),
               medium = unname(m$epi.intervals[1, 4]), high = unname(m$epi.intervals[2, 4]), veryhigh = unname(m$epi.intervals[3, 4]))
ex$thr$epi_below <- c(sum(epi < ex$thr$medium), sum(epi < ex$thr$high), sum(epi < ex$thr$veryhigh))

# 3. відкладений сезон: поріг з решти сезонів проти сезону, якого він не бачив
hid <- "2024/2025"
dtr <- d[, setdiff(names(d), hid), drop = FALSE]
T <- unname(mo_thr_vec(mo_thr_model(dtr, A))["base"])
y <- d[[hid]]
per_truth <- function(a) { t <- mo_timing(y, a); c(start = unname(t[1]), end = unname(t[2])) }
tr <- do.call(rbind, lapply(c(2.25, 2.5, 2.8, 3.25, 3.5), function(a) { p <- per_truth(a); r <- mo_season_result(y, T, p)
  data.frame(truth_angle = a, start = p[["start"]], end = p[["end"]], start_week = lab(p[["start"]], d), end_week = lab(p[["end"]], d), fa = unname(r["fa"]), delay = unname(r["delay"]), miss = unname(r["miss"])) }))
p28 <- per_truth(2.8)
period <- ifelse(seq_along(y) < p28[["start"]], "доепідемічний", ifelse(seq_along(y) <= p28[["end"]], "епідемічний", "післяепідемічний"))
ex$hidden <- list(season = hid, angle = A, threshold = T, weeks = rownames(d), values = round(y, 1), period = period, above = !is.na(y) & y > T, truth = tr)
ok <- !is.na(y)
ep <- period == "епідемічний"; ab <- !is.na(y) & y > T
TP <- sum(ok & ep & ab); FN <- sum(ok & ep & !ab); FP <- sum(ok & !ep & ab); TN <- sum(ok & !ep & !ab)
ex$hidden$confusion <- c(TP = TP, FN = FN, FP = FP, TN = TN)

# 4. очищення початку сезону на реальній малій території
h <- terr[terr$kind == "small" & grepl("Чупахівська", terr$name), ][1, ]
raw <- mo_series(h, cfg, clean = FALSE); s <- "2025/2026"
yy <- raw[[s]]; names(yy) <- rownames(raw)
steps <- list(); cur <- yy; pos <- which(!is.na(cur))
for (iter in 1:4) {
  idx <- which(!is.na(cur)); i <- idx[1]; ref <- cur[idx[2:(cfg$start_cleaning$compare_weeks + 1)]]
  lim <- median(ref) + cfg$start_cleaning$mad_multiplier * mad(ref)
  steps[[iter]] <- list(week = names(cur)[i], value = unname(cur[i]), median = median(ref), mad = mad(ref), limit = lim, removed = cur[i] > lim)
  if (cur[i] > lim) cur[i] <- NA else break
}
ex$cleaning <- list(territory = h$name, season = s, weeks = names(yy)[!is.na(yy)][1:12], values = round(unname(yy[!is.na(yy)][1:12]), 1), steps = steps)

# 5. коефіцієнт до еталона на реальній малій території
h2 <- terr[terr$kind == "small" & grepl("Недригайлівська", terr$name), ][1, ]
d2 <- mo_series(h2, cfg); r2 <- mo_series(terr[terr$id == h2$reference_id, ], cfg)
ex$k <- list(territory = h2$name, reference = terr$name[terr$id == h2$reference_id], population = h2$population,
             seasons = lapply(names(d2), function(s) list(season = s, hromada_sum = sum(d2[[s]], na.rm = TRUE), reference_sum = sum(r2[rownames(d2), s][!is.na(d2[[s]])], na.rm = TRUE),
                                                          weeks = sum(!is.na(d2[[s]])))),
             k_all = mo_k(unlist(d2), unlist(r2[rownames(d2), names(d2), drop = FALSE]), "all"))

# 6. приклад із хибними сигналами: перебір основних територій
cand <- list()
for (i in which(terr$kind == "main")) {
  di <- mo_series(terr[i, ], cfg)
  if (NCOL(di) < 5) next
  for (hs in names(di)) {
    yy <- di[[hs]]; if (sum(!is.na(yy)) < 10) next
    Tt <- unname(mo_thr_vec(mo_thr_model(di[, setdiff(names(di), hs), drop = FALSE], A))["base"])
    tt <- mo_timing(yy, 2.8); if (is.null(tt) || is.na(Tt)) next
    r <- mo_season_result(yy, Tt, tt)
    cand[[length(cand) + 1]] <- data.frame(name = terr$name[i], season = hs, T = Tt, n_train = NCOL(di) - 1, fa = unname(r["fa"]), delay = unname(r["delay"]), start = tt[[1]], end = tt[[2]])
  }
}
cand <- do.call(rbind, cand); cand <- cand[cand$fa >= 2 & cand$delay >= 1 & cand$delay <= 4, ]
cand <- cand[order(-cand$fa), ]
ex$fa_candidates <- head(cand, 6)

fx <- if (any(cand$name == "м. Охтирка")) cand[cand$name == "м. Охтирка", ][1, ] else cand[1, ]
dx <- mo_series(terr[terr$name == fx$name, ], cfg); yx <- dx[[fx$season]]; tx <- mo_timing(yx, 2.8)
ex$fa_example <- list(name = fx$name, season = fx$season, threshold = fx$T, n_train = fx$n_train, fa = fx$fa, delay = fx$delay, weeks = rownames(dx), values = round(yx, 1),
                      start = tx[[1]], end = tx[[2]], start_week = rownames(dx)[tx[[1]]], end_week = rownames(dx)[tx[[2]]])
dir.create("output", showWarnings = FALSE)
writeLines(jsonlite::toJSON(ex, auto_unbox = TRUE, pretty = TRUE, digits = 6, na = "null"), "output/examples.json", useBytes = TRUE)
cat("saved output/examples.json\n")
