home <- "C:/Диск D/01 - Word/аналіз грві/мем (Грабовий С.Л.)/memopt"
suppressMessages(source(file.path(home, "R", "memopt_core.R"), encoding = "UTF-8"))
cfg <- mo_load_config(file.path(home, "config.json")); cfg$.season_sets <- NULL
terr <- mo_read_territories(cfg)
d <- mo_series(terr[terr$id == "UA59", ], cfg, all_seasons = TRUE)
y <- d[["2025/2026"]]; w <- rownames(d)
yf <- mem:::fill.missing(y)
cat("weeks :", paste(w, collapse = " "), "\n")
cat("values:", paste(ifelse(is.na(y), paste0("(", round(yf), ")"), round(y)), collapse = " "), "\n")
tot <- sum(yf)
for (a in c(2.5, 2.8, 3.0)) {
  tm <- memtiming(data.frame(y), i.method = 2, i.param = a)
  mc <- tm$map.curve; om <- tm$optimum.map
  cat(sprintf("angle %.1f: weeks %s to %s, %d weeks, %.0f%% of season total\n", a, w[om[4]], w[om[5]], om[5] - om[4] + 1, 100 * sum(yf[om[4]:om[5]]) / tot))
}
tm <- memtiming(data.frame(y), i.method = 2, i.param = 2.8)
mc <- tm$map.curve
cat("MAP curve (duration, % of total) first 22:\n"); print(round(head(mc, 22), 1))
