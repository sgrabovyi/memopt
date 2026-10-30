home <- "C:/Диск D/01 - Word/аналіз грві/мем (Грабовий С.Л.)/memopt"
suppressMessages(source(file.path(home, "R", "memopt_core.R"), encoding = "UTF-8"))
cfg <- mo_load_config(file.path(home, "config_auto_seasons_test.json")); cfg$.season_sets <- NULL
terr <- mo_read_territories(cfg); main <- terr[terr$kind == "main", ]
old <- read.csv(file.path(home, "output/memopt_thresholds.csv"), encoding = "UTF-8")
new <- read.csv(file.path(home, "output/auto_seasons_test/memopt_thresholds.csv"), encoding = "UTF-8")
cat(sprintf("%-32s %6s %6s %6s | %s\n", "territory", "12/13", "13/14", "14/15", "weeks 2025/26 above base: 5 seasons / auto (base 5 / auto)"))
for (i in seq_len(nrow(main))) {
  d <- mo_series(main[i, ], cfg, all_seasons = TRUE); V <- tail(names(d), 5); ref <- mean(unlist(d[, V]), na.rm = TRUE)
  r <- sapply(c("2012/2013", "2013/2014", "2014/2015"), function(s) if (s %in% names(d)) mean(d[[s]], na.rm = TRUE) / ref else NA)
  y <- d[["2025/2026"]]; b5 <- old$base[old$id == main$id[i]]; ba <- new$base[new$id == main$id[i]]
  cat(sprintf("%-32s %6.2f %6.2f %6.2f | %2d / %2d  (%.0f / %.0f)\n", main$name[i], r[1], r[2], r[3], sum(y > b5, na.rm = TRUE), sum(y > ba, na.rm = TRUE), b5, ba))
}
