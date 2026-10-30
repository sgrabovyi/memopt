home <- "C:/Диск D/01 - Word/аналіз грві/мем (Грабовий С.Л.)/memopt"
suppressMessages(source(file.path(home, "R", "memopt_core.R"), encoding = "UTF-8"))
cfg <- mo_load_config(file.path(home, "config.json")); cfg$.season_sets <- NULL
terr <- mo_read_territories(cfg)
d <- mo_series(terr[terr$id == "UA59", ], cfg, all_seasons = TRUE)
war <- c("2023/2024", "2024/2025", "2025/2026"); pre <- rev(c("2012/2013", "2013/2014", "2014/2015", "2017/2018", "2018/2019"))
sets <- list(war); for (i in seq_along(pre)) sets[[i + 1]] <- c(rev(pre[1:i]), war)
t <- "2025/2026"; y <- d[[t]]; tm <- mo_timing(y, 2.8)
for (S in sets) {
  tr <- setdiff(S, t); T <- unname(mo_thr_vec(mo_thr_model(d[, tr, drop = FALSE], 2.75))["base"])
  ok <- !is.na(y); i <- seq_along(y)
  before <- sum(ok & i < tm[1] & y > T); after <- sum(ok & i > tm[2] & y > T)
  ine <- which(ok & i >= tm[1] & i <= tm[2] & y > T); delay <- if (length(ine)) ine[1] - tm[1] else tm[2] - tm[1] + 1
  cat(sprintf("%d сезонів | поріг %.0f | хибні до %d після %d | запізнення %d | сума %d\n", length(S), T, before, after, delay, before + after + delay))
}
