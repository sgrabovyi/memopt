home <- "C:/Диск D/01 - Word/аналіз грві/мем (Грабовий С.Л.)/memopt"
suppressMessages(source(file.path(home, "R", "memopt_core.R"), encoding = "UTF-8"))
cfg <- mo_load_config(file.path(home, "config.json")); cfg$.season_sets <- NULL
terr <- mo_read_territories(cfg)
d <- mo_series(terr[terr$id == "UA59", ], cfg, all_seasons = TRUE)
A <- 2.75; war <- c("2023/2024", "2024/2025", "2025/2026"); pre <- rev(c("2012/2013", "2013/2014", "2014/2015", "2017/2018", "2018/2019"))
sets <- list(war); for (i in seq_along(pre)) sets[[i + 1]] <- c(rev(pre[1:i]), war)
lev <- function(x, T) if (is.na(T["base"])) NA else if (x < T["base"]) "нижче епід." else if (x < T["medium"]) "низький" else if (x < T["high"]) "середній" else if (x < T["veryhigh"]) "високий" else "дуже високий"
for (S in sets) {
  cat("\n######## набір:", length(S), "сезонів:", paste(S, collapse = ", "), "\n")
  # 1. вперед у часі
  for (t in c("2024/2025", "2025/2026")) {
    tr <- S[seq_len(match(t, S) - 1)]
    T <- if (length(tr) >= 2) mo_thr_vec(mo_thr_model(d[, tr, drop = FALSE], A)) else c(base = NA)
    y <- d[[t]]; tm <- mo_timing(y, 2.8); r <- mo_season_result(y, unname(T["base"]), tm)
    cat(sprintf("  [1] перевірка %s, побудова на %d попередніх: епід.поріг %s, хибних %d, запізнення %d%s\n", t, length(tr), ifelse(is.na(T["base"]), "не будується", round(T["base"])), r["fa"], r["delay"], ifelse(r["failed"] == 1, " (поріг не побудовано)", "")))
  }
  # 2. MEM goodness
  g <- mo_good_vec(d[, S, drop = FALSE], A, 3)
  cat(sprintf("  [2] memgoodness: чутливість %.2f, специфічність %.2f, MCC %.2f\n", g["sens"], g["spec"], g["mcc"]))
  # 3. стійкість
  full <- mo_thr_vec(mo_thr_model(d[, S, drop = FALSE], A))
  loo <- sapply(S, function(s) mo_thr_vec(mo_thr_model(d[, setdiff(S, s), drop = FALSE], A))["base"])
  cat(sprintf("  [3] епід.поріг %.0f; без одного сезону від %.0f до %.0f (розмах %.0f%%)\n", full["base"], min(loo, na.rm = TRUE), max(loo, na.rm = TRUE), 100 * (max(loo, na.rm = TRUE) - min(loo, na.rm = TRUE)) / full["base"]))
  # 4. інтенсивність
  cat(sprintf("  [4] пороги %0.f/%0.f/%0.f/%0.f; піки: 2024/25 %.0f = %s, 2025/26 %.0f = %s\n", full["base"], full["medium"], full["high"], full["veryhigh"],
              max(d[["2024/2025"]], na.rm = TRUE), lev(max(d[["2024/2025"]], na.rm = TRUE), full), max(d[["2025/2026"]], na.rm = TRUE), lev(max(d[["2025/2026"]], na.rm = TRUE), full)))
}
