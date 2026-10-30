# memopt: калібрування параметрів на власних даних.
# Три інструменти: межа падіння базового рівня (stability_drop), множник MAD для очищення початку сезону,
# чутливість вибору кута до кута, яким визначається справжній початок епідемії (truth_angle).
# Запуск: Rscript run_calibration.R <drop|cleaning|truth> [config.json]

# ---------------------------------------------------------------- 1. межа стабільності: вкладена перехресна перевірка
# Для кожної основної території і кожного сезону s: сезон відкладається, на решті проходить ВЕСЬ вибір кута
# (скан, межа стабільності, мінімум сумарної помилки) для кожного значення drop; поріг з обраним кутом перевіряється на сезоні s.
# Так оцінюється якість усієї процедури на даних, яких вона не бачила.
mo_cal_drop_job <- function(j, cfg) {
  d <- j$d; S <- names(d); out <- list()
  for (s in S) {
    y <- d[[s]]
    if (sum(!is.na(y)) < 10) next
    tm <- mo_timing(y, cfg$angle$truth_angle)
    if (is.null(tm) || any(is.na(tm))) next
    dtr <- d[, setdiff(S, s), drop = FALSE]
    if (NCOL(dtr) < cfg$angle$min_seasons_goodness) next
    scan <- mo_scan_angles(dtr, cfg)
    for (dr in j$drops) {
      sel <- mo_select_angle(scan, cfg, dr)
      T <- unname(mo_thr_vec(mo_thr_model(dtr, sel$angle))["base"])
      r <- mo_season_result(y, T, tm)
      out[[length(out) + 1]] <- data.frame(id = j$id, name = j$name, held = s, drop = dr, angle = sel$angle, floor = sel$floor,
                                           fa = unname(r["fa"]), delay = unname(r["delay"]), miss = unname(r["miss"]), failed = unname(r["failed"]))
    }
  }
  if (length(out)) do.call(rbind, out) else NULL
}

mo_calibrate_drop <- function(cfg_path, drops = c(0.05, 0.10, 0.15, 0.20, 0.30, 0.40, 0.60, 1.00)) {
  cfg <- mo_load_config(cfg_path)
  terr <- mo_read_territories(cfg)
  cl <- mo_start_cluster(cfg)
  if (!is.null(cl)) on.exit({ parallel::stopCluster(cl); rm("MO_CL", envir = .GlobalEnv) }, add = TRUE)
  main <- terr[terr$kind == "main", ]
  jobs <- lapply(seq_len(nrow(main)), function(i) list(id = main$id[i], name = main$name[i], d = mo_series(main[i, ], cfg), drops = drops))
  res <- do.call(rbind, mo_pmap(jobs, mo_cal_drop_job, cfg))
  res$loss <- cfg$angle$weights$false_alert * res$fa + cfg$angle$weights$delay * res$delay
  summ <- res %>% group_by(drop) %>% summarise(trials = n(), fa = mean(fa), delay = mean(delay), pct_miss = 100 * mean(miss), loss = mean(loss),
                                               mean_angle = mean(angle), mean_floor = mean(floor), .groups = "drop")
  # довірчі межі: бутстреп по територіях (кластери), різниця сумарної помилки з найкращим значенням
  set.seed(1)
  ids <- unique(res$id)
  per <- res %>% group_by(drop, id) %>% summarise(s = sum(loss), n = n(), .groups = "drop")
  boot_loss <- function(dr) {
    p <- per[per$drop == dr, ]
    replicate(2000, { k <- sample(seq_len(nrow(p)), replace = TRUE); sum(p$s[k]) / sum(p$n[k]) })
  }
  bl <- lapply(drops, boot_loss); names(bl) <- as.character(drops)
  best <- summ$drop[which.min(summ$loss)]
  summ$se_boot <- sapply(as.character(drops), function(x) sd(bl[[x]]))
  summ$diff_vs_best <- summ$loss - min(summ$loss)
  one_se <- min(summ$loss) + summ$se_boot[summ$drop == best]
  allowed <- summ$drop[summ$loss <= one_se]
  list(summary = summ, details = res, best = best, one_se_rule = min(allowed), allowed_by_one_se = allowed)
}

# ---------------------------------------------------------------- 2. очищення початку сезону
# Оцінка на СИРИХ даних схованого сезону (щоб прибрані тижні не зникали з оцінки хибних сигналів).
mo_cal_clean_job <- function(j, cfg) {
  out <- list()
  raw <- j$raw; S <- names(raw)
  tms <- lapply(S, function(s) { y <- raw[[s]]; if (sum(!is.na(y)) >= 10) mo_timing(y, cfg$angle$truth_angle) else NULL }); names(tms) <- S
  for (v in j$variants) {
    dc <- if (is.na(v$mult)) raw else v$dc
    removed <- sum(is.na(dc) & !is.na(raw))
    for (s in S) {
      tm <- tms[[s]]
      if (is.null(tm) || any(is.na(tm))) next
      T <- unname(mo_thr_vec(mo_thr_model(dc[, setdiff(S, s), drop = FALSE], j$angle))["base"])
      r <- mo_season_result(raw[[s]], T, tm)
      out[[length(out) + 1]] <- data.frame(id = j$id, variant = v$label, held = s, T = T, fa = unname(r["fa"]), delay = unname(r["delay"]),
                                           miss = unname(r["miss"]), failed = unname(r["failed"]), removed = removed)
    }
  }
  if (length(out)) do.call(rbind, out) else NULL
}

mo_calibrate_cleaning <- function(cfg_path, multipliers = c(1.5, 2, 2.5, 3), compare = c(6, 8, 10), angle = 3.0) {
  cfg <- mo_load_config(cfg_path)
  terr <- mo_read_territories(cfg)
  cl <- mo_start_cluster(cfg)
  if (!is.null(cl)) on.exit({ parallel::stopCluster(cl); rm("MO_CL", envir = .GlobalEnv) }, add = TRUE)
  small <- terr[terr$kind == "small", ]
  variants_def <- list(list(label = "без очищення", mult = NA, cw = NA))
  for (m in multipliers) variants_def[[length(variants_def) + 1]] <- list(label = paste0("MAD x ", m, ", порівняння ", 8), mult = m, cw = 8)
  for (cw in setdiff(compare, 8)) variants_def[[length(variants_def) + 1]] <- list(label = paste0("MAD x 2, порівняння ", cw), mult = 2, cw = cw)
  jobs <- lapply(seq_len(nrow(small)), function(i) {
    raw <- mo_series(small[i, ], cfg, clean = FALSE)
    vs <- lapply(variants_def, function(v) list(label = v$label, mult = v$mult, dc = if (is.na(v$mult)) NULL else mo_series(small[i, ], cfg, clean = TRUE, multiplier = v$mult, compare_weeks = v$cw)))
    list(id = small$id[i], raw = raw, variants = vs, angle = angle)
  })
  res <- do.call(rbind, mo_pmap(jobs, mo_cal_clean_job, cfg))
  res$loss <- cfg$angle$weights$false_alert * res$fa + cfg$angle$weights$delay * res$delay
  base_T <- res[res$variant == "без очищення", c("id", "held", "T")]; names(base_T)[3] <- "T0"
  res <- merge(res, base_T, by = c("id", "held"))
  summ <- res %>% group_by(variant) %>% summarise(trials = n(), weeks_removed = sum(removed[!duplicated(id)]), fa = mean(fa), delay = mean(delay),
                                                  pct_miss = 100 * mean(miss), loss = mean(loss), pct_changed_threshold = 100 * mean(abs(T / T0 - 1) > 0.02, na.rm = TRUE),
                                                  median_abs_change_pct = 100 * median(abs(T / T0 - 1), na.rm = TRUE), .groups = "drop")
  list(summary = summ, details = res)
}

# ---------------------------------------------------------------- 3. чутливість до кута "істини"
mo_sensitivity_truth <- function(cfg_path, truths = c(2.5, 3.0)) {
  cfg0 <- mo_load_config(cfg_path)
  raw <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
  out <- list()
  for (tv in truths) {
    cfg_j <- raw
    cfg_j$angle$truth_angle <- tv
    cfg_j$paths$output_dir <- file.path("output", "sensitivity", paste0("truth_", tv))
    cfg_j$paths$series_dir <- cfg0$paths$series_dir
    cfg_j$export$dashboard_dim_thresholds <- FALSE
    tmp <- file.path(cfg0$.dir, paste0("config_tmp_truth_", tv, ".json"))
    writeLines(jsonlite::toJSON(cfg_j, auto_unbox = TRUE, pretty = TRUE, null = "null"), tmp, useBytes = TRUE)
    mo_run(tmp)
    unlink(tmp)
    r <- readRDS(file.path(cfg0$.path(cfg_j$paths$output_dir), "last_run.rds"))
    out[[as.character(tv)]] <- r
  }
  out
}
