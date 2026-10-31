# memopt: ретроспективна перевірка порогів на сезоні із заданим календарем епідемії.
# Календар (перший і останній тиждень епідемії) задає користувач: це може бути експертна оцінка або
# визначення за незалежними даними (наприклад, лабораторними). Якщо календар є суб'єктивною оцінкою,
# результат відображає саме цю оцінку.
# Запуск: Rscript run_backtest.R <сезон> <перший_тиждень_епідемії> <останній_тиждень_епідемії> [config.json]
#   приклад: Rscript run_backtest.R 2025/2026 2 8
# Результати: output/backtest/<сезон>_*.csv

mo_level_name <- function(v, thr) {
  if (is.na(v)) return(NA_character_)
  if (v >= thr["veryhigh"]) "дуже високий" else if (v >= thr["high"]) "високий" else if (v >= thr["medium"]) "середній" else if (v >= thr["base"]) "низький" else "базовий рівень"
}

# Результат порогу на сезоні за календарем: хибні сигнали (тижні до початку), запізнення (від початку), пропуск
mo_bt_eval <- function(y, base, wk, st, en) {
  pos <- function(w) which(wk == as.character(w))
  ps <- pos(st); pe <- pos(en)
  if (is.na(base)) return(c(fa = NA, delay = NA, miss = NA))
  fa <- sum(y[seq_len(ps - 1)] > base, na.rm = TRUE)
  ep <- y[ps:pe]; first <- which(ep > base)[1]
  c(fa = fa, delay = if (is.na(first)) NA else first - 1, miss = as.numeric(is.na(first)))
}

mo_backtest <- function(cfg_path, season, st, en) {
  cfg <- mo_load_config(cfg_path); terr <- mo_read_territories(cfg)
  out_dir <- file.path(cfg$.path(cfg$paths$output_dir), "backtest"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  tag <- gsub("/", "-", season)
  thr <- readxl::read_excel(file.path(cfg$.path(cfg$paths$output_dir), cfg$paths$workbook_name), sheet = MO_SHEETS$thresholds)
  ser <- list(); for (i in seq_len(nrow(terr))) ser[[terr$id[i]]] <- mo_series(terr[i, ], cfg)
  wk <- rownames(ser[[1]])
  # 1. пороги, побудовані з урахуванням цього сезону (він входить у розрахунок)
  rows <- list()
  for (i in seq_len(nrow(terr))) {
    r <- terr[i, ]; d <- ser[[r$id]]; if (!(season %in% names(d))) next
    y <- d[[season]]; t <- thr[thr$Код == r$id, ]
    th <- c(base = t$`Епідемічний поріг`, medium = t$`Середній поріг`, high = t$`Високий поріг`, veryhigh = t$`Дуже високий поріг`)
    e <- mo_bt_eval(y, th["base"], wk, st, en)
    ps <- which(wk == as.character(st)); pe <- which(wk == as.character(en))
    pk <- max(y[ps:pe], na.rm = TRUE)
    rows[[length(rows) + 1]] <- data.frame(id = r$id, name = r$name, kind = r$kind, base = th["base"], medium = th["medium"], peak_in_epidemic = pk,
      peak_pct_of_base = 100 * pk / th["base"], level_at_peak = mo_level_name(pk, th), fa = e["fa"], delay = e["delay"], miss = e["miss"],
      autumn_median_pct_of_base = 100 * median(y[seq_len(ps - 1)], na.rm = TRUE) / th["base"], row.names = NULL)
  }
  ins <- do.call(rbind, rows); write.csv(ins, file.path(out_dir, paste0(tag, "_in_sample.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  # 2. пороги без цього сезону (сезон відкладений): основні території
  tabs <- readRDS(file.path(cfg$.path(cfg$paths$output_dir), "last_run.rds"))$tabs
  hold_main <- list()
  for (i in which(terr$kind == "main")) {
    r <- terr[i, ]; d <- ser[[r$id]]; if (!(season %in% names(d))) next
    dtr <- d[, setdiff(names(d), season), drop = FALSE]; if (NCOL(dtr) < 2) next
    b <- unname(mo_thr_vec(mo_thr_model(dtr, tabs$angle[tabs$id == r$id]))["base"])
    e <- mo_bt_eval(d[[season]], b, wk, st, en)
    hold_main[[length(hold_main) + 1]] <- data.frame(id = r$id, name = r$name, base = b, fa = e["fa"], delay = e["delay"], miss = e["miss"], row.names = NULL)
  }
  hm <- do.call(rbind, hold_main); write.csv(hm, file.path(out_dir, paste0(tag, "_hidden_main.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  # 3. пороги без цього сезону: малі території, усі способи
  hr <- readRDS(file.path(cfg$.path(cfg$paths$output_dir), "last_run.rds"))$hr
  hs <- terr[terr$kind == "small", ]; hsm <- NULL
  if (nrow(hs)) {
    cl <- mo_start_cluster(cfg); if (!is.null(cl)) on.exit({ parallel::stopCluster(cl); rm("MO_CL", envir = .GlobalEnv) }, add = TRUE)
    preps <- lapply(seq_len(nrow(hs)), function(i) mo_hrom_prep(hs[i, ], ser, cfg, terr))
    tier <- vapply(preps, function(p) mo_tier(p$med_cases, cfg), character(1))
    ang_map <- setNames(tabs$angle, tabs$id)
    # кут власного порогу і придатність власного порогу беруться з робочого розрахунку (так само, як у memopt_core)
    own_ang <- hr$main$angle_own[match(vapply(preps, function(p) p$h$id, ""), hr$main$id)]
    usable <- as.logical(hr$main$own_usable[match(vapply(preps, function(p) p$h$id, ""), hr$main$id)]); usable[is.na(usable)] <- FALSE
    jobs <- Map(function(p, a) list(p = p, own_angle = a, ref_angle = ang_map[[p$refid]]), preps, own_ang)
    res <- mo_pmap(jobs, mo_hrom_job, cfg)
    ths <- list(); for (i in seq_along(res)) if (usable[i]) for (x in res[[i]]$hold) if (x$held == season) ths[[length(ths) + 1]] <- x$th
    fit <- mo_pool_fit(log(vapply(ths, function(t) t$own[1], numeric(1))), log(vapply(ths, function(t) t$refk[1], numeric(1))), vapply(ths, function(t) t$cases, numeric(1)))
    out <- list()
    for (i in seq_along(res)) {
      r <- res[[i]]; p <- r$p; y <- p$d[[season]]; h <- NULL
      for (x in r$hold) if (x$held == season) h <- x
      if (is.null(h)) next
      m <- mo_methods(h$th, tier[i], if (usable[i]) mo_pool_w(fit, h$th$cases) else 0)
      for (nm in names(m)) { b <- unname(m[[nm]][1]); e <- mo_bt_eval(y, b, wk, st, en)
        out[[length(out) + 1]] <- data.frame(id = p$h$id, name = p$h$name, tier = tier[i], method = nm, base = b, fa = e["fa"], delay = e["delay"], miss = e["miss"], row.names = NULL) }
    }
    hsm <- do.call(rbind, out); write.csv(hsm, file.path(out_dir, paste0(tag, "_hidden_small.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(list(in_sample = ins, hidden_main = hm, hidden_small = hsm))
}
