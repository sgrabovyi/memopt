# Перевірка memopt незалежною розміткою епідемічних тижнів (модель двох прихованих станів, R/memopt_hmm.R).
# 1) Для кожної території і сезону порівнює межі епідемії за MEM (memtiming, кут 2,8, як у доборі сезонів) і за моделлю.
# 2) Для області, районів і міст повторює добір сезонів, але межі епідемії і сезони без епідемії бере з моделі,
#    і показує, чи збігається обраний набір з тим, що обрав memopt.
# Запуск: Rscript run_hmm_check.R [config.json]. Результат: output/hmm_check/перевірка_розміткою_HMM.xlsx
# Не входить у пакет для ЦГЗ. Потрібен пакет depmixS4.
args <- commandArgs(trailingOnly = TRUE)
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
cfg_path <- if (length(args)) args[1] else file.path(here, "config.json")
MO_CORE <- normalizePath(file.path(here, "R", "memopt_core.R"), winslash = "/")
source(MO_CORE, encoding = "UTF-8")
source(file.path(here, "R", "memopt_hmm.R"), encoding = "UTF-8")

cfg <- mo_load_config(cfg_path)
cfg$angle$truth_angle <- as.numeric(cfg$angle$truth_angle)
out_dir <- file.path(cfg$.path(cfg$paths$output_dir), "hmm_check")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
terr <- mo_read_territories(cfg)
cfg$.season_sets <- NULL
weeks <- mo_weeks(cfg)
wk <- function(i) ifelse(is.na(i), NA, weeks[pmax(1, pmin(length(weeks), i))])

cat("1. Модель двох станів для кожної території\n"); flush.console()
ser <- list(); hmm <- list(); model_rows <- list(); bound_rows <- list()
for (i in seq_len(nrow(terr))) {
  t <- terr[i, ]
  d <- mo_series(t, cfg, all_seasons = TRUE)
  if (NCOL(d) < 2) next
  h <- mo_hmm_fit(d)
  if (is.null(h)) { cat("  модель не зійшлась:", t$name, "\n"); next }
  ser[[t$id]] <- d; tr <- mo_hmm_truth(h); hmm[[t$id]] <- tr
  model_rows[[t$id]] <- data.frame(id = t$id, name = t$name, level = t$level, seasons = NCOL(d),
    mean_low = round(h$mean_low, 1), mean_epi = round(h$mean_epi, 1), stay_epi = round(h$stay_epi, 3),
    epi_weeks_avg = round(mean(colSums(h$prob > 0.5, na.rm = TRUE)), 1))
  mem_noepi <- if (t$kind == "main") mo_noepi_seasons(d, cfg) else character(0)
  for (s in names(d)) {
    tm <- mo_season_timing(d[[s]], cfg); if (is.null(tm)) tm <- c(NA, NA)
    hp <- tr$tms[[s]]
    a <- if (all(!is.na(tm))) tm[1]:tm[2] else integer(0)
    b <- if (all(!is.na(hp))) hp[1]:hp[2] else integer(0)
    jac <- if (length(union(a, b))) length(intersect(a, b)) / length(union(a, b)) else NA
    bound_rows[[length(bound_rows) + 1]] <- data.frame(id = t$id, name = t$name, level = t$level, season = s,
      mem_start = wk(tm[1]), mem_end = wk(tm[2]), mem_weeks = length(a), mem_no_epidemic = s %in% mem_noepi,
      hmm_start = wk(hp[1]), hmm_end = wk(hp[2]), hmm_weeks = length(b), hmm_no_epidemic = s %in% tr$noepi,
      start_diff = unname(hp[1] - tm[1]), end_diff = unname(hp[2] - tm[2]), overlap = round(jac, 2))
  }
}
bounds <- do.call(rbind, bound_rows); models <- do.call(rbind, model_rows)

cat("2. Добір сезонів: межі епідемії за MEM і за моделлю\n"); flush.console()
cl <- mo_start_cluster(cfg)
if (!is.null(cl)) on.exit(parallel::stopCluster(cl), add = TRUE)
main <- terr[terr$kind == "main" & terr$id %in% names(hmm), ]
items <- lapply(main$id, function(id) list(d = ser[[id]], tms = hmm[[id]]$tms, noepi = hmm[[id]]$noepi))
sel_mem <- mo_pmap(items, function(it, cfg) mo_select_seasons(it$d, cfg), cfg)
sel_hmm <- mo_pmap(items, function(it, cfg) { cfg$.truth_tms <- it$tms; cfg$.truth_noepi <- it$noepi; mo_select_seasons(it$d, cfg) }, cfg)
pick <- function(s) { r <- s$table[s$table$chosen, ][1, ]; c(r$base, r$medium, r$angle) }
sel <- do.call(rbind, lapply(seq_along(main$id), function(i) {
  a <- sel_mem[[i]]; b <- sel_hmm[[i]]; pa <- pick(a); pb <- pick(b)
  data.frame(id = main$id[i], name = main$name[i], available = NCOL(ser[[main$id[i]]]),
    mem_seasons = paste(a$seasons, collapse = ", "), hmm_seasons = paste(b$seasons, collapse = ", "),
    same_set = identical(a$seasons, b$seasons),
    mem_no_epidemic = paste(a$noepi, collapse = ", "), hmm_no_epidemic = paste(b$noepi, collapse = ", "),
    mem_angle = pa[3], hmm_angle = pb[3], mem_base = round(pa[1], 1), hmm_base = round(pb[1], 1),
    base_diff_pct = round((pb[1] / pa[1] - 1) * 100, 1))
}))

lab <- list(
  bounds = c(id = "Код", name = "Територія", level = "Рівень", season = "Сезон", mem_start = "Початок за MEM (тиждень)",
    mem_end = "Кінець за MEM", mem_weeks = "Тижнів за MEM", mem_no_epidemic = "Без епідемії за MEM",
    hmm_start = "Початок за моделлю", hmm_end = "Кінець за моделлю", hmm_weeks = "Тижнів за моделлю",
    hmm_no_epidemic = "Без епідемії за моделлю", start_diff = "Зсув початку, тижнів (модель мінус MEM)",
    end_diff = "Зсув кінця, тижнів", overlap = "Збіг періодів (0-1)"),
  sel = c(id = "Код", name = "Територія", available = "Сезонів доступно", mem_seasons = "Набір за межами MEM (як у memopt)",
    hmm_seasons = "Набір за межами моделі", same_set = "Набори збігаються", mem_no_epidemic = "Сезони без епідемії за MEM",
    hmm_no_epidemic = "Сезони без епідемії за моделлю", mem_angle = "Кут (MEM)", hmm_angle = "Кут (модель)",
    mem_base = "Епідемічний поріг (MEM)", hmm_base = "Епідемічний поріг (модель)", base_diff_pct = "Різниця порогу, %"),
  models = c(id = "Код", name = "Територія", level = "Рівень", seasons = "Сезонів", mean_low = "Середнє, звичайний стан",
    mean_epi = "Середнє, епідемічний стан", stay_epi = "Імовірність лишитись в епідемії наступного тижня",
    epi_weeks_avg = "Епідемічних тижнів у сезоні, середнє"))
rn <- function(x, l) { names(x) <- l[names(x)]; x }
wb <- openxlsx::createWorkbook()
for (sh in list(list("Добір сезонів", rn(sel, lab$sel)), list("Межі епідемії", rn(bounds, lab$bounds)), list("Модель", rn(models, lab$models)))) {
  openxlsx::addWorksheet(wb, sh[[1]]); openxlsx::writeDataTable(wb, sh[[1]], sh[[2]]); openxlsx::setColWidths(wb, sh[[1]], 1:ncol(sh[[2]]), "auto")
}
f <- file.path(out_dir, "перевірка_розміткою_HMM.xlsx")
openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
write.csv(bounds, file.path(out_dir, "bounds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sel, file.path(out_dir, "selection.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Набори сезонів збіглися в %d з %d територій\n", sum(sel$same_set), nrow(sel)))
cat("Готово:", f, "\n")
