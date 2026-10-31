# Калібрування параметрів на власних даних.
# Запуск: Rscript run_calibration.R <drop|cleaning|truth> [config.json]
#   drop     межа падіння епідемічного порогу (stability_drop): вкладена перехресна перевірка
#   cleaning множник MAD і кількість тижнів порівняння для очищення початку сезону
#   truth    чутливість вибору кута до кута, яким визначається справжній початок епідемії
args <- commandArgs(trailingOnly = TRUE)
what <- if (length(args)) args[1] else "drop"
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
cfg <- if (length(args) > 1) args[2] else file.path(here, "config.json")
MO_CORE <- normalizePath(file.path(here, "R", "memopt_core.R"), winslash = "/")
source(MO_CORE, encoding = "UTF-8")
source(file.path(here, "R", "memopt_calibration.R"), encoding = "UTF-8")
out_dir <- file.path(here, "output", "calibration"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if (what == "drop") {
  r <- mo_calibrate_drop(cfg)
  print(as.data.frame(r$summary)); cat("Найкраще значення:", r$best, "; правило однієї похибки:", r$one_se_rule, "\n")
  saveRDS(r, file.path(out_dir, "calibration_drop.rds"))
  openxlsx::write.xlsx(list(summary = as.data.frame(r$summary), details = r$details), file.path(out_dir, "calibration_drop.xlsx"), overwrite = TRUE)
} else if (what == "cleaning") {
  r <- mo_calibrate_cleaning(cfg)
  print(as.data.frame(r$summary))
  saveRDS(r, file.path(out_dir, "calibration_cleaning.rds"))
  openxlsx::write.xlsx(list(summary = as.data.frame(r$summary), details = r$details), file.path(out_dir, "calibration_cleaning.xlsx"), overwrite = TRUE)
} else if (what == "truth") {
  r <- mo_sensitivity_truth(cfg)
  saveRDS(r, file.path(out_dir, "calibration_truth.rds"))
  cat("Готово\n")
}
