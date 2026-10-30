# Ретроспективна перевірка порогів на сезоні із заданим календарем епідемії.
# Запуск: Rscript run_backtest.R <сезон> <перший_тиждень_епідемії> <останній_тиждень_епідемії> [config.json]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) stop("Потрібно: сезон, перший і останній тиждень епідемії, наприклад: Rscript run_backtest.R 2025/2026 2 8")
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
cfg <- if (length(args) > 3) args[4] else file.path(here, "config.json")
MO_CORE <- normalizePath(file.path(here, "R", "memopt_core.R"), winslash = "/")
source(MO_CORE, encoding = "UTF-8")
source(file.path(here, "R", "memopt_backtest.R"), encoding = "UTF-8")
r <- mo_backtest(cfg, args[1], as.integer(args[2]), as.integer(args[3]))
cat("Готово: output/backtest\n")
