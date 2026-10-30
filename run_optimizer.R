# Запуск: Rscript run_optimizer.R [config.json]
args <- commandArgs(trailingOnly = TRUE)
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
cfg <- if (length(args)) args[1] else file.path(here, "config.json")
MO_CORE <- normalizePath(file.path(here, "R", "memopt_core.R"), winslash = "/")
source(MO_CORE, encoding = "UTF-8")
res <- mo_run(cfg)
