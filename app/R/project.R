# Спільне для застосунку і модулів у app/R: шляхи, налаштування проєкту, створення проєкту.
# Застосунок запускається з папки app (shiny::runApp('app')), тому корінь пакета на рівень вище.
suppressPackageStartupMessages(library(jsonlite))
APP_DIR <- normalizePath(getwd(), winslash = "/")
ROOT <- normalizePath(file.path(APP_DIR, ".."), winslash = "/")
CORE <- file.path(ROOT, "R", "memopt_core.R")
PROJ_ROOT <- file.path(ROOT, "projects")
TEMPLATE <- file.path(ROOT, "templates", "config_template.json")
dir.create(PROJ_ROOT, showWarnings = FALSE)
# сезони, які ядро відсікає завжди: беруться з самого ядра, щоб перелік у застосунку не розходився з розрахунком
AUTO_EXCLUDE <- local({
  src <- readLines(CORE, encoding = "UTF-8")
  i <- grep("^MO_AUTO_EXCLUDE <- ", src); j <- i
  while (!grepl("\\)\\s*$", src[j])) j <- j + 1
  eval(parse(text = src[i:j], encoding = "UTF-8"))
})
RELIABILITY_DEFAULT <- 20
SENS_LEVELS <- c(10, 15, 20, 25, 30)

plur <- function(n, one, few, many) {
  n <- abs(as.integer(n))
  if (n %% 10 == 1 && n %% 100 != 11) one else if (n %% 10 %in% 2:4 && !(n %% 100 %in% 12:14)) few else many
}

# ---------------------------------------------------------------- проєкти
list_projects <- function() sort(basename(list.dirs(PROJ_ROOT, recursive = FALSE)))
proj_dir <- function(p) file.path(PROJ_ROOT, p)

safe_name <- function(x) {
  x <- trimws(gsub("[\\\\/:*?\"<>|]", " ", x))
  gsub("\\s+", " ", x)
}

read_cfg <- function(p) fromJSON(file.path(proj_dir(p), "config.json"), simplifyVector = FALSE)
write_cfg <- function(p, cfg) writeLines(toJSON(cfg, auto_unbox = TRUE, pretty = TRUE, null = "null"), file.path(proj_dir(p), "config.json"), useBytes = TRUE)

create_project <- function(name) {
  d <- proj_dir(name)
  dir.create(file.path(d, "datasets"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(d, "output"), showWarnings = FALSE)
  cfg <- fromJSON(TEMPLATE, simplifyVector = FALSE)
  cfg$project <- name
  cfg$paths <- list(series_dir = "datasets", territories = "territories.csv", output_dir = "output")
  cfg$export <- NULL
  cfg$parallel <- list(workers = max(1L, min(6L, parallel::detectCores() - 1L)))
  writeLines(toJSON(cfg, auto_unbox = TRUE, pretty = TRUE, null = "null"), file.path(d, "config.json"), useBytes = TRUE)
  invisible(d)
}
