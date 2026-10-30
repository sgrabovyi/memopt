# Запуск: Rscript build_memapp_series.R (після run_optimizer.R; читає output/last_run.rds, пише output/ряди_для_memapp.xlsx)
# Будує книгу з рядами в тому вигляді, в якому memopt подає їх у memmodel (сезони, порожні святкові тижні,
# очищення початку), щоб у memapp можна було відтворити власні пороги кожної території.
suppressMessages({library(jsonlite); library(readxl); library(openxlsx); library(mem)})
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])); home <- normalizePath(here, winslash = "/")
source(file.path(home, "R", "memopt_core.R"), encoding = "UTF-8")
cfg <- mo_load_config(file.path(home, "config.json"))
terr <- mo_read_territories(cfg)
run <- readRDS(file.path(home, "output", "last_run.rds"))
thr <- run$thr
cat("territories:", nrow(terr), " thr rows:", nrow(thr), " cols:", names(thr), "\n")

log <- new.env(); log$rows <- list()
ser <- list()
for (i in seq_len(nrow(terr))) ser[[terr$id[i]]] <- mo_series(terr[i, ], cfg, log = log)
cleaned <- if (length(log$rows)) do.call(rbind, log$rows) else NULL
cat("cleaned rows:", if (is.null(cleaned)) 0 else nrow(cleaned), "\n")

# кут, який треба виставити в memapp
ang <- setNames(run$tabs$angle, run$tabs$id)
hr <- run$hr$main
ang_small <- setNames(hr$angle_own, hr$id)
angle_for <- function(id) if (id %in% names(ang)) ang[[id]] else if (id %in% names(ang_small)) ang_small[[id]] else NA

guide <- data.frame(
  Код = terr$id, Назва = terr$name, Вид = ifelse(terr$kind == "main", "основна", "мала"),
  `Кут нахилу в memapp` = vapply(terr$id, angle_for, numeric(1)),
  `Сезони на аркуші` = vapply(terr$id, function(id) paste(names(ser[[id]]), collapse = ", "), character(1)),
  `Порожні тижні` = vapply(terr$id, function(id) {
    h <- paste(cfg$season$holiday_weeks, collapse = ", ")
    cl <- if (is.null(cleaned)) NULL else cleaned[cleaned$id == id, ]
    if (!is.null(cl) && nrow(cl)) paste0(h, "; очищення початку: ", paste(paste0(cl$season, " тиждень ", cl$weeks), collapse = "; ")) else h
  }, character(1)),
  check.names = FALSE, stringsAsFactors = FALSE)
m <- match(terr$id, thr$id)
guide[["Власний епідемічний поріг (memapp має дати)"]] <- ifelse(terr$kind == "main", thr$base[m], hr$own_base[match(terr$id, hr$id)])
guide[["Власний середній поріг"]] <- ifelse(terr$kind == "main", thr$medium[m], hr$own_medium[match(terr$id, hr$id)])
guide[["Власний високий поріг"]] <- ifelse(terr$kind == "main", thr$high[m], hr$own_high[match(terr$id, hr$id)])
guide[["Власний дуже високий поріг"]] <- ifelse(terr$kind == "main", thr$veryhigh[m], hr$own_veryhigh[match(terr$id, hr$id)])
guide[["Підсумковий епідемічний поріг (у файлі)"]] <- thr$base[m]
guide[["Підсумковий середній поріг"]] <- thr$medium[m]
guide[["Підсумковий високий поріг"]] <- thr$high[m]
guide[["Підсумковий дуже високий поріг"]] <- thr$veryhigh[m]
guide[["Вага власного порогу (малі)"]] <- hr$w_own[match(terr$id, hr$id)]
guide[["Еталон (малі)"]] <- hr$reference[match(terr$id, hr$id)]
guide[["Коефіцієнт до еталона (малі)"]] <- hr$k[match(terr$id, hr$id)]
for (nm in names(guide)) if (is.numeric(guide[[nm]])) guide[[nm]] <- round(guide[[nm]], 3)

wb <- createWorkbook()
addWorksheet(wb, "Як користуватись")
intro <- c(
  "Ряди для відтворення порогів memopt у memapp (сезон 2026/27).",
  "Кожен аркуш названий кодом території і містить колонку Week і по колонці на сезон, уже підготовлені так, як їх бачить memmodel у memopt:",
  paste0("залишені тільки сезони з розрахунку, тижні ", paste(cfg$season$holiday_weeks, collapse = ", "), " порожні (свята), для малих територій прибрано викиди на початку сезону (колонка 'Порожні тижні')."),
  "Порядок дій у memapp: вкладка Файл, завантажити цю книгу, вибрати аркуш з кодом території, усі тижні з 40 по 20;",
  "вкладка Модель, у налаштуваннях поставити 'Параметр нахилу' з колонки 'Кут нахилу в memapp' (решта налаштувань за замовчанням: метод 2, усі сезони, хвости 1, поріг 5, інтенсивність 6 з рівнями 40, 90, 97.5, крива 2, рівень 95, інші 3, центрування -1).",
  "memapp покаже власні пороги території (колонки 'Власний ...'). Для основних територій це і є підсумкові пороги.",
  "Для малих територій (громад) підсумковий поріг у файлі є частковим об'єднанням: exp(w * ln(власний) + (1 - w) * ln(еталон * коефіцієнт)), де w з колонки 'Вага власного порогу', еталон з колонки 'Еталон', а пороги еталона з аркуша еталона на його куті.",
  "Якщо після об'єднання епідемічний поріг не нижчий за середній, епідемічний знижено до середній / 1.05.",
  "")
writeData(wb, "Як користуватись", data.frame(x = intro), colNames = FALSE)
writeData(wb, "Як користуватись", guide, startRow = length(intro) + 1)
setColWidths(wb, "Як користуватись", cols = 1:ncol(guide), widths = c(12, 36, 9, 10, 44, 40, rep(14, ncol(guide) - 6)))
for (id in terr$id) {
  d <- ser[[id]]
  out <- data.frame(Week = as.integer(rownames(d)), d, check.names = FALSE)
  addWorksheet(wb, id)
  writeData(wb, id, out)
}
f <- file.path(home, "output", "ряди_для_memapp.xlsx")
saveWorkbook(wb, f, overwrite = TRUE)
cat("saved:", f, " sheets:", length(names(wb)), "\n")
print(guide[guide$Код %in% c("UA5908", "UA5908027"), 1:10])
