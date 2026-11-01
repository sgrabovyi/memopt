# Розрахунок за розкладом. Запускається планувальником Windows раз на тиждень (завдання створює застосунок на вкладці
# «Налаштування»): Rscript run_scheduled.R <папка проєкту>. Перераховує пороги лише тоді, коли в даних з'явився новий
# завершений сезон (після 20-го тижня), і надсилає підсумок; якщо розрахунок зупинився з помилкою, надсилає повідомлення
# про збій з останніми рядками журналу. Інакше нічого не робить. Журнал перевірок: output/журнал_розкладу.txt.
args <- commandArgs(trailingOnly = TRUE)
here <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), winslash = "/"))
proj <- normalizePath(args[1], winslash = "/")
MO_CORE <- file.path(here, "R", "memopt_core.R")
for (f in c("data_checks.R", "notify.R")) source(file.path(here, "app", "R", f), encoding = "UTF-8")
dir.create(file.path(proj, "output"), showWarnings = FALSE)
logf <- file.path(proj, "output", "журнал_розкладу.txt")
say <- function(...) cat(format(Sys.time(), "%d.%m.%Y %H:%M"), ..., "\n", file = logf, append = TRUE)

state_f <- file.path(proj, "output", "розклад_стан.json")
state <- if (file.exists(state_f)) jsonlite::fromJSON(state_f) else list()
seasons <- all_seasons(file.path(proj, "datasets"))
done <- seasons[!vapply(seasons, season_running, logical(1))]
last <- if (length(done)) max(done) else NA
name <- basename(proj)

if (is.na(last) || identical(state$last_season, last)) {
  say("перевірка: нового завершеного сезону немає (останній", last, "), перерахунок не потрібен")
  quit(save = "no", status = 0)
}

say("знайдено новий завершений сезон", last, ": перерахунок порогів")
runlog <- file.path(proj, "output", "журнал_розрахунку.txt")
ok <- tryCatch({
  sink(runlog); on.exit(sink(), add = TRUE)
  source(MO_CORE, encoding = "UTF-8")
  mo_run(file.path(proj, "config.json"))
  TRUE
}, error = function(e) { cat("Error:", conditionMessage(e), "\n", file = runlog, append = TRUE); FALSE })
try(sink(), silent = TRUE)

if (ok) {
  thr <- read.csv(file.path(proj, "output", "memopt_thresholds.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  ch <- tryCatch(openxlsx::read.xlsx(file.path(proj, "output", "memopt_results.xlsx"), sheet = "Зміни порогів"), error = function(e) NULL)
  flagged <- if (is.null(ch)) 0 else sum(nzchar(ch[[ncol(ch)]]) & !is.na(ch[[ncol(ch)]]))
  y <- as.integer(substr(last, 6, 9))
  text <- paste0("Пороги MEM перераховано: проєкт «", name, "», сезон ", y, "/", y + 1, " (останній сезон у даних ", last, ").\n",
                 "Територій: ", nrow(thr), ". Змін, які варто перевірити: ", flagged, " (аркуш «Зміни порогів» книги результатів).\n",
                 "Перегляньте результати в застосунку memopt і, якщо все гаразд, передайте пороги в роботу.")
  state$last_season <- last
  writeLines(jsonlite::toJSON(state, auto_unbox = TRUE), state_f)
  say("розрахунок завершено, сповіщення:", paste(notify(proj, paste0("memopt: пороги на сезон ", y, "/", y + 1, " перераховано"), text), collapse = "; "))
} else {
  tail_log <- paste(tail(readLines(runlog, warn = FALSE, encoding = "UTF-8"), 15), collapse = "\n")
  text <- paste0("Розрахунок порогів за розкладом зупинився з помилкою: проєкт «", name, "», сезон ", last, ".\n",
                 "Відкрийте застосунок memopt, вкладка «Розрахунок», і збережіть журнал для розробника.\n\nОстанні рядки журналу:\n", tail_log)
  say("розрахунок зупинився з помилкою, сповіщення:", paste(notify(proj, "memopt: збій розрахунку порогів за розкладом", text), collapse = "; "))
  quit(save = "no", status = 1)
}
