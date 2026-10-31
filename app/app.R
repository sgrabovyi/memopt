# memopt: застосунок shiny для розрахунку порогів MEM. Етап 1: проєкти областей, завантаження даних, розрахунок у фоні
# з журналом, перегляд і завантаження результатів. Запуск: Rscript -e "shiny::runApp('app', launch.browser = TRUE)"
# або файлом "Запустити_memopt.bat". Проєкти лежать у папці projects поруч із папкою app: кожна область окремо.
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(jsonlite)
})

APP_DIR <- normalizePath(getwd(), winslash = "/")
ROOT <- normalizePath(file.path(APP_DIR, ".."), winslash = "/")
CORE <- file.path(ROOT, "R", "memopt_core.R")
PROJ_ROOT <- file.path(ROOT, "projects")
TEMPLATE <- file.path(ROOT, "templates", "config_template.json")
dir.create(PROJ_ROOT, showWarnings = FALSE)

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

dataset_table <- function(p) {
  f <- list.files(file.path(proj_dir(p), "datasets"), pattern = "\\.xlsx$", full.names = TRUE)
  f <- f[!startsWith(basename(f), "~$")]
  if (!length(f)) return(data.frame(Файл = character(0), Аркушів = integer(0), Аркуші = character(0), Змінено = character(0)))
  do.call(rbind, lapply(f, function(x) {
    sh <- tryCatch(readxl::excel_sheets(x), error = function(e) character(0))
    data.frame(Файл = basename(x), Аркушів = length(sh),
               Аркуші = paste(c(head(sh, 6), if (length(sh) > 6) sprintf("і ще %d", length(sh) - 6)), collapse = ", "),
               Змінено = format(file.mtime(x), "%d.%m.%Y %H:%M"), check.names = FALSE)
  }))
}

project_status <- function(p) {
  d <- proj_dir(p)
  ds <- dataset_table(p)
  thr <- file.path(d, "output", "memopt_thresholds.csv")
  data.frame(
    Що = c("Ряди захворюваності (папка datasets)", "Перелік територій (territories.csv)", "Налаштування (config.json)", "Результати розрахунку"),
    Стан = c(if (nrow(ds)) paste("є:", nrow(ds), plur(nrow(ds), "файл", "файли", "файлів")) else "немає: завантажте на вкладці «Дані»",
             if (file.exists(file.path(d, "territories.csv"))) "є" else "немає: завантажте на вкладці «Дані» (на етапі 2 застосунок складатиме його сам)",
             if (file.exists(file.path(d, "config.json"))) "є" else "немає",
             if (file.exists(thr)) paste("є, розрахунок від", format(file.mtime(thr), "%d.%m.%Y %H:%M")) else "ще не рахувалось"),
    check.names = FALSE)
}

ready_to_run <- function(p) {
  d <- proj_dir(p)
  nrow(dataset_table(p)) > 0 && file.exists(file.path(d, "territories.csv")) && file.exists(file.path(d, "config.json"))
}

# ---------------------------------------------------------------- інтерфейс
theme <- bs_theme(version = 5, primary = "#17345E", secondary = "#0076be", base_font = font_collection("Segoe UI", "Arial", "sans-serif"))

ui <- page_navbar(
  title = "memopt: пороги MEM", theme = theme, id = "tabs",
  header = tags$head(tags$style(HTML("
    .log-box { background:#0f1b2d; color:#dfe7f2; font-family:Consolas,monospace; font-size:12.5px; height:420px; overflow-y:auto; white-space:pre-wrap; padding:12px; border-radius:8px; }
    .muted { color:#5b6573; }"))),
  nav_panel("Проєкт",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Проєкт області"),
           selectInput("project", "Відкрити проєкт", choices = list_projects()),
           tags$hr(),
           textInput("new_name", "Або створіть новий", placeholder = "Наприклад: Сумська область"),
           actionButton("create", "Створити проєкт", class = "btn-primary"),
           tags$p(class = "muted", style = "margin-top:12px",
                  "Кожна область має свою папку в projects: дані, налаштування і результати зберігаються там і не змішуються з іншими областями.")),
      card(card_header("Що готово в проєкті"), tableOutput("status"), uiOutput("status_hint")))),
  nav_panel("Дані",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Завантаження"),
           fileInput("upload_xlsx", "Ряди захворюваності (Excel, можна кілька файлів)", multiple = TRUE, accept = ".xlsx", buttonLabel = "Обрати", placeholder = "файли не обрано"),
           fileInput("upload_terr", "Перелік територій (territories.csv)", accept = ".csv", buttonLabel = "Обрати", placeholder = "файл не обрано"),
           tags$p(class = "muted", "Формат рядів як у memapp: аркуш на територію, у першій колонці тижні від 40 до 52 і від 1 до 20, далі колонка на кожен сезон (2023/2024 тощо), значення на 100 тис. населення.")),
      card(card_header("Файли проєкту"), DTOutput("datasets")))),
  nav_panel("Розрахунок",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Запуск"),
           uiOutput("run_ready"),
           actionButton("run", "Розрахувати пороги", class = "btn-primary btn-lg"),
           tags$p(class = "muted", style = "margin-top:12px", "Розрахунок триває від 3 до 10 хвилин. Вікно можна не закривати й переглядати інші вкладки."),
           uiOutput("run_state")),
      card(card_header("Журнал розрахунку"), div(class = "log-box", textOutput("log"))))),
  nav_panel("Результати",
    card(card_header("Пороги на 100 тис. населення за тиждень"),
         layout_columns(col_widths = c(3, 3, 6),
           downloadButton("dl_xlsx", "Книга результатів (Excel)"),
           downloadButton("dl_csv", "Таблиця порогів (CSV)"),
           radioButtons("res_kind", NULL, c("Усі території" = "all", "Основні території" = "основна", "Громади" = "мала"), inline = TRUE)),
         DTOutput("thresholds"))),
  nav_panel("Налаштування",
    card(card_header("Налаштування розрахунку"),
         tags$p("На цьому етапі налаштування беруться з типового шаблону і однакові для всіх областей. На етапі 3 тут можна буде задати перший сезон нинішнього періоду, виключити сезон з причиною для всіх територій або однієї, вказати адреси для сповіщень."),
         tableOutput("settings")))
)

# ---------------------------------------------------------------- логіка
server <- function(input, output, session) {
  rv <- reactiveValues(job = NULL, log = NULL, tick = 0, msg = NULL)

  observeEvent(input$create, {
    nm <- safe_name(input$new_name)
    if (!nzchar(nm)) { showNotification("Вкажіть назву проєкту.", type = "warning"); return() }
    if (nm %in% list_projects()) { showNotification("Такий проєкт уже є, його відкрито.", type = "message") } else create_project(nm)
    updateSelectInput(session, "project", choices = list_projects(), selected = nm)
    updateTextInput(session, "new_name", value = "")
  })

  P <- reactive({ req(input$project); input$project })
  refresh <- reactive({ rv$tick; input$project; invalidateLater(5000); TRUE })

  output$status <- renderTable({ refresh(); project_status(P()) }, striped = TRUE, bordered = TRUE)
  output$status_hint <- renderUI({
    refresh()
    if (ready_to_run(P())) tags$p("Усе на місці: можна запускати розрахунок на вкладці «Розрахунок».")
    else tags$p(class = "muted", "Завантажте відсутні файли на вкладці «Дані».")
  })

  observeEvent(input$upload_xlsx, {
    f <- input$upload_xlsx; dest <- file.path(proj_dir(P()), "datasets")
    ok <- file.copy(f$datapath, file.path(dest, f$name), overwrite = TRUE)
    showNotification(paste("Збережено", sum(ok), plur(sum(ok), "файл", "файли", "файлів")), type = "message"); rv$tick <- rv$tick + 1
  })
  observeEvent(input$upload_terr, {
    file.copy(input$upload_terr$datapath, file.path(proj_dir(P()), "territories.csv"), overwrite = TRUE)
    showNotification("Перелік територій збережено.", type = "message"); rv$tick <- rv$tick + 1
  })
  output$datasets <- renderDT({ refresh(); datatable(dataset_table(P()), rownames = FALSE, options = list(dom = "t", pageLength = 50, language = list(emptyTable = "Файлів ще немає"))) })

  output$run_ready <- renderUI({
    refresh()
    if (ready_to_run(P())) tags$p(paste0("Проєкт «", P(), "» готовий до розрахунку.")) else tags$p(class = "text-danger", "Бракує даних: див. вкладку «Проєкт».")
  })

  observeEvent(input$run, {
    if (!is.null(rv$job) && rv$job$is_alive()) { showNotification("Розрахунок уже йде.", type = "warning"); return() }
    if (!ready_to_run(P())) { showNotification("Бракує даних для розрахунку.", type = "error"); return() }
    logf <- file.path(proj_dir(P()), "output", "журнал_розрахунку.txt")
    rv$log <- logf
    rv$job <- callr::r_bg(function(core, cfg) { assign("MO_CORE", core, envir = .GlobalEnv); source(core, encoding = "UTF-8"); invisible(mo_run(cfg)) },
                          args = list(core = CORE, cfg = file.path(proj_dir(P()), "config.json")),
                          stdout = logf, stderr = "2>&1", supervise = TRUE)
    rv$msg <- NULL
  })

  observe({
    invalidateLater(1000)
    j <- rv$job
    if (!is.null(j) && !j$is_alive() && is.null(rv$msg)) {
      rv$msg <- if (j$get_exit_status() == 0) "done" else "error"
      rv$tick <- rv$tick + 1
    }
  })

  output$log <- renderText({
    invalidateLater(1000)
    if (is.null(rv$log) || !file.exists(rv$log)) return("Журнал з'явиться після запуску.")
    paste(readLines(rv$log, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  })
  output$run_state <- renderUI({
    invalidateLater(1000)
    j <- rv$job
    if (is.null(j)) return(NULL)
    if (j$is_alive()) return(tags$p(tags$b("Розрахунок іде.")))
    if (identical(rv$msg, "done")) tags$p(class = "text-success", tags$b("Готово. Результати на вкладці «Результати»."))
    else tags$p(class = "text-danger", tags$b("Розрахунок зупинився з помилкою: див. останні рядки журналу."))
  })

  thr <- reactive({
    refresh()
    f <- file.path(proj_dir(P()), "output", "memopt_thresholds.csv")
    if (!file.exists(f)) return(NULL)
    read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8")
  })
  output$thresholds <- renderDT({
    t <- thr()
    if (is.null(t)) return(datatable(data.frame(Повідомлення = "Результатів ще немає: запустіть розрахунок."), rownames = FALSE, options = list(dom = "t")))
    if (input$res_kind != "all") t <- t[t$kind == input$res_kind, ]
    v <- data.frame(Територія = t$name, Вид = t$kind, `Сезони в розрахунку` = t$seasons_used, Кут = t$angle_txt,
                    Епідемічний = t$base, Середній = t$medium, Високий = t$high, `Дуже високий` = t$veryhigh, check.names = FALSE)
    datatable(v, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, language = list(search = "Пошук:", lengthMenu = "Показати _MENU_", info = "_START_ до _END_ з _TOTAL_",
                                                              paginate = list(previous = "Назад", `next` = "Далі")))) |>
      formatRound(c("Епідемічний", "Середній", "Високий", "Дуже високий"), digits = 1, dec.mark = ",", mark = " ")
  })
  output$dl_xlsx <- downloadHandler(filename = function() paste0("memopt_результати_", P(), ".xlsx"),
                                    content = function(file) file.copy(file.path(proj_dir(P()), "output", "memopt_results.xlsx"), file))
  output$dl_csv <- downloadHandler(filename = function() paste0("memopt_пороги_", P(), ".csv"),
                                   content = function(file) file.copy(file.path(proj_dir(P()), "output", "memopt_thresholds.csv"), file))

  output$settings <- renderTable({
    f <- file.path(proj_dir(P()), "config.json"); if (!file.exists(f)) return(NULL)
    c <- fromJSON(f, simplifyVector = TRUE); ss <- c$season_selection
    data.frame(Параметр = c("Перший сезон нинішнього періоду", "Сезони, що не є контрольними", "Кути нахилу", "Кут для меж епідемії", "Найбільше сезонів у розрахунку", "Ядер для розрахунку"),
               Значення = c(ss$era_start, paste(ss$not_validation, collapse = ", "), paste0("від ", c$angle$min, " до ", c$angle$max, " з кроком ", c$angle$step),
                            c$angle$truth_angle, c$season$max_seasons, c$parallel$workers), check.names = FALSE)
  }, striped = TRUE, bordered = TRUE)

  session$onSessionEnded(function() { j <- isolate(rv$job); if (!is.null(j) && j$is_alive()) j$kill() })
}

shinyApp(ui, server)
