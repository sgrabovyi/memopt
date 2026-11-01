# Вкладка «Дашборд»: лише для Сумської області (у переліку територій є область з кодом UA59) і лише в нашій
# інсталяції: у пакет для ЦГЗ цей файл не входить. Передає пороги проєкту в ETL дашборда (input/Dim_Thresholds.xlsx),
# архівує попередній файл з датою і часом і запускає ETL.
DASH_REGION <- "UA59"

is_dashboard_project <- function(proj) {
  tf <- file.path(proj, "territories.csv")
  if (!file.exists(tf)) return(FALSE)
  t <- read.csv(tf, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE)
  DASH_REGION %in% t[[1]]
}

etl_ok <- function(d) nzchar(d) && file.exists(file.path(d, "GRVI_Dashboard_NEW.py")) && dir.exists(file.path(d, "input"))

# Пороги для ETL у форматі input/Dim_Thresholds.xlsx: території з export_dim = yes
dim_thresholds <- function(proj) {
  thr <- read.csv(file.path(proj, "output", "memopt_thresholds.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  terr <- read.csv(file.path(proj, "territories.csv"), stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"), check.names = FALSE)
  sel <- terr[[1]][!is.na(terr$export_dim) & tolower(terr$export_dim) == "yes"]
  d0 <- thr[match(sel, thr$id), ]; d0 <- d0[!is.na(d0$id), ]
  data.frame(PCODE = d0$id, Name_in_Dataset = d0$name, Base_Threshold = d0$base, Medium_Threshold = d0$medium,
             High_Threshold = d0$high, VeryHigh_Threshold = d0$veryhigh, check.names = FALSE)
}

# Сезон, на який розраховано пороги: наступний після останнього сезону в даних проєкту
threshold_season <- function(proj) {
  s <- all_seasons(file.path(proj, "datasets")); s <- s[!vapply(s, season_running, logical(1))]
  if (!length(s)) return(NA_character_)
  y <- as.integer(substr(max(s), 6, 9))
  sprintf("%d/%02d", y, (y + 1) %% 100)
}

etl_season <- function(d) tryCatch(jsonlite::fromJSON(file.path(d, "config.json"))$thresholds$current_season, error = function(e) NA_character_)

dashboard_ui <- function() {
  nav_panel("Дашборд", value = "dash",
    layout_columns(col_widths = c(5, 7),
      card(card_header("Пороги в дашборді Сумської області"),
           textInput("etl_dir", "Папка ETL дашборда", placeholder = "наприклад C:\\...\\Dashboard GRVI\\GRVI_Dashboard_ETL"),
           actionButton("etl_dir_save", "Запам'ятати папку", class = "btn-outline-primary btn-sm"),
           uiOutput("etl_state"),
           tags$hr(),
           tags$p("1. Перегляньте таблицю праворуч: які пороги зараз у дашборді і які пропонує новий розрахунок."),
           actionButton("etl_push", "Передати нові пороги в дашборд", class = "btn-primary"),
           tags$p(class = "muted", style = "margin-top:8px", "Попередній файл порогів ETL зберігається в input/Пороги_архів з датою і часом, тож до нього завжди можна повернутися."),
           tags$hr(),
           tags$p("2. Оновіть дані дашборда: застосунок запустить ETL. Потім у Power BI натисніть «Оновити» (Refresh)."),
           actionButton("etl_run", "Запустити ETL", class = "btn-outline-primary"),
           uiOutput("etl_run_state")),
      card(card_header("Пороги: зараз у дашборді і за новим розрахунком"), DTOutput("etl_compare"),
           tags$h6(style = "margin-top:12px", "Журнал ETL"), div(class = "log-box", style = "height:220px", textOutput("etl_log")))))
}

dashboard_server <- function(input, output, session, P, rv) {
  observeEvent(input$project, {
    show <- is_dashboard_project(proj_dir(P()))
    if (show) nav_show("tabs", "dash") else nav_hide("tabs", "dash")
    cfg <- read_cfg(P())
    updateTextInput(session, "etl_dir", value = if (is.null(cfg$dashboard$etl_dir)) "" else cfg$dashboard$etl_dir)
  })
  observeEvent(input$etl_dir_save, {
    d <- trimws(input$etl_dir)
    if (!etl_ok(d)) { showNotification("У цій папці немає GRVI_Dashboard_NEW.py і папки input: перевірте шлях.", type = "error"); return() }
    cfg <- read_cfg(P()); cfg$dashboard$etl_dir <- d; write_cfg(P(), cfg); rv$tick <- rv$tick + 1
    showNotification("Папку ETL збережено.", type = "message")
  })
  output$etl_state <- renderUI({
    rv$tick; d <- trimws(input$etl_dir)
    if (!etl_ok(d)) return(tags$p(class = "text-danger", "Папку ETL не знайдено."))
    es <- etl_season(d); ts <- threshold_season(proj_dir(P()))
    tagList(tags$p(sprintf("Поточний сезон у налаштуваннях ETL: %s. Нові пороги розраховано на сезон %s.", es, ts)),
            if (!identical(gsub("/20", "/", es), gsub("/20", "/", ts)))
              tags$p(class = "text-danger", "Сезони не збігаються. Якщо це пороги нового сезону, спершу змініть thresholds.current_season у config.json ETL, інакше ETL запише їх як пороги попереднього сезону."))
  })
  output$etl_compare <- renderDT({
    rv$tick; d <- trimws(input$etl_dir); proj <- proj_dir(P())
    if (!file.exists(file.path(proj, "output", "memopt_thresholds.csv"))) return(datatable(data.frame(Повідомлення = "Спершу розрахуйте пороги."), rownames = FALSE, options = list(dom = "t")))
    new <- dim_thresholds(proj)
    cur <- if (etl_ok(d) && file.exists(file.path(d, "input", "Dim_Thresholds.xlsx"))) as.data.frame(readxl::read_excel(file.path(d, "input", "Dim_Thresholds.xlsx"))) else NULL
    v <- data.frame(Територія = new$Name_in_Dataset, `Епідемічний зараз` = if (is.null(cur)) NA else cur$Base_Threshold[match(new$PCODE, cur$PCODE)],
                    `Епідемічний новий` = new$Base_Threshold, `Середній зараз` = if (is.null(cur)) NA else cur$Medium_Threshold[match(new$PCODE, cur$PCODE)],
                    `Середній новий` = new$Medium_Threshold, check.names = FALSE)
    v$`Зміна епідемічного, %` <- round(100 * (v$`Епідемічний новий` / v$`Епідемічний зараз` - 1), 1)
    datatable(v, rownames = FALSE, options = list(pageLength = 15, language = list(search = "Пошук:"))) |>
      formatRound(2:5, digits = 1, dec.mark = ",", mark = " ") |>
      formatStyle("Зміна епідемічного, %", color = styleInterval(c(-0.05, 0.05), c("#0076BE", "#5b6573", "#B83227")))
  })
  observeEvent(input$etl_push, {
    d <- trimws(input$etl_dir); proj <- proj_dir(P())
    if (!etl_ok(d)) { showNotification("Папку ETL не знайдено.", type = "error"); return() }
    if (!file.exists(file.path(proj, "output", "memopt_thresholds.csv"))) { showNotification("Спершу розрахуйте пороги.", type = "error"); return() }
    new <- dim_thresholds(proj)
    if (!nrow(new)) { showNotification("Немає територій з позначкою export_dim = yes.", type = "error"); return() }
    cur <- file.path(d, "input", "Dim_Thresholds.xlsx")
    if (file.exists(cur)) {
      arch <- file.path(d, "input", "Пороги_архів"); dir.create(arch, showWarnings = FALSE)
      es <- gsub("/", "_", etl_season(d))
      file.copy(cur, file.path(arch, sprintf("Dim_Thresholds_%s.prev_%s.xlsx", es, format(Sys.time(), "%Y-%m-%d_%H%M"))))
    }
    openxlsx::write.xlsx(new, cur, sheetName = "Dim_Thresholds", overwrite = TRUE)
    rv$tick <- rv$tick + 1
    showNotification(sprintf("Пороги %d територій передано в ETL. Тепер запустіть ETL.", nrow(new)), type = "message", duration = 8)
  })
  observeEvent(input$etl_run, {
    d <- trimws(input$etl_dir)
    if (!etl_ok(d)) { showNotification("Папку ETL не знайдено.", type = "error"); return() }
    if (!is.null(rv$etl_job) && rv$etl_job$is_alive()) { showNotification("ETL уже працює.", type = "warning"); return() }
    py <- file.path(d, ".venv", "Scripts", "python.exe"); if (!file.exists(py)) py <- Sys.which("python")
    rv$etl_log <- file.path(proj_dir(P()), "output", "журнал_ETL.txt")
    env <- c("current", PYTHONPATH = file.path(d, "modules"), PYTHONIOENCODING = "utf-8")
    rv$etl_job <- processx::process$new(py, "GRVI_Dashboard_NEW.py", wd = d, env = env, stdout = rv$etl_log, stderr = "2>&1", cleanup = TRUE)
  })
  output$etl_log <- renderText({
    invalidateLater(1500)
    if (is.null(rv$etl_log) || !file.exists(rv$etl_log)) return("Журнал з'явиться після запуску ETL.")
    paste(tail(readLines(rv$etl_log, warn = FALSE, encoding = "UTF-8"), 200), collapse = "\n")
  })
  output$etl_run_state <- renderUI({
    invalidateLater(1500); j <- rv$etl_job
    if (is.null(j)) return(NULL)
    if (j$is_alive()) return(tags$p(tags$b("ETL працює.")))
    if (identical(j$get_exit_status(), 0L)) tags$p(class = "text-success", tags$b("ETL завершився успішно. Відкрийте Power BI і натисніть «Оновити»."))
    else tags$p(class = "text-danger", tags$b("ETL зупинився з помилкою: див. журнал."))
  })
}
