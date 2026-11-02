# Сповіщення і щотижневий розклад у застосунку: лише в нашій інсталяції, де нові дані з'являються самі.
# У пакет для ЦГЗ цей файл, notify.R і run_scheduled.R не входять: там новий сезон з'являється, лише коли
# епідеміолог сам завантажує файли, і тоді простіше натиснути «Розрахувати пороги».

notify_ui <- function() {
  tagList(
    tags$hr(),
    tags$h6("Сповіщення про перерахунок порогів"),
    note("Застосунок може сам перераховувати пороги раз на тиждень за розкладом (налаштовується нижче) і повідомляти про це. Повідомлення надходить у двох випадках: пороги перераховано, бо в даних з'явився новий завершений сезон, і тоді в листі нові пороги; або розрахунок за розкладом не вдався, і тоді в листі причина. Якщо розклад не вмикати, сповіщення не потрібні: виберіть «Не надсилати»."),
    radioButtons("n_method", NULL, c("Не надсилати" = "none", "Пошта" = "email", "Telegram" = "telegram", "Пошта і Telegram" = "both"), inline = TRUE),
    conditionalPanel("input.n_method == 'email' || input.n_method == 'both'",
      textAreaInput("emails", "Адреси отримувачів (кожна з нового рядка)", rows = 3, placeholder = "epid@cgz.example.ua"),
      layout_columns(col_widths = c(8, 4), textInput("smtp_server", "Поштовий сервер (SMTP)", placeholder = "smtp.gmail.com"),
                     numericInput("smtp_port", "Порт", value = 465, min = 1, max = 65535)),
      layout_columns(col_widths = c(6, 6), textInput("smtp_user", "Логін", placeholder = "memopt.oblast@gmail.com"),
                     passwordInput("smtp_password", "Пароль", placeholder = "залиште порожнім, щоб не змінювати")),
      textInput("smtp_from", "Адреса відправника (якщо інша, ніж логін)"),
      note("Для Gmail: сервер smtp.gmail.com, порт 465, пароль застосунку (Google Account, Безпека, Паролі застосунків), а не звичайний пароль.")),
    conditionalPanel("input.n_method == 'telegram' || input.n_method == 'both'",
      passwordInput("tg_token", "Токен бота Telegram", placeholder = "залиште порожнім, щоб не змінювати"),
      textInput("tg_chats", "Чати (номери через кому)", placeholder = "-1001234567890"),
      note("Бота створює @BotFather; бота треба додати в групу або канал. Номер чату можна дізнатися, написавши боту і відкривши https://api.telegram.org/bot<токен>/getUpdates.")),
    note("Паролі й токени зберігаються окремо від налаштувань, у файлі secrets.json у папці проєкту."),
    actionButton("n_test", "Надіслати пробне повідомлення", class = "btn-outline-primary btn-sm"),
    uiOutput("n_test_result"),
    tags$hr(),
    tags$h6("Розклад"),
    note("Щотижнева перевірка: якщо в даних з'явився новий завершений сезон (після 20-го тижня), пороги перераховуються, і надходить сповіщення. Розрахунок на вимогу, як і раніше, кнопкою на вкладці «Розрахунок»."),
    layout_columns(col_widths = c(6, 6),
      selectInput("sch_day", "День", c("понеділок" = "MON", "вівторок" = "TUE", "середа" = "WED", "четвер" = "THU", "п'ятниця" = "FRI")),
      textInput("sch_time", "Час", value = "09:00")),
    uiOutput("sch_status"),
    layout_columns(col_widths = c(6, 6),
      actionButton("sch_on", "Увімкнути розклад", class = "btn-outline-primary btn-sm"),
      actionButton("sch_off", "Вимкнути розклад", class = "btn-outline-danger btn-sm")))
}

# Заповнити поля збереженими налаштуваннями проєкту
notify_load <- function(session, cfg) {
  n <- cfg$notifications
  updateTextAreaInput(session, "emails", value = paste(unlist(n$emails), collapse = "\n"))
  updateRadioButtons(session, "n_method", selected = if (is.null(n$method)) "none" else n$method)
  updateTextInput(session, "smtp_server", value = if (is.null(n$smtp$server)) "" else n$smtp$server)
  updateNumericInput(session, "smtp_port", value = if (is.null(n$smtp$port)) 465 else n$smtp$port)
  updateTextInput(session, "smtp_user", value = if (is.null(n$smtp$user)) "" else n$smtp$user)
  updateTextInput(session, "smtp_from", value = if (is.null(n$smtp$from)) "" else n$smtp$from)
  updateTextInput(session, "tg_chats", value = paste(unlist(n$telegram$chats), collapse = ", "))
}

# Записати сповіщення в налаштування, паролі й токени в secrets.json. Повертає cfg або NULL, якщо адреса неправильна.
notify_save <- function(input, session, cfg, proj) {
  em <- trimws(unlist(strsplit(input$emails, "[\n,;]+"))); em <- em[nzchar(em)]
  bad <- em[!grepl("^[^@[:space:]]+@[^@[:space:]]+[.][^@[:space:]]+$", em)]
  if (length(bad)) { showNotification(paste("Неправильна адреса:", paste(bad, collapse = ", ")), type = "error"); return(NULL) }
  chats <- trimws(unlist(strsplit(input$tg_chats, "[,; ]+"))); chats <- chats[nzchar(chats)]
  cfg$notifications <- list(method = input$n_method, emails = as.list(em),
                            smtp = list(server = trimws(input$smtp_server), port = as.integer(input$smtp_port), user = trimws(input$smtp_user), from = trimws(input$smtp_from)),
                            telegram = list(chats = as.list(chats)))
  sec <- read_secrets(proj)
  if (nzchar(input$smtp_password)) sec$smtp_password <- input$smtp_password
  if (nzchar(input$tg_token)) sec$telegram_token <- trimws(input$tg_token)
  write_secrets(proj, sec)
  updateTextInput(session, "smtp_password", value = ""); updateTextInput(session, "tg_token", value = "")
  cfg
}

notify_server <- function(input, output, session, P, rv, save_all, root) {
  observeEvent(input$n_test, {
    if (!save_all()) return()
    rv$n_test <- notify(proj_dir(P()), "memopt: пробне повідомлення", paste0("Це пробне повідомлення застосунку memopt, проєкт «", P(), "». Якщо ви його отримали, сповіщення налаштовано правильно."))
  })
  output$n_test_result <- renderUI({
    r <- rv$n_test; if (is.null(r)) return(NULL)
    tags$div(style = "margin-top:8px", lapply(r, function(x) tags$p(class = if (grepl("помилка", x)) "text-danger" else "text-success", x)))
  })
  # розклад: завдання планувальника Windows; під час увімкнення запам'ятовується поточний останній сезон, щоб
  # перша перевірка не перераховувала пороги без нового сезону
  output$sch_status <- renderUI({
    rv$sch
    st <- task_status(P())
    if (is.null(st)) return(note("Розклад вимкнено."))
    nxt <- sub("^[^:]+:\\s*", "", grep("Next Run Time|Наступний запуск|Время следующего запуска", st, value = TRUE)[1])
    tags$p(class = "text-success", "Розклад увімкнено.", if (!is.na(nxt)) paste(" Наступна перевірка:", nxt))
  })
  observeEvent(input$sch_on, {
    if (!save_all()) return()
    if (!grepl("^[0-2][0-9]:[0-5][0-9]$", input$sch_time)) { showNotification("Час у вигляді 09:00.", type = "error"); return() }
    s <- all_seasons(file.path(proj_dir(P()), "datasets")); s <- s[!vapply(s, season_running, logical(1))]
    if (length(s)) writeLines(jsonlite::toJSON(list(last_season = max(s)), auto_unbox = TRUE), file.path(proj_dir(P()), "output", "розклад_стан.json"))
    r <- tryCatch({ task_create(P(), proj_dir(P()), root, input$sch_day, input$sch_time); "ok" }, error = function(e) conditionMessage(e))
    if (identical(r, "ok")) showNotification("Розклад увімкнено.", type = "message") else showNotification(paste("Не вдалося створити завдання:", r), type = "error", duration = 15)
    rv$sch <- Sys.time()
  })
  observeEvent(input$sch_off, { task_delete(P()); rv$sch <- Sys.time(); showNotification("Розклад вимкнено.", type = "message") })
}
