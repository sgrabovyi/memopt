# memopt: застосунок shiny для розрахунку порогів MEM.
# Етап 1: проєкти областей, завантаження даних, розрахунок у фоні з журналом, результати.
# Етап 2: перевірка даних з точним місцем і порадою, перелік територій з назв аркушів, населення громад,
# правило підтвердження районом (кандидати за порогом статистичної надійності, чутливість, ручне додавання з причиною).
# Етап 3: налаштування (перший сезон нинішнього періоду, виключення сезонів з причиною, ядра, адреси для сповіщень).
# Етап 4: вкладка «Дашборд» для Сумської області (app/R/dashboard.R): передача порогів в ETL з архівом і запуск ETL.
# Етап 5: сповіщення поштою і в Telegram, щотижневий розклад (app/R/notify.R, run_scheduled.R).
# Запуск: файл "Запустити_memopt.bat" або Rscript -e "shiny::runApp('app', launch.browser = TRUE)".
# Допоміжні функції в app/R (shiny підключає їх сам): project.R, data_checks.R, territories.R, dashboard.R.
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(jsonlite)
})

# Підписи таблиць українською: DT бере глобальні налаштування лише тоді, коли в таблиці не задано власних,
# тому кожна таблиця передає language = lang(...), де можна змінити окремі підписи.
DT_UK <- list(search = "Пошук:", lengthMenu = "Показати _MENU_ рядків", info = "Рядки _START_-_END_ з _TOTAL_",
              infoEmpty = "Рядків немає", infoFiltered = "(відібрано з _MAX_)", zeroRecords = "Нічого не знайдено",
              emptyTable = "Даних немає", loadingRecords = "Завантаження", processing = "Обробка",
              paginate = list(first = "Перша", previous = "Назад", `next` = "Далі", last = "Остання"))
lang <- function(...) modifyList(DT_UK, list(...))

dataset_table <- function(p) {
  f <- dataset_files(file.path(proj_dir(p), "datasets"))
  if (!length(f)) return(data.frame(Файл = character(0), Аркушів = integer(0), Аркуші = character(0), Змінено = character(0)))
  do.call(rbind, lapply(f, function(x) {
    sh <- tryCatch(readxl::excel_sheets(x), error = function(e) character(0))
    data.frame(Файл = basename(x), Аркушів = length(sh),
               Аркуші = paste(c(head(sh, 6), if (length(sh) > 6) sprintf("і ще %d", length(sh) - 6)), collapse = ", "),
               Змінено = format(file.mtime(x), "%d.%m.%Y %H:%M"), check.names = FALSE)
  }))
}

n_errors <- function(x) if (is.null(x)) 0L else sum(x$Рівень == "помилка")

project_status <- function(p, data_chk, terr_chk) {
  d <- proj_dir(p)
  ds <- dataset_table(p)
  thr <- file.path(d, "output", "memopt_thresholds.csv")
  de <- n_errors(data_chk); te <- n_errors(terr_chk)
  data.frame(
    Що = c("Ряди захворюваності", "Перевірка даних", "Перелік територій", "Результати розрахунку"),
    Стан = c(if (nrow(ds)) paste("є:", nrow(ds), plur(nrow(ds), "файл", "файли", "файлів")) else "немає: завантажте на вкладці «Дані»",
             if (is.null(data_chk)) "ще не перевірялись" else if (de) paste(de, plur(de, "помилка", "помилки", "помилок"), "у даних: див. вкладку «Дані»") else "помилок немає",
             if (!file.exists(file.path(d, "territories.csv"))) "не збережено: див. вкладку «Території»" else if (te) paste(te, plur(te, "помилка", "помилки", "помилок"), "у переліку: див. вкладку «Території»") else "збережено, помилок немає",
             if (file.exists(thr)) paste("є, розрахунок від", format(file.mtime(thr), "%d.%m.%Y %H:%M")) else "ще не рахувалось"),
    check.names = FALSE)
}

# Правило підтвердження районом: середній поріг громади в кількості випадків на тиждень
rule_table <- function(p) {
  d <- proj_dir(p)
  f <- file.path(d, "output", "memopt_thresholds.csv"); tf <- file.path(d, "territories.csv")
  if (!file.exists(f) || !file.exists(tf)) return(NULL)
  thr <- read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8")
  terr <- read.csv(tf, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"))
  if (names(terr)[1] != "id") names(terr)[1] <- "id"
  h <- terr[terr$kind == "small", , drop = FALSE]
  if (!nrow(h)) return(NULL)
  h$medium <- thr$medium[match(h$id, thr$id)]
  h$cases <- h$medium * as.numeric(h$population) / 1e5
  h[!is.na(h$cases), c("id", "name", "group", "population", "medium", "cases")]
}

# ---------------------------------------------------------------- інтерфейс
theme <- bs_theme(version = 5, primary = "#17345E", secondary = "#0076be", base_font = font_collection("Segoe UI", "Arial", "sans-serif"))
note <- function(...) tags$p(class = "muted", ...)

ui <- page_navbar(
  # fillable = FALSE: вкладки мають звичайну висоту і прокручуються, а не стискаються до висоти вікна
  # (інакше вміст карток накладається один на одний)
  title = "memopt: пороги MEM", theme = theme, id = "tabs", fillable = FALSE,
  header = tags$head(tags$style(HTML("
    .log-box { background:#0f1b2d; color:#dfe7f2; font-family:Consolas,monospace; font-size:12.5px; height:420px; overflow-y:auto; white-space:pre-wrap; padding:12px; border-radius:8px; }
    .muted { color:#5b6573; }
    .rule-num { font-size:28px; font-weight:700; color:#17345E; }
    .progress { height:24px; font-size:13px; margin:8px 0; }"))),
  nav_panel("Проєкт",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Проєкт області"),
           selectInput("project", "Відкрити проєкт", choices = list_projects()),
           tags$hr(),
           textInput("new_name", "Або створіть новий", placeholder = "Наприклад: Сумська область"),
           actionButton("create", "Створити проєкт", class = "btn-primary"),
           note(style = "margin-top:12px", "Кожна область має свою папку в projects: дані, налаштування і результати зберігаються там і не змішуються з іншими областями.")),
      card(card_header("Що готово в проєкті"), tableOutput("status"), uiOutput("status_hint")))),
  nav_panel("Дані",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Завантаження"),
           fileInput("upload_xlsx", "Ряди захворюваності (Excel, можна кілька файлів)", multiple = TRUE, accept = ".xlsx", buttonLabel = "Обрати", placeholder = "файли не обрано"),
           note("Кожен аркуш це одна територія, і назва аркуша стає назвою території, наприклад «Конотопський район», «Конотопський район без центру», «м. Конотоп», «Буринська міська ТГ». Excel дозволяє в назві аркуша щонайбільше 31 символ: якщо повна назва довша, скоротіть її на аркуші, а повну впишіть потім на вкладці «Території». Назви в межах області мають бути різними: громади з однаковою назвою розрізніть районом у дужках. Коди адмінодиниць не потрібні."),
           note("У першій колонці тижні від 40 до 52, потім від 1 до 20; далі колонка на кожен сезон з назвою 2023/2024; значення це кількість випадків на 100 тис. населення за тиждень, як у memapp."),
           actionButton("check_data", "Перевірити дані", class = "btn-outline-primary")),
      card(card_header("Файли проєкту"), DTOutput("datasets")))),
  nav_panel("Перевірка даних",
    card(card_header("Помилки у файлах"), uiOutput("data_check_summary"), DTOutput("data_check")),
    card(card_header("Що варто переглянути в даних (з останнього розрахунку)"),
         note("Під час розрахунку програма переглядає кожен сезон кожної території і позначає ті, що помітно відрізняються від звичних: багато нульових чи пропущених тижнів або пік, утричі вищий чи нижчий за звичайний для цієї території. Це не помилки, такі сезони беруться в розрахунок. Але варто перевірити, чи правильно внесені дані: якщо сезон справді зіпсований, його можна виключити на вкладці «Налаштування»."),
         uiOutput("flags_summary"), DTOutput("flags"))),
  nav_panel("Території",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Як заповнити"),
           note("Перелік складено з назв аркушів. Тип території і вид громади вгадано з назви; виправте їх, якщо треба: двічі клацніть клітинку, впишіть значення і натисніть Enter."),
           tags$p(tags$b("Типи: "), paste(TYPES, collapse = ", "), "."),
           tags$p(tags$b("Вид громади: "), paste(HKINDS, collapse = ", "), "."),
           note("Для кожної громади перевірте вид і вкажіть район (так само, як він записаний у колонці «Назва») і населення. Еталон громади визначається за видом: міська громада спирається на весь район, сільська і селищна на район без центру, а якщо його немає, на весь район."),
           tags$h6("Населення громад"),
           note("Населення можна вписати в таблицю вручну, а можна завантажити одним файлом. У файлі Excel або CSV перша колонка містить назви громад так само, як у переліку, друга населення; у третю можна вписати район, якщо в області є громади з однаковими назвами. Програма знайде громади за назвою і впише населення в перелік; назви, яких не знайде, покаже нижче."),
           fileInput("upload_pop", "Завантажити файл з населенням громад", accept = c(".xlsx", ".csv"), buttonLabel = "Обрати", placeholder = "файл не обрано"),
           uiOutput("pop_unmatched"),
           note("Якщо населення громад після 2022 року помітно змінилось через міграцію, а знаменник лишився старим, пороги таких громад можуть працювати гірше."),
           actionButton("save_terr", "Зберегти перелік", class = "btn-primary")),
      card(card_header("Перелік територій"), DTOutput("terr_table"), tags$hr(), tags$h6("Перевірка переліку"), DTOutput("terr_check")))),
  nav_panel("Розрахунок",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Запуск"),
           uiOutput("run_ready"),
           actionButton("run", "Розрахувати пороги", class = "btn-primary btn-lg"),
           note(style = "margin-top:12px", "Розрахунок триває від 3 до 10 хвилин. Вікно можна не закривати й переглядати інші вкладки."),
           uiOutput("run_state")),
      card(card_header("Журнал розрахунку"), div(class = "log-box", textOutput("log"))))),
  nav_panel("Результати",
    card(card_header("Пороги на 100 тис. населення за тиждень"),
         layout_columns(col_widths = c(3, 3, 6),
           downloadButton("dl_xlsx", "Книга результатів (Excel)"),
           downloadButton("dl_csv", "Таблиця порогів (CSV)"),
           radioButtons("res_kind", NULL, c("Усі території" = "all", "Основні території" = "основна", "Громади" = "мала"), inline = TRUE)),
         DTOutput("thresholds"))),
  nav_panel("Правило для малих громад",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Правило підтвердження районом"),
           tags$p("У малій громаді кілька випадків на тиждень можуть випадково підняти рівень. Тому мала громада показує рівень «середній» і вище лише тоді, коли її район уже не нижче свого епідемічного порогу; інакше показується «низький» з позначкою місцевого підвищення."),
           numericInput("rel_min", "Поріг статистичної надійності, випадків на тиждень", value = RELIABILITY_DEFAULT, min = 1, max = 100, step = 1),
           note("Громада вважається малою, якщо її середній поріг відповідає меншій кількості випадків на тиждень. Типове значення 20 випадків: показники, що спираються на менш ніж 20 подій, мають відносну похибку 23% і більше (Klein et al., 2002, Healthy People Statistical Notes 24)."),
           selectizeInput("rule_manual", "Додати громади вручну", choices = NULL, multiple = TRUE, options = list(placeholder = "оберіть громади")),
           textInput("rule_reason", "Причина ручного додавання", placeholder = "Наприклад: великий відтік населення"),
           actionButton("save_rule", "Зберегти правило", class = "btn-primary")),
      card(card_header("Громади, до яких застосовується правило"),
           uiOutput("rule_summary"), DTOutput("rule_list"), tags$hr(),
           tags$h6("Скільки громад потрапляє під правило за різного порогу надійності"), tableOutput("rule_sens")))),
  nav_panel("Налаштування",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Сезони"),
           selectInput("era_start", "Перший сезон нинішнього періоду", choices = NULL),
           note("З цього сезону починається період, у якому умови спостереження порівнянні з нинішніми (у Сумській області після початку повномасштабної війни це 2023/2024). Сезони цього періоду завжди входять у розрахунок, старші додаються, лише якщо покращують результат."),
           checkboxInput("era_first_not_ctl", "Перший сезон періоду не є контрольним (на ньому пороги не перевіряються)", value = TRUE),
           note("Залиште позначку, якщо перший сезон періоду нетиповий, наприклад почався на хвилі COVID-19."),
           tags$hr(),
           tags$h6("Сезони, які програма відсікає сама"),
           tableOutput("auto_excl"),
           tags$hr(),
           tags$h6("Виключити сезон з розрахунку"),
           note("Лише для сезонів з вадами даних (збій обліку, зміна визначення випадку) або незвичайним спалахом. Причина обов'язкова: вона потрапляє в книгу результатів."),
           layout_columns(col_widths = c(4, 8),
             selectInput("ex_season", "Сезон", choices = NULL),
             selectInput("ex_terr", "Для якої території", choices = NULL)),
           textInput("ex_reason", "Причина", placeholder = "Наприклад: звітність громад не надходила з 3-го по 9-й тиждень"),
           actionButton("ex_add", "Додати виключення", class = "btn-outline-primary"),
           tags$div(style = "margin-top:10px", DTOutput("ex_table")),
           actionButton("ex_del", "Видалити позначене виключення", class = "btn-outline-danger btn-sm")),
      card(card_header("Розрахунок і сповіщення"),
           numericInput("workers", "Скільки ядер процесора використовувати", value = 2, min = 1, max = parallel::detectCores(), step = 1),
           note(sprintf("На цьому комп'ютері %d ядер. Що більше ядер, то швидше розрахунок, але комп'ютер під час нього працює повільніше. На слабкому комп'ютері поставте 2.", parallel::detectCores())),
           tags$hr(),
           tags$h6("Сповіщення про перерахунок порогів"),
           note("Застосунок може сам перераховувати пороги раз на тиждень за розкладом (налаштовується нижче) і повідомляти про це. Повідомлення надходить у двох випадках: пороги перераховано, бо в даних з'явився новий завершений сезон, і тоді в листі нові пороги; або розрахунок за розкладом не вдався, і тоді в листі причина. Отримувачами зазвичай є епідеміолог, який відповідає за пороги, і керівник відділу. Якщо розклад не вмикати, сповіщення не потрібні: виберіть «Не надсилати»."),
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
             actionButton("sch_off", "Вимкнути розклад", class = "btn-outline-danger btn-sm")),
           tags$hr(),
           tags$h6("Інші параметри (лише для перегляду)"),
           tableOutput("settings"),
           tags$hr(),
           actionButton("save_settings", "Зберегти налаштування", class = "btn-primary"),
           uiOutput("settings_saved")))),
  # вкладка дашборда лише в нашій інсталяції (файл app/R/dashboard.R у пакет для ЦГЗ не входить)
  if (exists("dashboard_ui")) dashboard_ui()
)

# ---------------------------------------------------------------- логіка
server <- function(input, output, session) {
  rv <- reactiveValues(job = NULL, log = NULL, tick = 0, msg = NULL, data_chk = NULL, tb = NULL, unmatched = NULL)

  observeEvent(input$create, {
    nm <- safe_name(input$new_name)
    if (!nzchar(nm)) { showNotification("Вкажіть назву проєкту.", type = "warning"); return() }
    if (nm %in% list_projects()) showNotification("Такий проєкт уже є, його відкрито.", type = "message") else create_project(nm)
    updateSelectInput(session, "project", choices = list_projects(), selected = nm)
    updateTextInput(session, "new_name", value = "")
  })

  P <- reactive({ req(input$project); input$project })
  # таблиці перемальовуються лише тоді, коли змінились файли проєкту (дані, перелік, налаштування, результати),
  # а не за таймером: інакше прокрутка щоразу стрибає вгору
  files_state <- reactivePoll(3000, session, checkFunc = function() {
    p <- isolate(input$project); if (is.null(p) || !nzchar(p)) return("")
    d <- proj_dir(p)
    f <- c(list.files(file.path(d, "datasets"), full.names = TRUE), file.path(d, c("territories.csv", "config.json", "output/memopt_thresholds.csv")))
    paste(f, file.mtime(f), collapse = "|")
  }, valueFunc = function() Sys.time())
  refresh <- reactive({ rv$tick; input$project; files_state(); TRUE })

  # перевірка даних і перелік територій перебудовуються при зміні проєкту, завантаженні або на вимогу
  run_checks <- function() {
    d <- proj_dir(P())
    chk <- check_datasets(file.path(d, "datasets"))
    # порожня таблиця означає «перевірено, помилок немає»; NULL означає «ще не перевірялось»
    rv$data_chk <- if (is.null(chk)) msg("", "", "", "", "", "")[0, ] else chk
    rv$tb <- territory_table(d)
    rv$unmatched <- NULL
    rv$tick <- rv$tick + 1
  }
  observeEvent(input$project, run_checks())
  observeEvent(input$check_data, { run_checks(); showNotification("Дані перевірено.", type = "message") })
  terr_chk <- reactive({ if (is.null(rv$tb)) NULL else check_territories(rv$tb) })

  output$status <- renderTable({ refresh(); project_status(P(), rv$data_chk, terr_chk()) }, striped = TRUE, bordered = TRUE)
  output$status_hint <- renderUI({
    refresh()
    if (ready_to_run()) tags$p("Усе на місці: можна запускати розрахунок на вкладці «Розрахунок».")
    else note("Виправте те, що позначено в таблиці, і збережіть перелік територій.")
  })

  observeEvent(input$upload_xlsx, {
    f <- input$upload_xlsx; dest <- file.path(proj_dir(P()), "datasets")
    ok <- file.copy(f$datapath, file.path(dest, f$name), overwrite = TRUE)
    showNotification(paste("Збережено", sum(ok), plur(sum(ok), "файл", "файли", "файлів"), "; дані перевірено"), type = "message")
    run_checks()
  })
  output$datasets <- renderDT({ refresh(); datatable(dataset_table(P()), rownames = FALSE, options = list(dom = "t", pageLength = 50, language = lang(emptyTable = "Файлів ще немає"))) })

  output$data_check_summary <- renderUI({
    x <- rv$data_chk
    if (is.null(x)) return(note("Дані ще не перевірялись."))
    if (!nrow(x)) return(tags$p(class = "text-success", tags$b("Помилок у даних не знайдено.")))
    e <- sum(x$Рівень == "помилка"); w <- sum(x$Рівень == "увага")
    if (!e && !w) return(tags$p(class = "text-success", tags$b("Помилок у даних не знайдено."), " Нижче лише довідкові повідомлення."))
    tags$p(tags$b(sprintf("%d %s і %d %s.", e, plur(e, "помилка", "помилки", "помилок"), w, plur(w, "попередження", "попередження", "попереджень"))),
           " Помилки треба виправити у файлах Excel і завантажити їх знову; попередження варто переглянути; довідка лише пояснює, що програма робить з даними.")
  })
  output$data_check <- renderDT({
    x <- rv$data_chk; if (is.null(x) || !nrow(x)) x <- data.frame(Повідомлення = "Помилок немає")
    datatable(x, rownames = FALSE, options = list(pageLength = 25, language = lang(emptyTable = "Помилок немає"))) |>
      (\(d) if ("Рівень" %in% names(x)) formatStyle(d, "Рівень", color = styleEqual(c("помилка", "увага", "довідка"), c("#B83227", "#9A6700", "#5b6573")), fontWeight = "bold") else d)()
  })

  # зауваження з останнього розрахунку (аркуш «Перевірка даних» книги результатів)
  flags <- reactive({
    refresh()
    f <- file.path(proj_dir(P()), "output", "memopt_results.xlsx")
    if (!file.exists(f)) return(NULL)
    x <- tryCatch(as.data.frame(readxl::read_excel(f, sheet = "Перевірка даних")), error = function(e) NULL)
    if (is.null(x) || !"Примітка" %in% names(x)) return(NULL)
    x[!is.na(x$Примітка) & nzchar(x$Примітка) & x$Примітка != "сезон відсікається автоматично", , drop = FALSE]
  })
  output$flags_summary <- renderUI({
    x <- flags()
    if (is.null(x)) return(note("Зауваження з'являться після першого розрахунку на вкладці «Розрахунок»."))
    if (!nrow(x)) return(tags$p(class = "text-success", tags$b("Сезонів, які варто переглянути, немає.")))
    tags$p(tags$b(sprintf("%d %s, які варто переглянути.", nrow(x), plur(nrow(x), "сезон", "сезони", "сезонів"))))
  })
  output$flags <- renderDT({
    x <- flags(); req(x, nrow(x))
    v <- data.frame(Територія = x$Назва, Сезон = x$Сезон, `Що помічено` = x$Примітка,
                    `Нульових тижнів` = x$`Нульових тижнів`, `Пропущених тижнів` = x$`Пропущено тижнів усередині сезону`,
                    `Пік порівняно зі звичним, разів` = round(x$`Пік до звичного піку`, 2), check.names = FALSE)
    datatable(v, rownames = FALSE, options = list(pageLength = 25, language = lang())) |>
      formatRound("Пік порівняно зі звичним, разів", digits = 2, dec.mark = ",")
  })

  # ----- території
  show_cols <- c("Назва", "Тип", "Вид громади", "Район", "Населення", "Файл", "Аркуш", "Сезонів")
  output$terr_table <- renderDT({
    req(rv$tb)
    datatable(rv$tb[, show_cols], rownames = FALSE, selection = "none",
              editable = list(target = "cell", disable = list(columns = c(5, 6, 7))),
              options = list(pageLength = 25, language = lang(search = "Пошук:", emptyTable = "Аркушів немає: завантажте дані")))
  })
  observeEvent(input$terr_table_cell_edit, {
    e <- input$terr_table_cell_edit
    col <- show_cols[e$col + 1]; v <- trimws(as.character(e$value))
    if (col == "Населення") v <- suppressWarnings(as.numeric(gsub("[ ,]", "", v)))
    if (col == "Тип" && !(v %in% TYPES)) showNotification(paste("Тип має бути одним із:", paste(TYPES, collapse = ", ")), type = "warning")
    if (col == "Вид громади") { v <- tolower(v); if (!(v %in% HKINDS)) showNotification(paste("Вид громади має бути одним із:", paste(HKINDS, collapse = ", ")), type = "warning") }
    if (col == "Тип" && v == "громада" && is.na(rv$tb$`Вид громади`[e$row])) rv$tb$`Вид громади`[e$row] <- guess_kind_of_hromada(rv$tb$Назва[e$row])
    tb <- rv$tb; tb[e$row, col] <- v; rv$tb <- tb
  })
  observeEvent(input$upload_pop, {
    r <- tryCatch(apply_population(rv$tb, input$upload_pop$datapath), error = function(e) NULL)
    if (is.null(r)) { showNotification("Файл не вдалося прочитати: потрібні колонки назва і населення.", type = "error"); return() }
    rv$tb <- r$tb; rv$unmatched <- r$unmatched
    showNotification("Населення додано. Не забудьте зберегти перелік.", type = "message")
  })
  output$pop_unmatched <- renderUI({
    u <- rv$unmatched
    if (is.null(u) || !nrow(u)) return(NULL)
    tagList(tags$p(class = "text-danger", sprintf("%d %s з файлу не знайдено в переліку:", nrow(u), plur(nrow(u), "назва", "назви", "назв"))), renderTable(u))
  })
  output$terr_check <- renderDT({
    x <- terr_chk(); if (is.null(x)) x <- data.frame(Повідомлення = "Помилок немає")
    datatable(x, rownames = FALSE, options = list(dom = "tp", pageLength = 10, language = lang(emptyTable = "Помилок немає")))
  })
  observeEvent(input$save_terr, {
    x <- terr_chk()
    if (n_errors(x)) { showNotification("Спершу виправте помилки в переліку (таблиця під переліком).", type = "error"); return() }
    write_territories(rv$tb, proj_dir(P())); rv$tb <- territory_table(proj_dir(P()))
    showNotification("Перелік територій збережено.", type = "message"); rv$tick <- rv$tick + 1
  })

  # ----- розрахунок
  ready_to_run <- reactive({
    rv$tick
    d <- proj_dir(P())
    nrow(dataset_table(P())) > 0 && n_errors(rv$data_chk) == 0 && file.exists(file.path(d, "territories.csv")) && n_errors(terr_chk()) == 0
  })
  output$run_ready <- renderUI({
    refresh()
    if (ready_to_run()) tags$p(paste0("Проєкт «", P(), "» готовий до розрахунку.")) else tags$p(class = "text-danger", "Бракує даних або є помилки: див. вкладку «Проєкт».")
  })
  observeEvent(input$run, {
    if (!is.null(rv$job) && rv$job$is_alive()) { showNotification("Розрахунок уже йде.", type = "warning"); return() }
    if (!ready_to_run()) { showNotification("Спершу виправте помилки в даних і переліку територій.", type = "error"); return() }
    logf <- file.path(proj_dir(P()), "output", "журнал_розрахунку.txt")
    rv$log <- logf
    rv$job <- callr::r_bg(function(core, cfg) { assign("MO_CORE", core, envir = .GlobalEnv); source(core, encoding = "UTF-8"); invisible(mo_run(cfg)) },
                          args = list(core = CORE, cfg = file.path(proj_dir(P()), "config.json")),
                          stdout = logf, stderr = "2>&1", supervise = TRUE)
    rv$msg <- NULL; rv$started <- Sys.time()
  })
  dur_file <- function() file.path(proj_dir(P()), "output", "тривалість_розрахунку.txt")
  observe({
    invalidateLater(1000)
    j <- rv$job
    if (!is.null(j) && !j$is_alive() && is.null(rv$msg)) {
      rv$msg <- if (j$get_exit_status() == 0) "done" else "error"; rv$tick <- rv$tick + 1
      # тривалість успішного розрахунку запам'ятовується, щоб наступного разу показати, скільки приблизно лишилось
      if (identical(rv$msg, "done")) writeLines(as.character(round(as.numeric(difftime(Sys.time(), rv$started, units = "secs")))), dur_file())
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
    if (j$is_alive()) {
      # прогрес: етап з журналу (рядки «1.» ... «6.») і час від запуску порівняно з тривалістю попереднього розрахунку
      lg <- if (!is.null(rv$log) && file.exists(rv$log)) readLines(rv$log, warn = FALSE, encoding = "UTF-8") else character(0)
      heads <- grep("^[1-6][.] ", lg, value = TRUE)
      st <- if (length(heads)) as.integer(substr(tail(heads, 1), 1, 1)) else 1L
      floor_pct <- c(0, 5, 30, 45, 90, 95)[st]
      el <- as.numeric(difftime(Sys.time(), rv$started, units = "secs"))
      prev <- suppressWarnings(as.numeric(tryCatch(readLines(dur_file(), warn = FALSE)[1], error = function(e) NA)))
      pct <- if (!is.na(prev) && prev > 0) max(floor_pct, min(99, round(100 * el / prev))) else floor_pct
      mmss <- function(x) sprintf("%d хв %02d с", as.integer(x %/% 60), as.integer(round(x %% 60)))
      left <- if (!is.na(prev) && prev > el) paste0(", лишилось приблизно ", mmss(prev - el)) else ""
      return(tagList(
        tags$p(tags$b(sprintf("Розрахунок іде: етап %d з 6.", st)), " ", if (length(heads)) sub("^[1-6][.] ", "", tail(heads, 1))),
        tags$div(class = "progress", tags$div(class = "progress-bar progress-bar-striped progress-bar-animated", role = "progressbar",
                                              style = sprintf("width:%d%%", max(pct, 3)), sprintf("%d%%", pct))),
        note(paste0("Минуло ", mmss(el), left, ".", if (is.na(prev)) " Після першого розрахунку програма запам'ятає його тривалість і показуватиме, скільки лишилось." else ""))))
    }
    if (identical(rv$msg, "done")) tags$p(class = "text-success", tags$b("Готово. Результати на вкладках «Результати» і «Правило для малих громад»."))
    else {
      # непередбачена помилка: показати її людською мовою і дати зберегти журнал для розробника
      lg <- if (!is.null(rv$log) && file.exists(rv$log)) readLines(rv$log, warn = FALSE, encoding = "UTF-8") else character(0)
      er <- grep("^Error|^Помилка|Error in|Error:", lg, value = TRUE)
      tagList(tags$p(class = "text-danger", tags$b("Розрахунок зупинився через помилку, яку перевірка даних не впізнала.")),
              if (length(er)) tags$p("Повідомлення програми: ", tags$code(tail(er, 1))),
              tags$p("Якщо з повідомлення незрозуміло, що виправити в даних, збережіть журнал і надішліть його розробнику."),
              downloadButton("dl_log", "Зберегти журнал розрахунку"))
    }
  })
  output$dl_log <- downloadHandler(filename = function() paste0("memopt_журнал_", P(), ".txt"), content = function(file) file.copy(rv$log, file))

  # ----- результати
  thr <- reactive({
    refresh()
    f <- file.path(proj_dir(P()), "output", "memopt_thresholds.csv")
    if (!file.exists(f)) return(NULL)
    read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8")
  })
  output$thresholds <- renderDT({
    t <- thr()
    if (is.null(t)) return(datatable(data.frame(Повідомлення = "Результатів ще немає: запустіть розрахунок."), rownames = FALSE, options = list(dom = "t", language = lang())))
    if (input$res_kind != "all") t <- t[t$kind == input$res_kind, ]
    v <- data.frame(Територія = t$name, Вид = t$kind, `Сезони в розрахунку` = t$seasons_used, Кут = t$angle_txt,
                    Епідемічний = t$base, Середній = t$medium, Високий = t$high, `Дуже високий` = t$veryhigh, check.names = FALSE)
    datatable(v, rownames = FALSE, options = list(pageLength = 25, language = lang())) |>
      formatRound(c("Епідемічний", "Середній", "Високий", "Дуже високий"), digits = 1, dec.mark = ",", mark = " ")
  })
  output$dl_xlsx <- downloadHandler(filename = function() paste0("memopt_результати_", P(), ".xlsx"),
                                    content = function(file) file.copy(file.path(proj_dir(P()), "output", "memopt_results.xlsx"), file))
  output$dl_csv <- downloadHandler(filename = function() paste0("memopt_пороги_", P(), ".csv"),
                                   content = function(file) file.copy(file.path(proj_dir(P()), "output", "memopt_thresholds.csv"), file))

  # ----- правило підтвердження районом
  observeEvent(input$project, {
    cfg <- read_cfg(P())
    updateNumericInput(session, "rel_min", value = if (is.null(cfg$district_confirm$reliability_min_cases)) RELIABILITY_DEFAULT else cfg$district_confirm$reliability_min_cases)
    man <- cfg$district_confirm$manual
    rt <- rule_table(P())
    updateSelectizeInput(session, "rule_manual", choices = if (is.null(rt)) character(0) else rt$name, selected = names(man))
    updateTextInput(session, "rule_reason", value = if (length(man)) unique(unlist(man))[1] else "")
  })
  rule_now <- reactive({
    refresh()
    rt <- rule_table(P()); if (is.null(rt)) return(NULL)
    lim <- if (is.na(input$rel_min)) RELIABILITY_DEFAULT else input$rel_min
    rt$by_rule <- rt$cases < lim
    rt$manual <- rt$name %in% input$rule_manual
    rt
  })
  output$rule_summary <- renderUI({
    rt <- rule_now()
    if (is.null(rt)) return(note("Список з'явиться після розрахунку порогів для громад."))
    n <- sum(rt$by_rule | rt$manual)
    tags$p(tags$span(class = "rule-num", n), " ", plur(n, "громада", "громади", "громад"), " з ", nrow(rt), ": ",
           sum(rt$by_rule), " за порогом надійності, ", sum(rt$manual & !rt$by_rule), " додано вручну.")
  })
  output$rule_list <- renderDT({
    rt <- rule_now(); req(rt)
    v <- data.frame(Громада = rt$name, Населення = rt$population, `Середній поріг на 100 тис.` = round(rt$medium, 1),
                    `Середній поріг, випадків на тиждень` = round(rt$cases, 1),
                    Правило = ifelse(rt$by_rule, "так, за порогом надійності", ifelse(rt$manual, "так, додано вручну", "ні")), check.names = FALSE)
    datatable(v[order(v$Правило == "ні", v$`Середній поріг, випадків на тиждень`), ], rownames = FALSE,
              options = list(pageLength = 15, language = lang(search = "Пошук:"))) |>
      formatStyle("Правило", color = styleEqual(c("ні"), c("#5b6573"), default = "#17345E"), fontWeight = "bold")
  })
  output$rule_sens <- renderTable({
    rt <- rule_now(); req(rt)
    data.frame(`Поріг надійності, випадків на тиждень` = as.integer(SENS_LEVELS),
               `Громад за порогом` = vapply(SENS_LEVELS, function(l) sum(rt$cases < l), integer(1)),
               `Разом із доданими вручну` = vapply(SENS_LEVELS, function(l) sum(rt$cases < l | rt$manual), integer(1)), check.names = FALSE)
  }, striped = TRUE, bordered = TRUE)
  observeEvent(input$save_rule, {
    rt <- rule_now()
    if (is.null(rt)) { showNotification("Спершу розрахуйте пороги.", type = "warning"); return() }
    if (length(input$rule_manual) && !nzchar(trimws(input$rule_reason))) { showNotification("Вкажіть причину ручного додавання.", type = "warning"); return() }
    cfg <- read_cfg(P())
    cfg$district_confirm <- list(reliability_min_cases = input$rel_min,
                                 manual = if (length(input$rule_manual)) setNames(as.list(rep(trimws(input$rule_reason), length(input$rule_manual))), input$rule_manual) else setNames(list(), character(0)))
    write_cfg(P(), cfg)
    tf <- file.path(proj_dir(P()), "territories.csv")
    terr <- read.csv(tf, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"), check.names = FALSE)
    on <- rt$id[rt$by_rule | rt$manual]
    terr$district_confirm <- ifelse(terr$kind == "small", ifelse(terr[[1]] %in% on, "так", "ні"), NA)
    write.csv(terr, tf, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    showNotification(sprintf("Правило збережено: %d %s.", length(on), plur(length(on), "громада", "громади", "громад")), type = "message")
  })

  output$settings <- renderTable({
    rv$settings_saved
    f <- file.path(proj_dir(P()), "config.json"); if (!file.exists(f)) return(NULL)
    c <- fromJSON(f, simplifyVector = TRUE)
    data.frame(Параметр = c("Кути нахилу, що перебираються", "Кут для меж епідемії в контрольних сезонах", "Найбільше сезонів у розрахунку", "Межа падіння порогу на кроці кута",
                            "Ваги хибного сигналу і тижня запізнення"),
               Значення = c(paste0("від ", c$angle$min, " до ", c$angle$max, " з кроком ", c$angle$step), c$angle$truth_angle, c$season$max_seasons,
                            paste0(round(100 * c$angle$stability_drop), "%"), paste(c$angle$weights$false_alert, c$angle$weights$delay, sep = " : ")), check.names = FALSE)
  }, striped = TRUE, bordered = TRUE)

  # ----- налаштування
  terr_ids <- reactive({
    rv$tick
    tf <- file.path(proj_dir(P()), "territories.csv")
    if (!file.exists(tf)) return(setNames(character(0), character(0)))
    t <- read.csv(tf, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE)
    setNames(t[[1]], t$name)
  })
  data_seasons <- reactive({
    rv$tick
    s <- all_seasons(file.path(proj_dir(P()), "datasets"))
    s[!vapply(s, season_running, logical(1))]
  })
  observeEvent(list(input$project, rv$tick), {
    cfg <- read_cfg(P()); ss <- cfg$season_selection; seas <- data_seasons(); ids <- terr_ids()
    era <- if (is.null(ss$era_start)) "2023/2024" else ss$era_start
    updateSelectInput(session, "era_start", choices = unique(c(seas, era)), selected = era)
    updateCheckboxInput(session, "era_first_not_ctl", value = era %in% unlist(ss$not_validation))
    updateSelectInput(session, "ex_season", choices = setdiff(seas, names(AUTO_EXCLUDE)))
    updateSelectInput(session, "ex_terr", choices = c("усі території" = "", setNames(unname(ids), names(ids))))
    updateNumericInput(session, "workers", value = if (is.null(cfg$parallel$workers)) 2 else cfg$parallel$workers)
    n <- cfg$notifications
    updateTextAreaInput(session, "emails", value = paste(unlist(n$emails), collapse = "\n"))
    updateRadioButtons(session, "n_method", selected = if (is.null(n$method)) "none" else n$method)
    updateTextInput(session, "smtp_server", value = if (is.null(n$smtp$server)) "" else n$smtp$server)
    updateNumericInput(session, "smtp_port", value = if (is.null(n$smtp$port)) 465 else n$smtp$port)
    updateTextInput(session, "smtp_user", value = if (is.null(n$smtp$user)) "" else n$smtp$user)
    updateTextInput(session, "smtp_from", value = if (is.null(n$smtp$from)) "" else n$smtp$from)
    updateTextInput(session, "tg_chats", value = paste(unlist(n$telegram$chats), collapse = ", "))
    ex <- list()
    for (s in names(cfg$exclude_seasons)) ex[[length(ex) + 1]] <- data.frame(Сезон = s, Територія = "усі території", Код = "", Причина = cfg$exclude_seasons[[s]])
    for (id in names(cfg$exclude_territory_seasons)) for (s in names(cfg$exclude_territory_seasons[[id]]))
      ex[[length(ex) + 1]] <- data.frame(Сезон = s, Територія = if (id %in% ids) names(ids)[ids == id][1] else id, Код = id, Причина = cfg$exclude_territory_seasons[[id]][[s]])
    rv$ex <- if (length(ex)) do.call(rbind, ex) else data.frame(Сезон = character(0), Територія = character(0), Код = character(0), Причина = character(0))
  }, ignoreInit = FALSE)
  output$auto_excl <- renderTable(data.frame(Сезон = names(AUTO_EXCLUDE), Причина = unname(AUTO_EXCLUDE)), striped = TRUE, bordered = TRUE)
  output$ex_table <- renderDT({
    req(rv$ex)
    datatable(rv$ex[, c("Сезон", "Територія", "Причина")], rownames = FALSE, selection = "single",
              options = list(dom = "t", language = lang(emptyTable = "Виключень немає")))
  })
  observeEvent(input$ex_add, {
    if (!nzchar(trimws(input$ex_reason))) { showNotification("Вкажіть причину виключення.", type = "warning"); return() }
    if (!nzchar(input$ex_season)) return()
    ids <- terr_ids(); code <- input$ex_terr
    who <- if (nzchar(code)) names(ids)[ids == code][1] else "усі території"
    ex <- rv$ex[!(rv$ex$Сезон == input$ex_season & rv$ex$Код == code), ]
    rv$ex <- rbind(ex, data.frame(Сезон = input$ex_season, Територія = who, Код = code, Причина = trimws(input$ex_reason)))
    updateTextInput(session, "ex_reason", value = "")
  })
  observeEvent(input$ex_del, {
    i <- input$ex_table_rows_selected
    if (length(i)) rv$ex <- rv$ex[-i, ] else showNotification("Позначте рядок у таблиці виключень.", type = "warning")
  })
  # збереження всіх налаштувань; повертає FALSE, якщо щось введено неправильно
  save_all <- function() {
    em <- trimws(unlist(strsplit(input$emails, "[\n,;]+"))); em <- em[nzchar(em)]
    bad <- em[!grepl("^[^@[:space:]]+@[^@[:space:]]+[.][^@[:space:]]+$", em)]
    if (length(bad)) { showNotification(paste("Неправильна адреса:", paste(bad, collapse = ", ")), type = "error"); return(FALSE) }
    w <- input$workers
    if (is.na(w) || w < 1) { showNotification("Кількість ядер має бути не менше 1.", type = "error"); return(FALSE) }
    cfg <- read_cfg(P())
    cfg$season_selection$era_start <- input$era_start
    cfg$season_selection$not_validation <- if (isTRUE(input$era_first_not_ctl)) list(input$era_start) else list()
    all_ex <- rv$ex[rv$ex$Код == "", ]; one_ex <- rv$ex[rv$ex$Код != "", ]
    cfg$exclude_seasons <- if (nrow(all_ex)) setNames(as.list(all_ex$Причина), all_ex$Сезон) else setNames(list(), character(0))
    cfg$exclude_territory_seasons <- setNames(list(), character(0))
    for (id in unique(one_ex$Код)) { r <- one_ex[one_ex$Код == id, ]; cfg$exclude_territory_seasons[[id]] <- setNames(as.list(r$Причина), r$Сезон) }
    cfg$parallel$workers <- as.integer(min(w, parallel::detectCores()))
    chats <- trimws(unlist(strsplit(input$tg_chats, "[,; ]+"))); chats <- chats[nzchar(chats)]
    cfg$notifications <- list(method = input$n_method, emails = as.list(em),
                              smtp = list(server = trimws(input$smtp_server), port = as.integer(input$smtp_port), user = trimws(input$smtp_user), from = trimws(input$smtp_from)),
                              telegram = list(chats = as.list(chats)))
    write_cfg(P(), cfg)
    sec <- read_secrets(proj_dir(P()))
    if (nzchar(input$smtp_password)) sec$smtp_password <- input$smtp_password
    if (nzchar(input$tg_token)) sec$telegram_token <- trimws(input$tg_token)
    write_secrets(proj_dir(P()), sec)
    updateTextInput(session, "smtp_password", value = ""); updateTextInput(session, "tg_token", value = "")
    rv$settings_saved <- format(Sys.time(), "%H:%M")
    TRUE
  }
  observeEvent(input$save_settings, save_all())
  observeEvent(input$n_test, {
    if (!save_all()) return()
    r <- notify(proj_dir(P()), "memopt: пробне повідомлення", paste0("Це пробне повідомлення застосунку memopt, проєкт «", P(), "». Якщо ви його отримали, сповіщення налаштовано правильно."))
    rv$n_test <- r
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
    if (length(s)) writeLines(toJSON(list(last_season = max(s)), auto_unbox = TRUE), file.path(proj_dir(P()), "output", "розклад_стан.json"))
    r <- tryCatch({ task_create(P(), proj_dir(P()), ROOT, input$sch_day, input$sch_time); "ok" }, error = function(e) conditionMessage(e))
    if (identical(r, "ok")) showNotification("Розклад увімкнено.", type = "message") else showNotification(paste("Не вдалося створити завдання:", r), type = "error", duration = 15)
    rv$sch <- Sys.time()
  })
  observeEvent(input$sch_off, { task_delete(P()); rv$sch <- Sys.time(); showNotification("Розклад вимкнено.", type = "message") })
  output$settings_saved <- renderUI({
    if (is.null(rv$settings_saved)) return(NULL)
    tags$p(class = "text-success", style = "margin-top:10px", sprintf("Збережено о %s. Нові налаштування подіють під час наступного розрахунку порогів.", rv$settings_saved))
  })

  if (exists("dashboard_server")) dashboard_server(input, output, session, P, rv)

  session$onSessionEnded(function() { j <- isolate(rv$job); if (!is.null(j) && j$is_alive()) j$kill() })
}

shinyApp(ui, server)
