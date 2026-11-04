# memopt: застосунок shiny для розрахунку порогів MEM.
# Інтерфейс кроками: смуга етапів угорі, на кожному кроці одне речення «що зробити» і кнопка переходу далі.
# Етап 1: проєкти областей, завантаження даних, розрахунок у фоні з журналом, результати.
# Етап 2: перевірка даних з точним місцем і порадою, перелік територій з назв аркушів, населення громад,
# громади з малою кількістю випадків (за межею надійності, чутливість, ручне додавання з причиною).
# Етап 3: налаштування (з якого сезону перевіряти пороги, виключення сезонів з причиною, ядра).
# Етап 4: вкладка «Дашборд» для Сумської області (app/R/dashboard.R): передача порогів в ETL з архівом і запуск ETL.
# Етап 5: сповіщення поштою і в Telegram, щотижневий розклад (app/R/notify.R, notify_ui.R, run_scheduled.R; лише в нашій інсталяції).
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

# Громади з малою кількістю випадків: середній поріг громади в кількості випадків на тиждень
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
# Робота йде кроками. Угорі смуга етапів: пройдені темні, поточний обведений, ще не пройдені бліді; клацання на етапі
# відкриває його. На кожному кроці вгорі одним реченням сказано, що зробити, а внизу кнопка переходу до наступного кроку.
# Довгі пояснення сховані під «Докладніше». Налаштування окремою кнопкою вгорі, бо їх змінюють рідко.
theme <- bs_theme(version = 5, primary = "#17345E", secondary = "#0076be", base_font = font_collection("Segoe UI", "Arial", "sans-serif"))
note <- function(...) tags$p(class = "muted", ...)
more <- function(title, ...) tags$details(class = "more", tags$summary(title), tags$div(class = "more-body", ...))
STEPS <- c(project = "Проєкт", data = "Дані", terr = "Території", run = "Розрахунок", small = "Громади з малою кількістю випадків", res = "Результати")
step_head <- function(key, what) tags$div(class = "step-head", tags$h4(sprintf("Крок %d. %s", match(key, names(STEPS)), STEPS[[key]])), tags$p(class = "lead-what", what))
box <- function(title, ..., cls = "") tags$div(class = paste("box", cls), if (!is.null(title)) tags$h6(class = "box-title", title), ...)
SHEET_NAMES <- tags$table(class = "table table-sm names",
  tags$thead(tags$tr(tags$th("На аркуші"), tags$th("Що це"), tags$th("Назва в застосунку"))),
  tags$tbody(lapply(list(c("Сумська обл.", "область", "Сумська область"), c("Конотопський р-н", "район", "Конотопський район"),
                         c("Конотопський без центру", "район без районного центру", "Конотопський район без центру"),
                         c("м. Конотоп", "районний центр", "м. Конотоп"), c("Буринська МТГ", "міська громада", "Буринська міська ТГ"),
                         c("Миколаївська СелТГ", "селищна громада", "Миколаївська селищна ТГ"), c("Бочечківська СілТГ", "сільська громада", "Бочечківська сільська ТГ")),
                    function(r) tags$tr(lapply(r, tags$td)))))

ui <- page(
  title = "memopt: пороги MEM", theme = theme,
  tags$head(tags$style(HTML("
    body { background:#f4f6f9; }
    .wrap { max-width:1280px; margin:0 auto; padding:0 20px 40px; }
    .topbar { display:flex; align-items:center; justify-content:space-between; padding:14px 0 6px; }
    .topbar .brand { font-size:20px; font-weight:700; color:#17345E; }
    .topbar .proj { color:#5b6573; margin-left:12px; font-size:15px; }
    .stepper { display:flex; margin:10px 0 22px; background:#fff; border-radius:10px; padding:16px 8px 12px; box-shadow:0 1px 3px rgba(0,0,0,.08); }
    .step { flex:1; text-align:center; position:relative; cursor:pointer; padding:0 4px; }
    .step .dot { width:34px; height:34px; border-radius:50%; line-height:30px; margin:0 auto 6px; font-weight:700; background:#e6eaf0; color:#9aa5b5; border:2px solid #e6eaf0; position:relative; z-index:1; }
    .step.done .dot { background:#17345E; color:#fff; border-color:#17345E; }
    .step.current .dot { background:#fff; color:#17345E; border-color:#17345E; box-shadow:0 0 0 4px #d6e0ee; }
    .step .lbl { font-size:13px; color:#9aa5b5; line-height:1.25; }
    .step.done .lbl, .step.current .lbl { color:#17345E; font-weight:600; }
    .step:not(:last-child)::after { content:''; position:absolute; top:16px; left:calc(50% + 20px); right:calc(-50% + 20px); height:3px; background:#e6eaf0; }
    .step.done:not(:last-child)::after { background:#17345E; }
    .step-head h4 { color:#17345E; font-weight:700; margin-bottom:4px; }
    .lead-what { font-size:16px; color:#2b3440; margin-bottom:16px; }
    .box { background:#fff; border-radius:10px; padding:16px 18px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,.08); }
    .box-title { font-weight:700; color:#17345E; margin-bottom:10px; }
    .box.bad { border-left:5px solid #B83227; }
    .box.good { border-left:5px solid #2e7d32; }
    .box.warn { border-left:5px solid #C98A00; }
    .box.action { border-top:4px solid #17345E; }
    .num { display:inline-block; width:26px; height:26px; border-radius:50%; background:#17345E; color:#fff; text-align:center; line-height:26px; font-weight:700; margin-right:8px; }
    .next-bar { display:flex; justify-content:flex-end; align-items:center; gap:16px; margin-top:8px; }
    .next-bar .why { color:#5b6573; }
    .btn-next { font-size:17px; padding:10px 22px; }
    .muted { color:#5b6573; }
    .more { margin:8px 0; } .more summary { cursor:pointer; color:#0076be; } .more-body { padding:8px 0 0 14px; color:#3b4552; }
    .ok-line { color:#2e7d32; font-weight:600; } .bad-line { color:#B83227; font-weight:600; }
    .log-box { background:#0f1b2d; color:#dfe7f2; font-family:Consolas,monospace; font-size:12.5px; height:420px; overflow-y:auto; white-space:pre-wrap; padding:12px; border-radius:8px; }
    .rule-num { font-size:28px; font-weight:700; color:#17345E; }
    .progress { height:24px; font-size:13px; margin:8px 0; }
    table.names td, table.names th { padding:3px 10px; }
    .shiny-input-container { width:100% !important; }
    .shiny-file-input-progress .progress-bar { font-size:0; }
    #busy { position:fixed; inset:0; background:rgba(244,246,249,.6); z-index:2000; display:none; align-items:center; justify-content:center; }
    #busy.on { display:flex; }
    #busy .msg { background:#fff; border-radius:10px; padding:18px 26px; box-shadow:0 2px 10px rgba(0,0,0,.15); font-size:16px; color:#17345E; display:flex; gap:12px; align-items:center; }
    #busy .spin { width:22px; height:22px; border:3px solid #d6e0ee; border-top-color:#17345E; border-radius:50%; animation:spin 0.8s linear infinite; }
    @keyframes spin { to { transform:rotate(360deg); } }"))),
  tags$div(id = "busy", tags$div(class = "msg", tags$div(class = "spin"), "Зачекайте, програма працює")),
  tags$script(HTML("
    var busyT = null;
    $(document).on('shiny:busy', function() { if (!busyT) busyT = setTimeout(function() { $('#busy').addClass('on'); }, 500); });
    $(document).on('shiny:idle', function() { clearTimeout(busyT); busyT = null; $('#busy').removeClass('on'); });
    // після натискання кнопки сторінка блідне одразу, щоб не натискали вдруге
    $(document).on('click', '.btn', function() { if (!$(this).is('.btn-next, .btn-file')) $('#busy').addClass('on'); setTimeout(function() { if (!busyT) $('#busy').removeClass('on'); }, 800); });")),
  tags$script(HTML("
    $(document).on('change', 'input[type=file]', function() {
      var n = this.files.length, t = $(this).closest('.input-group').find('input[type=text]');
      if (n > 1) setTimeout(function() { t.val('обрано файлів: ' + n); }, 50);
    });")),
  tags$div(class = "wrap",
    tags$div(class = "topbar",
      tags$div(tags$span(class = "brand", "memopt: пороги MEM"), uiOutput("proj_name", inline = TRUE)),
      tags$div(uiOutput("top_buttons", inline = TRUE),
               actionButton("open_settings", "Налаштування", class = "btn-outline-secondary btn-sm"))),
    uiOutput("stepper"),
    navset_hidden(id = "tabs",
      nav_panel_hidden("project",
        step_head("project", "Створіть проєкт своєї області або відкрийте вже створений."),
        layout_columns(col_widths = c(6, 6), fill = FALSE,
          box("Новий проєкт", cls = "action",
              textInput("new_name", "Назва області", placeholder = "Наприклад: Сумська область"),
              actionButton("create", "Створити проєкт", class = "btn-primary"),
              note(style = "margin-top:12px", "Дані, налаштування і результати кожної області зберігаються окремо, у папці projects.")),
          box("Уже створений проєкт",
              selectInput("project", NULL, choices = list_projects()),
              uiOutput("project_info"))),
        uiOutput("next_project")),
      nav_panel_hidden("data",
        step_head("data", "Завантажте файли Excel з рядами захворюваності. Можна вибрати кілька файлів одразу."),
        box(NULL, cls = "action",
            fileInput("upload_xlsx", NULL, multiple = TRUE, accept = ".xlsx", buttonLabel = "Обрати файли", placeholder = "файли не обрано", width = "100%"),
            more("Які файли потрібні і як назвати аркуші",
                 tags$p(tags$b("Обов'язково: "), "ряди області, усіх районів і всіх громад. ", tags$b("Дуже бажано: "), "ряди районів без районного центру, бо без них пороги селищних і сільських громад будуть менш точними. ",
                        tags$b("За бажанням: "), "ряди районних центрів, якщо для них потрібні власні пороги."),
                 tags$p("На аркуші: у першій колонці тижні від 40 до 52, потім від 1 до 20; далі колонка на кожен сезон з назвою 2023/2024; значення це кількість випадків на 100 тис. населення за тиждень, як у memapp. Зразок: templates\\series_template.xlsx."),
                 tags$p("Кожен аркуш це одна територія, а Excel дозволяє в назві аркуша не більше 31 символу. Пишіть назви коротко, програма сама запише повні:"),
                 SHEET_NAMES,
                 tags$p("Назви в межах області мають бути різними: громади з однаковою назвою розрізніть районом у дужках."))),
        uiOutput("data_summary"),
        box("Файли проєкту",
            DTOutput("datasets"),
            tags$div(style = "margin-top:8px", actionButton("del_file", "Видалити позначений файл", class = "btn-outline-danger btn-sm"),
                     note(style = "display:inline; margin-left:10px", "Щоб позначити файл, клацніть його рядок."))),
        uiOutput("data_errors"),
        uiOutput("next_data")),
      nav_panel_hidden("terr",
        step_head("terr", "Програма склала перелік територій з назв аркушів. Перевірте його, додайте населення громад і збережіть."),
        layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
          box(tagList(tags$span(class = "num", "1"), "Перевірте тип і вид"), cls = "action",
              tags$p("Тип території і вид громади програма визначила за назвою. Щоб виправити, двічі клацніть клітинку в таблиці нижче, впишіть значення і натисніть Enter."),
              note(tags$b("Тип: "), paste(setdiff(TYPES, "не використовувати"), collapse = ", "), ". ", tags$b("Вид громади: "), paste(HKINDS, collapse = ", "), "."),
              more("Навіщо вид громади",
                   tags$p("Ряд однієї громади короткий і нерівний, тому поріг лише за ним був би нестійким. Програма розраховує поріг громади з урахуванням даних більшої території, до якої громада належить (у книзі результатів її названо еталоном). Для міської громади це весь район, для селищної і сільської район без районного центру, бо в місті захворюваність зростає і спадає інакше, ніж у селах. Якщо району без районного центру немає, береться весь район."))),
          box(tagList(tags$span(class = "num", "2"), "Додайте населення громад"), cls = "action",
              tags$p("Файл Excel або CSV: у першій колонці назва громади, у другій кількість населення, у третій (необов'язково) назва району."),
              fileInput("upload_pop", NULL, accept = c(".xlsx", ".csv"), buttonLabel = "Обрати файл", placeholder = "не обрано", width = "100%"),
              uiOutput("pop_unmatched"),
              more("Навіщо населення",
                   tags$p("Населення потрібне лише для того, щоб перевести поріг громади з показника на 100 тис. у кількість випадків на тиждень і знайти громади, де випадків замало (крок 6). Тому досить одного числа, останнього відомого населення. Показники на 100 тис. у самих рядах рахуються ще до завантаження в застосунок, з населенням відповідного року."),
                   tags$p("Населення можна також вписати в колонку «Населення» таблиці вручну."),
                   tags$p("Окремого населення для кожного року не потрібно. Коли з'являться новіші дані про населення, перед черговим розрахунком завантажте новий файл: він замінить старі числа."))),
          box(tagList(tags$span(class = "num", "3"), "Збережіть перелік"), cls = "action",
              uiOutput("terr_state"),
              actionButton("save_terr", "Зберегти перелік територій", class = "btn-primary"))),
        uiOutput("terr_problems"),
        box("Перелік територій",
            checkboxInput("terr_only_bad", "Показати лише території, де треба щось виправити", value = FALSE),
            DTOutput("terr_table")),
        uiOutput("next_terr")),
      nav_panel_hidden("run",
        step_head("run", "Натисніть «Розрахувати пороги». Розрахунок триває від 3 до 10 хвилин; тим часом можна переглядати інші кроки."),
        layout_columns(col_widths = c(4, 8), fill = FALSE,
          box(NULL, cls = "action",
              uiOutput("run_ready"),
              actionButton("run", "Розрахувати пороги", class = "btn-primary btn-lg"),
              uiOutput("run_state")),
          box("Журнал розрахунку", div(class = "log-box", textOutput("log")))),
        uiOutput("flags_box"),
        uiOutput("next_run")),
      nav_panel_hidden("res",
        step_head("res", "Завантажте пороги для роботи. Повна книга результатів потрібна, щоб перевірити, як пороги розраховано."),
        uiOutput("res_state"),
        box("Пороги на 100 тис. населення за тиждень",
            tags$div(style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center; margin-bottom:10px",
                     downloadButton("dl_simple", "Пороги для роботи (Excel)", class = "btn-primary"),
                     downloadButton("dl_xlsx", "Повна книга результатів (Excel)", class = "btn-outline-primary"),
                     downloadButton("dl_csv", "Таблиця порогів (CSV)", class = "btn-outline-primary"),
                     radioButtons("res_kind", NULL, c("Усі території" = "all", "Основні території" = "основна", "Громади" = "мала"), inline = TRUE)),
            note("«Пороги для роботи» містить ту саму таблицю, що нижче: територія, чотири пороги і позначка громад з малою кількістю випадків. У повній книзі результатів для щоденної роботи досить трьох аркушів: «Пороги», «Зміни порогів» і «Перевірка даних». Що на решті аркушів, коротко сказано на першому аркуші книги, «Зміст», а докладніше в розділі 7 посібника (docs\\ПОСІБНИК_користувача.docx)."),
            DTOutput("thresholds"))),
      nav_panel_hidden("small",
        step_head("small", "Перевірте, які громади позначено через малу кількість випадків, і натисніть «Зберегти»."),
        layout_columns(col_widths = c(4, 8), fill = FALSE,
          box(NULL, cls = "action",
              tags$p("У громаді, де за тиждень буває лише кілька випадків, рівень може піднятися випадково. Тому в таких громадах середній і вищі рівні показуються лише тоді, коли захворюваність у районі без районного центру (а якщо його немає, у всьому районі) перевищила свій епідемічний поріг."),
              numericInput("rel_min", "Випадків на тиждень замало, якщо їх менше ніж", value = RELIABILITY_DEFAULT, min = 1, max = 100, step = 1),
              more("Чому 20",
                   tags$p("Програма переводить середній поріг громади у кількість випадків на тиждень і позначає громади, де ця кількість менша за вказану. Показники, що спираються на менш ніж 20 випадків, мають відносну похибку 23% і більше (Klein et al., 2002, Healthy People Statistical Notes 24).")),
              selectizeInput("rule_manual", "Позначити громади вручну", choices = NULL, multiple = TRUE, options = list(placeholder = "оберіть громади")),
              textInput("rule_reason", "Причина", placeholder = "Наприклад: великий відтік населення"),
              actionButton("save_rule", "Зберегти", class = "btn-primary")),
          box("Позначені громади",
              uiOutput("rule_summary"), DTOutput("rule_list"), tags$hr(),
              tags$h6("Скільки громад було б позначено за іншої межі"), tableOutput("rule_sens"))),
        uiOutput("next_small")),
      nav_panel_hidden("settings",
        tags$div(class = "step-head", tags$h4("Налаштування"), tags$p(class = "lead-what", "Змінюйте лише тоді, коли є причина. Після зміни збережіть налаштування і перерахуйте пороги.")),
        layout_columns(col_widths = c(6, 6), fill = FALSE,
          box("Сезони",
              selectInput("era_start", "Сезони для перевірки порогів: починаючи з", choices = NULL),
              note("Програма вибирає, скільки останніх сезонів брати для розрахунку порогу. Для цього вона перевіряє кожен варіант на сезонах, починаючи з указаного в цьому полі: прибирає один сезон, рахує поріг за рештою сезонів і дивиться, скільки хибних тривог дав би поріг у прибраному сезоні і на скільки тижнів запізнився б сигнал про початок епідемії."),
              note("Вкажіть у полі перший сезон після останньої події, яка помітно змінила рівень захворюваності або облік випадків: масового виїзду чи повернення населення, нової форми звітності, нового визначення випадку."),
              note("Переглядайте поле щороку перед розрахунком. Після нової такої події поле змінюють не одразу, бо для перевірки порогів потрібні щонайменше два завершені сезони. Наприклад, якщо населення почало масово повертатися в сезоні 2027/2028, у полі вказують 2027/2028 лише після завершення сезону 2028/2029. Раніше застосунок не дасть зберегти таку зміну."),
              checkboxInput("era_first_not_ctl", "Не перевіряти пороги на сезоні, указаному в полі", value = TRUE),
              note("Позначте, якщо сезон, указаний у полі, нетиповий, наприклад почався на хвилі COVID-19. У розрахунок порогу такий сезон усе одно увійде."),
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
          box(if (exists("notify_ui")) "Розрахунок і сповіщення" else "Розрахунок",
              numericInput("workers", "Скільки ядер процесора використовувати", value = 2, min = 1, max = parallel::detectCores(), step = 1),
              note(sprintf("На цьому комп'ютері %d ядер. Що більше ядер, то швидше розрахунок, але комп'ютер під час нього працює повільніше. На слабкому комп'ютері поставте 2.", parallel::detectCores())),
              if (exists("notify_ui")) notify_ui(),
              tags$hr(),
              more("Інші параметри розрахунку (лише для перегляду)", tableOutput("settings")))),
        tags$div(class = "next-bar",
                 uiOutput("settings_saved", inline = TRUE),
                 actionButton("back_steps", "Повернутися до кроків", class = "btn-outline-primary"),
                 actionButton("save_settings", "Зберегти налаштування", class = "btn-primary btn-next"))),
      # вкладка дашборда лише в нашій інсталяції (файл app/R/dashboard.R у пакет для ЦГЗ не входить)
      if (exists("dashboard_ui")) dashboard_ui()
    ))
)

# ---------------------------------------------------------------- логіка
server <- function(input, output, session) {
  rv <- reactiveValues(job = NULL, log = NULL, tick = 0, msg = NULL, data_chk = NULL, tb = NULL, unmatched = NULL, terr_dirty = FALSE, terr_idx = NULL)

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
    chk <- if (length(dataset_files(file.path(d, "datasets")))) check_datasets(file.path(d, "datasets")) else NULL
    # порожня таблиця означає «перевірено, помилок немає»; NULL означає «ще не перевірялось»
    rv$data_chk <- if (is.null(chk)) msg("", "", "", "", "", "")[0, ] else chk
    rv$tb <- territory_table(d)
    rv$unmatched <- NULL; rv$terr_dirty <- FALSE
    rv$tick <- rv$tick + 1
  }
  observeEvent(input$project, run_checks())
  terr_chk <- reactive({ if (is.null(rv$tb)) NULL else check_territories(rv$tb) })

  # ----- кроки: що вже зроблено, смуга етапів, перехід до наступного кроку
  small_saved <- function(d) {
    tf <- file.path(d, "territories.csv"); if (!file.exists(tf)) return(FALSE)
    t <- read.csv(tf, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"), check.names = FALSE)
    if (!any(t$kind == "small", na.rm = TRUE)) return(TRUE)
    "district_confirm" %in% names(t) && any(!is.na(t$district_confirm[t$kind == "small"]))
  }
  has_project <- reactive({ p <- input$project; !is.null(p) && nzchar(p) && dir.exists(proj_dir(p)) })
  step_done <- reactive({
    refresh(); rv$tick
    none <- setNames(rep(FALSE, length(STEPS)), names(STEPS))
    if (!has_project()) return(none)
    d <- proj_dir(P())
    data_ok <- nrow(dataset_table(P())) > 0 && !is.null(rv$data_chk) && n_errors(rv$data_chk) == 0
    terr_ok <- data_ok && file.exists(file.path(d, "territories.csv")) && n_errors(terr_chk()) == 0 && !isTRUE(rv$terr_dirty)
    run_ok <- terr_ok && file.exists(file.path(d, "output", "memopt_thresholds.csv"))
    small_ok <- run_ok && small_saved(d)
    c(project = TRUE, data = data_ok, terr = terr_ok, run = run_ok, small = small_ok, res = small_ok)
  })
  first_undone <- function() { st <- step_done(); k <- names(st)[!st]; if (length(k)) k[1] else "res" }
  output$stepper <- renderUI({
    st <- step_done(); cur <- input$tabs
    tags$div(class = "stepper", lapply(seq_along(STEPS), function(i) {
      k <- names(STEPS)[i]
      tags$div(class = paste("step", if (st[[k]]) "done", if (identical(cur, k)) "current"),
               onclick = sprintf("Shiny.setInputValue('goto', '%s', {priority: 'event'})", k), title = "Відкрити цей крок",
               tags$div(class = "dot", if (st[[k]] && !identical(cur, k)) "\u2713" else i), tags$div(class = "lbl", STEPS[[k]]))
    }))
  })
  # відкритий або щойно створений проєкт відкривається на першому незробленому кроці
  observeEvent(input$project, { if (has_project()) nav_select("tabs", first_undone()) }, priority = -10)
  observeEvent(input$goto, nav_select("tabs", input$goto))
  for (k in names(STEPS)) local({ kk <- k; observeEvent(input[[paste0("go_", kk)]], nav_select("tabs", kk)) })
  observeEvent(input$open_settings, nav_select("tabs", "settings"))
  observeEvent(input$back_steps, nav_select("tabs", first_undone()))
  next_bar <- function(key, why = NULL) {
    st <- step_done(); ok <- st[[key]]
    i <- match(key, names(STEPS)); nk <- names(STEPS)[i + 1]
    if (is.na(nk)) return(NULL)
    b <- actionButton(paste0("go_", nk), sprintf("Далі: крок %d. %s", i + 1, STEPS[[nk]]), class = paste("btn-next", if (ok) "btn-primary" else "btn-outline-secondary"))
    if (!ok) b <- tagAppendAttributes(b, disabled = NA)
    tags$div(class = "next-bar", if (!ok && !is.null(why)) tags$span(class = "why", why), b)
  }
  output$proj_name <- renderUI({ if (has_project()) tags$span(class = "proj", paste("Проєкт:", P())) })
  output$top_buttons <- renderUI({
    if (has_project() && exists("is_dashboard_project") && is_dashboard_project(proj_dir(P())))
      actionButton("open_dash", "Дашборд", class = "btn-outline-secondary btn-sm", style = "margin-right:6px")
  })
  observeEvent(input$open_dash, nav_select("tabs", "dash"))
  output$project_info <- renderUI({
    refresh()
    if (!has_project()) return(note("Проєктів ще немає: створіть новий ліворуч."))
    thr <- file.path(proj_dir(P()), "output", "memopt_thresholds.csv")
    n <- nrow(dataset_table(P()))
    tagList(tags$p(sprintf("Файлів з рядами: %d.", n)),
            tags$p(if (file.exists(thr)) paste("Останній розрахунок порогів:", format(file.mtime(thr), "%d.%m.%Y %H:%M")) else "Пороги ще не розраховувались."))
  })
  output$next_project <- renderUI(next_bar("project", "Створіть або відкрийте проєкт."))

  observeEvent(input$upload_xlsx, {
    f <- input$upload_xlsx; dest <- file.path(proj_dir(P()), "datasets")
    # файл з населенням громад до рядів не додається: він потрібен на кроці «Території»
    pop <- vapply(f$datapath, is_population_file, logical(1))
    if (any(pop)) showNotification(paste0("Файл «", paste(f$name[pop], collapse = "», «"), "» схожий на файл з населенням громад, тому його не додано до рядів. Завантажте його на кроці 3, «Території»."),
                                   type = "warning", duration = 20)
    ok <- file.copy(f$datapath[!pop], file.path(dest, f$name[!pop]), overwrite = TRUE)
    if (length(ok)) showNotification(paste0("Додано ", sum(ok), " ", plur(sum(ok), "файл", "файли", "файлів"), " з рядами; дані перевірено."), type = "message")
    run_checks()
  })
  output$datasets <- renderDT({
    refresh()
    datatable(dataset_table(P()), rownames = FALSE, selection = "single",
              options = list(dom = "t", pageLength = 50, language = lang(emptyTable = "Файлів ще немає")))
  })
  observeEvent(input$del_file, {
    i <- input$datasets_rows_selected
    if (!length(i)) { showNotification("Спершу клацніть рядок файлу, який треба видалити.", type = "warning"); return() }
    f <- dataset_table(P())$Файл[i]
    file.remove(file.path(proj_dir(P()), "datasets", f)); run_checks()
    showNotification(paste0("Файл «", f, "» видалено з проєкту."), type = "message")
  })
  output$data_summary <- renderUI({
    refresh(); x <- rv$data_chk; n <- nrow(dataset_table(P()))
    if (!n) return(NULL)
    if (is.null(x)) return(NULL)
    e <- sum(x$Рівень == "помилка"); w <- sum(x$Рівень == "увага")
    if (e) return(box(NULL, cls = "bad", tags$p(class = "bad-line", sprintf("У даних %d %s.", e, plur(e, "помилка", "помилки", "помилок"))),
                      tags$p("Список нижче показує файл, аркуш і клітинку кожної помилки та що з нею зробити. Виправте помилки в Excel і завантажте ці файли знову: файл з тією самою назвою замінить старий.")))
    gp <- x[x$Рівень == "увага" & grepl("порожн(ій|і|іх) тижн", x$`Що не так`), , drop = FALSE]
    box(NULL, cls = if (w) "warn" else "good",
        tags$p(class = "ok-line", sprintf("Файлів: %d. Помилок у даних немає.", n)),
        if (nrow(gp)) tags$p(tags$b(sprintf("На %d %s є порожні тижні всередині сезонів.", nrow(gp), plur(nrow(gp), "аркуші", "аркушах", "аркушах"))),
                             " Програма вважає порожню клітинку відсутніми даними, а не нулем, тому пороги цих територій можуть вийти іншими, ніж за повних даних. Якщо випадків у ці тижні не було, впишіть 0 і завантажте файл знову. Перелік аркушів і сезонів нижче."),
        if (w) tags$p(sprintf("Є %d %s: розрахунку вони не заважають, але їх варто переглянути (список нижче).", w, plur(w, "зауваження", "зауваження", "зауважень"))))
  })
  output$data_errors <- renderUI({
    x <- rv$data_chk
    if (is.null(x) || !nrow(x)) return(NULL)
    main <- x[x$Рівень != "довідка", , drop = FALSE]; info <- x[x$Рівень == "довідка", , drop = FALSE]
    box(if (nrow(main)) "Що виправити або переглянути" else NULL,
        if (nrow(main)) DTOutput("data_check"),
        if (nrow(info)) more(sprintf("Як програма обробила дані (%d %s)", nrow(info), plur(nrow(info), "повідомлення", "повідомлення", "повідомлень")), tableOutput("data_info")))
  })
  output$data_check <- renderDT({
    x <- rv$data_chk; req(x); x <- x[x$Рівень != "довідка", , drop = FALSE]
    datatable(x, rownames = FALSE, selection = "none", options = list(pageLength = 25, language = lang())) |>
      formatStyle("Рівень", color = styleEqual(c("помилка", "увага"), c("#B83227", "#9A6700")), fontWeight = "bold")
  })
  output$data_info <- renderTable({ x <- rv$data_chk; req(x); x[x$Рівень == "довідка", -1, drop = FALSE] }, striped = TRUE)
  output$next_data <- renderUI({
    refresh(); n <- nrow(dataset_table(P()))
    next_bar("data", if (!n) "Завантажте файли з рядами." else "Виправте помилки в даних і завантажте файли знову.")
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
  output$flags <- renderDT({
    x <- flags(); req(x, nrow(x))
    v <- data.frame(Територія = x$Назва, Сезон = x$Сезон, `Що помічено` = x$Примітка,
                    `Тижнів без випадків` = x$`Нульових тижнів`, `Пропущених тижнів` = x$`Пропущено тижнів усередині сезону`, check.names = FALSE)
    datatable(v, rownames = FALSE, selection = "none", options = list(pageLength = 10, language = lang()))
  })

  # ----- території
  # «Перевірка» в першій колонці позначає рядки з помилками; таблицю можна звузити до них, тому рядки таблиці
  # зіставляються з рядками переліку через rv$terr_idx
  show_cols <- c("Перевірка", "Назва", "Тип", "Вид громади", "Район", "Населення", "Файл", "Аркуш", "Сезонів")
  # поки файл з населенням не завантажено, район і населення громад порожні: ці помилки не показуються, замість них одне нагадування
  pop_loaded <- reactive({ tb <- rv$tb; !is.null(tb) && any(!is.na(tb$Населення[tb$Тип %in% "громада"])) })
  shown_chk <- reactive({
    x <- terr_chk(); if (is.null(x) || pop_loaded()) return(x)
    x <- x[!(x$`Що не так` %in% c("не вказано населення громади", "не вказано район громади")), , drop = FALSE]
    if (nrow(x)) x else NULL
  })
  bad_names <- reactive({ x <- shown_chk(); if (is.null(x)) character(0) else unique(x$Територія[x$Рівень == "помилка" & nzchar(x$Територія)]) })
  output$terr_table <- renderDT({
    req(rv$tb)
    tb <- rv$tb; tb$Перевірка <- ifelse(tb$Назва %in% bad_names(), "виправити", "")
    tb$Тип[is.na(tb$Тип)] <- ""
    # які саме клітинки треба заповнити (приховані колонки для підсвічування)
    h <- tb$Тип == "громада"; districts <- tb$Назва[tb$Тип == "район"]
    tb$.t <- as.integer(!(tb$Тип %in% TYPES))
    tb$.k <- as.integer(h & !(tb$`Вид громади` %in% HKINDS))
    tb$.r <- as.integer(h & pop_loaded() & (is.na(tb$Район) | !(tb$Район %in% districts)))
    tb$.p <- as.integer(h & pop_loaded() & (is.na(tb$Населення) | tb$Населення <= 0))
    idx <- if (isTRUE(input$terr_only_bad)) which(nzchar(tb$Перевірка)) else seq_len(nrow(tb))
    rv$terr_idx <- idx
    pink <- styleEqual(1, "#fbd5d1")
    datatable(tb[idx, c(show_cols, ".t", ".k", ".r", ".p")], rownames = FALSE, selection = "none",
              editable = list(target = "cell", disable = list(columns = c(0, 6:12))),
              options = list(pageLength = 100, columnDefs = list(list(visible = FALSE, targets = 9:12)),
                             language = lang(search = "Пошук:", emptyTable = "Аркушів немає: завантажте дані на кроці 2"))) |>
      formatStyle("Перевірка", color = "#B83227", fontWeight = "bold") |>
      formatStyle("Тип", ".t", backgroundColor = pink) |> formatStyle("Вид громади", ".k", backgroundColor = pink) |>
      formatStyle("Район", ".r", backgroundColor = pink) |> formatStyle("Населення", ".p", backgroundColor = pink)
  })
  observeEvent(input$terr_table_cell_edit, {
    e <- input$terr_table_cell_edit
    row <- rv$terr_idx[e$row]
    col <- show_cols[e$col + 1]; v <- trimws(as.character(e$value))
    if (col == "Населення") v <- suppressWarnings(as.numeric(gsub("[ ,]", "", v)))
    if (col == "Тип") { v <- tolower(v); if (!(v %in% TYPES)) showNotification(paste("Тип має бути одним із:", paste(TYPES, collapse = ", ")), type = "warning") }
    if (col == "Вид громади") { v <- tolower(v); if (!(v %in% HKINDS)) showNotification(paste("Вид громади має бути одним із:", paste(HKINDS, collapse = ", ")), type = "warning") }
    if (col == "Тип" && v == "громада" && is.na(rv$tb$`Вид громади`[row])) rv$tb$`Вид громади`[row] <- guess_kind_of_hromada(rv$tb$Назва[row])
    tb <- rv$tb; tb[row, col] <- v; rv$tb <- tb; rv$terr_dirty <- TRUE
  })
  observeEvent(input$upload_pop, {
    r <- tryCatch(apply_population(rv$tb, input$upload_pop$datapath), error = function(e) NULL)
    if (is.null(r)) { showNotification("Файл не вдалося прочитати: у першій колонці мають бути назви громад, у другій населення.", type = "error"); return() }
    rv$tb <- r$tb; rv$unmatched <- r$unmatched; rv$terr_dirty <- TRUE
    showNotification("Населення додано. Не забудьте зберегти перелік територій.", type = "message")
  })
  output$pop_unmatched <- renderUI({
    u <- rv$unmatched
    if (is.null(u) || !nrow(u)) return(NULL)
    tagList(tags$p(class = "bad-line", sprintf("%d %s з файлу не знайдено в переліку територій. Виправте назву у файлі так, як вона записана в колонці «Назва», і завантажте файл знову.",
                                               nrow(u), plur(nrow(u), "назву", "назви", "назв"))), renderTable(u))
  })
  output$terr_problems <- renderUI({
    x <- shown_chk(); if (is.null(rv$tb) || !nrow(rv$tb)) return(NULL)
    nopop <- if (!pop_loaded() && any(rv$tb$Тип %in% "громада")) box(NULL, cls = "warn",
      tags$p(tags$b("Ще не завантажено файл з населенням громад."), " Завантажте його в пункті 2 угорі. З нього програма візьме населення і район кожної громади, після цього тут буде видно, чи лишилось щось виправити."))
    if (is.null(x) || !nrow(x)) return(if (!is.null(nopop)) nopop else box(NULL, cls = "good", tags$p(class = "ok-line", "У переліку територій помилок немає.")))
    e <- sum(x$Рівень == "помилка")
    box(if (e) sprintf("Що треба виправити: %d %s", e, plur(e, "помилка", "помилки", "помилок")) else "Варто звернути увагу",
        cls = if (e) "bad" else "warn",
        if (e) tags$p("Рядки з помилками позначено в таблиці переліку словом «виправити», а клітинки, які треба заповнити, підсвічено. Щоб бачити лише ці рядки, увімкніть позначку над таблицею."),
        DTOutput("terr_check"), nopop)
  })
  # однакові помилки різних територій одним рядком: скільки їх і які території
  output$terr_check <- renderDT({
    x <- shown_chk(); req(x)
    key <- paste(x$Рівень, x$`Що не так`, x$`Як виправити`, sep = "\r")
    g <- do.call(rbind, lapply(unique(key), function(k) {
      r <- x[key == k, ]; nm <- r$Територія[nzchar(r$Територія)]
      data.frame(Рівень = r$Рівень[1], `Що не так` = r$`Що не так`[1], Скільки = nrow(r),
                 Території = paste0(paste(head(nm, 4), collapse = ", "), if (length(nm) > 4) sprintf(" і ще %d", length(nm) - 4) else ""),
                 `Як виправити` = r$`Як виправити`[1], check.names = FALSE)
    }))
    datatable(g, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = 50, language = lang())) |>
      formatStyle("Рівень", color = styleEqual(c("помилка", "увага"), c("#B83227", "#9A6700")), fontWeight = "bold")
  })
  output$terr_state <- renderUI({
    refresh()
    saved <- file.exists(file.path(proj_dir(P()), "territories.csv")); e <- n_errors(shown_chk())
    if (!pop_loaded() && any(rv$tb$Тип %in% "громада")) tags$p("Спершу завантажте файл з населенням громад (пункт 2).")
    else if (e) tags$p(class = "bad-line", sprintf("Спершу виправте %d %s (список нижче), потім збережіть.", e, plur(e, "помилку", "помилки", "помилок")))
    else if (isTRUE(rv$terr_dirty) || !saved) tags$p("Помилок немає. Натисніть кнопку, щоб зберегти перелік.")
    else tags$p(class = "ok-line", "Перелік територій збережено.")
  })
  observeEvent(input$save_terr, {
    x <- terr_chk()
    if (n_errors(x)) { showNotification("Спершу виправте помилки в переліку територій (список над таблицею).", type = "error"); return() }
    write_territories(rv$tb, proj_dir(P())); rv$tb <- territory_table(proj_dir(P())); rv$terr_dirty <- FALSE
    showNotification("Перелік територій збережено.", type = "message"); rv$tick <- rv$tick + 1
  })
  output$next_terr <- renderUI(next_bar("terr", "Виправте помилки в переліку і натисніть «Зберегти перелік територій»."))

  # ----- розрахунок
  ready_to_run <- reactive({
    rv$tick
    d <- proj_dir(P())
    nrow(dataset_table(P())) > 0 && n_errors(rv$data_chk) == 0 && file.exists(file.path(d, "territories.csv")) && n_errors(terr_chk()) == 0
  })
  output$run_ready <- renderUI({
    refresh()
    if (ready_to_run()) tags$p("Усе готово до розрахунку.") else tags$p(class = "bad-line", "Спершу пройдіть кроки 2 і 3: завантажте дані без помилок і збережіть перелік територій.")
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
    if (identical(rv$msg, "done")) tags$p(class = "text-success", tags$b("Готово. Пороги розраховано: перейдіть до кроку 5."))
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
  output$next_run <- renderUI(next_bar("run", "Дочекайтеся завершення розрахунку."))
  output$dl_log <- downloadHandler(filename = function() paste0("memopt_журнал_", P(), ".txt"), content = function(file) file.copy(rv$log, file))

  # ----- результати
  thr <- reactive({
    refresh()
    f <- file.path(proj_dir(P()), "output", "memopt_thresholds.csv")
    if (!file.exists(f)) return(NULL)
    read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8")
  })
  output$flags_box <- renderUI({
    if (is.null(thr()) || (!is.null(rv$job) && rv$job$is_alive())) return(NULL)
    x <- flags(); if (is.null(x) || !nrow(x)) return(NULL)
    box(sprintf("Сезони, які варто перевірити в даних: %d", nrow(x)), cls = "warn",
        tags$p("Ці сезони помітно відрізняються від звичних для території: багато нульових чи пропущених тижнів або пік, утричі вищий чи нижчий за звичайний. Це не помилки, такі сезони беруться в розрахунок. Перевірте в Excel, чи правильно внесено дані. Якщо сезон справді зіпсований, виключіть його в налаштуваннях (кнопка вгорі праворуч) і розрахуйте пороги заново. Якщо все гаразд, переходьте до наступного кроку."),
        DTOutput("flags"))
  })
  # позначені громади з кроку «Громади з малою кількістю випадків», як їх збережено
  marks <- reactive({
    refresh(); rv$tick
    tf <- file.path(proj_dir(P()), "territories.csv"); if (!file.exists(tf)) return(NULL)
    t <- read.csv(tf, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"), check.names = FALSE)
    if (!"district_confirm" %in% names(t)) return(NULL)
    man <- read_cfg(P())$district_confirm$manual
    t <- t[t$kind %in% "small" & !is.na(t$district_confirm), , drop = FALSE]
    if (!nrow(t)) return(NULL)
    lab <- ifelse(t$district_confirm != "так", "ні", ifelse(t$name %in% names(man),
                  paste0("так, додано вручну (", vapply(t$name, function(n) if (n %in% names(man)) as.character(man[[n]])[1] else "", ""), ")"), "так, мало випадків"))
    data.frame(id = t[[1]], label = lab, stringsAsFactors = FALSE)
  })
  MARK_COL <- "Громада з малою кількістю випадків"
  output$res_state <- renderUI({
    st <- step_done()
    if (is.null(thr())) return(box(NULL, cls = "warn", tags$p("Результатів ще немає: розрахуйте пороги на кроці 4.")))
    if (!st[["res"]]) return(box(NULL, cls = "warn", tags$p("Спершу збережіть громади з малою кількістю випадків на кроці 5, інакше в книзі результатів їх не буде позначено.")))
    box("Усе зроблено", cls = "good",
        tags$p(paste0("Пороги розраховано, громади з малою кількістю випадків позначено в колонці «", MARK_COL, "». Для роботи завантажте «Пороги для роботи (Excel)» кнопкою нижче.")),
        tags$p("Щороку після 20-го тижня, коли сезон завершився, допишіть його колонку в ряди, завантажте файли на кроці 2 і розрахуйте пороги заново."))
  })
  # Пороги в робочому порядку: область, далі кожен район за абеткою, за ним його громади за абеткою, район без районного
  # центру і районний центр. Район громади береться з території, з якою громада порівнюється (без слів «без центру»).
  # Район міста визначається за спільним початком назв (Конотоп і Конотопський, Ромни і Роменський); місто без такого району йде в кінець.
  work_table <- reactive({
    t <- thr(); if (is.null(t)) return(NULL)
    terr <- read.csv(file.path(proj_dir(P()), "territories.csv"), stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"), check.names = FALSE); names(terr)[1] <- "id"
    districts <- stringi::stri_sort(t$name[t$level == "район"], locale = "uk")
    stem <- function(x) tolower(sub("^м\\.\\s*", "", x))
    city_district <- function(city) {
      pre <- vapply(districts, function(d) { a <- strsplit(stem(city), "")[[1]]; b <- strsplit(tolower(d), "")[[1]]; n <- min(length(a), length(b))
        k <- which(a[seq_len(n)] != b[seq_len(n)]); if (length(k)) k[1] - 1L else n }, integer(1))
      if (length(pre) && max(pre) >= 3) districts[which.max(pre)] else NA_character_
    }
    ref <- terr$name[match(terr$reference_id[match(t$id, terr$id)], terr$id)]
    dist <- ifelse(t$level == "район", t$name, ifelse(t$level == "район без центру", sub(" без центру$", "", t$name),
              ifelse(t$level == "громада", sub(" без центру$", "", ref), ifelse(t$level == "місто", vapply(t$name, city_district, ""), NA))))
    pos <- match(dist, districts); pos[t$level == "область"] <- 0; pos[is.na(pos)] <- length(districts) + 1
    within <- match(t$level, c("область", "район", "громада", "район без центру", "місто"))
    nm <- rank(stringi::stri_rank(t$name, locale = "uk"))
    t <- t[order(pos, within, nm), ]
    v <- data.frame(Територія = t$name, `Епідемічний поріг` = round(t$base, 1), `Середній поріг` = round(t$medium, 1),
                    `Високий поріг` = round(t$high, 1), `Дуже високий поріг` = round(t$veryhigh, 1), check.names = FALSE)
    mk <- marks(); if (!is.null(mk)) v[[MARK_COL]] <- ifelse(is.na(match(t$id, mk$id)), "", sub(",.*", "", mk$label[match(t$id, mk$id)]))
    attr(v, "kind") <- t$kind
    v
  })
  output$thresholds <- renderDT({
    v <- work_table()
    if (is.null(v)) return(datatable(data.frame(Повідомлення = "Результатів ще немає: запустіть розрахунок."), rownames = FALSE, options = list(dom = "t", language = lang())))
    if (input$res_kind != "all") v <- v[attr(v, "kind") == input$res_kind, , drop = FALSE]
    datatable(v, rownames = FALSE, options = list(pageLength = 25, ordering = FALSE, language = lang())) |>
      formatRound(2:5, digits = 1, dec.mark = ",", mark = " ")
  })
  # пороги для щоденної роботи: один аркуш з територіями і порогами, без розрахункових подробиць
  for_season <- function(t) {
    s <- unlist(strsplit(paste(t$seasons_used, collapse = ","), "[,;] *")); y <- suppressWarnings(max(as.integer(substr(trimws(s), 6, 9)), na.rm = TRUE))
    if (is.finite(y)) paste0(y, "-", y + 1) else ""
  }
  output$dl_simple <- downloadHandler(filename = function() { t <- thr(); paste0("Пороги_", if (!is.null(t)) paste0(for_season(t), "_"), P(), ".xlsx") }, content = function(file) {
    t <- thr(); v <- work_table(); attr(v, "kind") <- NULL; mk <- marks()
    wb <- openxlsx::createWorkbook(); sh <- "Пороги"; openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, v, headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = "#1F4E78", fontColour = "#FFFFFF", wrapText = TRUE, valign = "center"))
    openxlsx::addStyle(wb, sh, openxlsx::createStyle(numFmt = "0.0"), rows = seq_len(nrow(v)) + 1, cols = 2:5, gridExpand = TRUE)
    openxlsx::setColWidths(wb, sh, cols = seq_len(ncol(v)), widths = c(36, 14, 14, 14, 14, if (ncol(v) > 5) 20))
    openxlsx::freezePane(wb, sh, firstRow = TRUE, firstCol = TRUE)
    s <- for_season(t)
    txt <- c(paste0("Пороги тижневої захворюваності на ГРВІ на 100 тис. населення", if (nzchar(s)) paste0(" для сезону ", sub("-", "/", s)), ". Розраховано ", format(file.mtime(file.path(proj_dir(P()), "output", "memopt_thresholds.csv")), "%d.%m.%Y"), " (проєкт «", P(), "»)."),
             "Якщо захворюваність за тиждень нижча за епідемічний поріг, епідемії немає. Від епідемічного до середнього порогу інтенсивність епідемії низька, від середнього до високого середня, від високого до дуже високого висока, вище дуже високого дуже висока.",
             if (!is.null(mk)) paste0("У колонці «", MARK_COL, "» позначено громади, де за тиждень буває лише кілька випадків, і рівень може піднятися випадково. У таких громадах середній і вищі рівні показуються лише тоді, коли захворюваність у районі без районного центру (а якщо його немає, у всьому районі) перевищила свій епідемічний поріг."),
             "Як розраховано пороги, видно в повній книзі результатів memopt.")
    openxlsx::addWorksheet(wb, "Пояснення"); openxlsx::writeData(wb, "Пояснення", txt, colNames = FALSE)
    openxlsx::setColWidths(wb, "Пояснення", cols = 1, widths = 120)
    openxlsx::addStyle(wb, "Пояснення", openxlsx::createStyle(wrapText = TRUE, valign = "top"), rows = seq_along(txt), cols = 1)
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  })
  output$dl_xlsx <- downloadHandler(filename = function() paste0("memopt_результати_", P(), ".xlsx"), content = function(file) {
    src <- file.path(proj_dir(P()), "output", "memopt_results.xlsx"); mk <- marks()
    if (is.null(mk)) return(file.copy(src, file))
    # аркуш «Пороги» переписується з новою колонкою після «Дуже високий поріг»; решта книги без змін
    wb <- openxlsx::loadWorkbook(src); sh <- "Пороги"
    p <- openxlsx::readWorkbook(wb, sh, check.names = FALSE, sep.names = " ")
    val <- ifelse(is.na(match(p$Код, mk$id)), "", mk$label[match(p$Код, mk$id)])
    at <- match("Дуже високий поріг", names(p)); if (is.na(at)) at <- ncol(p)
    p <- data.frame(p[seq_len(at)], setNames(list(val), MARK_COL), p[-seq_len(at)], check.names = FALSE)
    pos <- match(sh, names(wb)); openxlsx::removeWorksheet(wb, sh); openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, p, headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = "#1F4E78", fontColour = "#FFFFFF", wrapText = TRUE, valign = "center"))
    openxlsx::setColWidths(wb, sh, cols = seq_len(ncol(p)), widths = "auto"); openxlsx::freezePane(wb, sh, firstRow = TRUE, firstCol = TRUE)
    n <- length(names(wb)); ord <- seq_len(n - 1); openxlsx::worksheetOrder(wb) <- append(ord, n, after = pos - 1)
    if ("Зміст" %in% names(wb)) {
      z <- openxlsx::readWorkbook(wb, "Зміст", check.names = FALSE, sep.names = " "); r <- match(sh, z$Аркуш)
      if (!is.na(r)) openxlsx::writeData(wb, "Зміст", paste0(z[r, 3], " У колонці «", MARK_COL, "» позначено громади, у яких середній і вищі рівні показуються лише тоді, коли захворюваність у районі без районного центру (а якщо його немає, у всьому районі) перевищила свій епідемічний поріг."),
                                         startCol = 3, startRow = r + 1)
    }
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  })
  output$dl_csv <- downloadHandler(filename = function() paste0("memopt_пороги_", P(), ".csv"), content = function(file) {
    t <- read.csv(file.path(proj_dir(P()), "output", "memopt_thresholds.csv"), stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE)
    mk <- marks(); if (!is.null(mk)) t$few_cases <- ifelse(is.na(match(t$id, mk$id)), "", sub(",.*", "", mk$label[match(t$id, mk$id)]))
    write.csv(t, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  })

  # ----- громади з малою кількістю випадків
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
    tags$p(tags$span(class = "rule-num", n), sprintf(" %s з %d: %d через малу кількість випадків, %d позначено вручну.", plur(n, "громада", "громади", "громад"), nrow(rt),
           sum(rt$by_rule), sum(rt$manual & !rt$by_rule)))
  })
  output$rule_list <- renderDT({
    rt <- rule_now(); req(rt)
    v <- data.frame(Громада = rt$name, Населення = rt$population, `Середній поріг на 100 тис.` = round(rt$medium, 1),
                    `Середній поріг, випадків на тиждень` = round(rt$cases, 1),
                    Позначено = ifelse(rt$by_rule, "так, мало випадків", ifelse(rt$manual, "так, додано вручну", "ні")), check.names = FALSE)
    datatable(v[order(v$Позначено == "ні", v$`Середній поріг, випадків на тиждень`), ], rownames = FALSE,
              options = list(pageLength = 15, language = lang(search = "Пошук:"))) |>
      formatStyle("Позначено", color = styleEqual(c("ні"), c("#5b6573"), default = "#17345E"), fontWeight = "bold") |>
      formatRound(c("Середній поріг на 100 тис.", "Середній поріг, випадків на тиждень"), digits = 1, dec.mark = ",", mark = " ") |>
      formatRound("Населення", digits = 0, mark = " ")
  })
  output$rule_sens <- renderTable({
    rt <- rule_now(); req(rt)
    data.frame(`Замало, якщо менше ніж (випадків на тиждень)` = as.integer(SENS_LEVELS),
               `Громад з малою кількістю випадків` = vapply(SENS_LEVELS, function(l) sum(rt$cases < l), integer(1)),
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
    showNotification(sprintf("Збережено: позначено %d %s.", length(on), plur(length(on), "громада", "громади", "громад")), type = "message"); rv$tick <- rv$tick + 1
  })

  output$next_small <- renderUI(next_bar("small", if (step_done()[["run"]]) "Перевірте позначені громади і натисніть «Зберегти»." else "Спершу розрахуйте пороги на кроці 4."))

  output$settings <- renderTable({
    rv$settings_saved
    f <- file.path(proj_dir(P()), "config.json"); if (!file.exists(f)) return(NULL)
    c <- fromJSON(f, simplifyVector = TRUE)
    data.frame(Параметр = c("Кути нахилу, що перебираються", "Кут, за яким визначаються межі епідемії під час перевірки порогів", "Найбільше сезонів у розрахунку", "Межа падіння порогу на кроці кута",
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
    if (exists("notify_load")) notify_load(session, cfg)
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
    w <- input$workers
    if (is.na(w) || w < 1) { showNotification("Кількість ядер має бути не менше 1.", type = "error"); return(FALSE) }
    # для перевірки порогів потрібні щонайменше два завершені сезони, починаючи з указаного в полі
    ctl <- data_seasons(); ctl <- ctl[ctl >= input$era_start & !(ctl %in% names(AUTO_EXCLUDE))]
    if (isTRUE(input$era_first_not_ctl)) ctl <- setdiff(ctl, input$era_start)
    if (length(data_seasons()) && length(ctl) < 2) {
      showNotification(sprintf("Налаштування не збережено. Для перевірки порогів потрібні щонайменше два завершені сезони, починаючи з %s, а в даних %s. Залиште в полі «%s» попередній сезон.",
                               input$era_start, if (length(ctl)) paste("лише", ctl) else "жодного", "Сезони для перевірки порогів: починаючи з"), type = "error", duration = 20)
      return(FALSE)
    }
    cfg <- read_cfg(P())
    cfg$season_selection$era_start <- input$era_start
    cfg$season_selection$not_validation <- if (isTRUE(input$era_first_not_ctl)) list(input$era_start) else list()
    all_ex <- rv$ex[rv$ex$Код == "", ]; one_ex <- rv$ex[rv$ex$Код != "", ]
    cfg$exclude_seasons <- if (nrow(all_ex)) setNames(as.list(all_ex$Причина), all_ex$Сезон) else setNames(list(), character(0))
    cfg$exclude_territory_seasons <- setNames(list(), character(0))
    for (id in unique(one_ex$Код)) { r <- one_ex[one_ex$Код == id, ]; cfg$exclude_territory_seasons[[id]] <- setNames(as.list(r$Причина), r$Сезон) }
    cfg$parallel$workers <- as.integer(min(w, parallel::detectCores()))
    if (exists("notify_save")) { cfg <- notify_save(input, session, cfg, proj_dir(P())); if (is.null(cfg)) return(FALSE) }
    write_cfg(P(), cfg)
    rv$settings_saved <- format(Sys.time(), "%H:%M")
    TRUE
  }
  observeEvent(input$save_settings, save_all())
  if (exists("notify_server")) notify_server(input, output, session, P, rv, save_all, ROOT)
  output$settings_saved <- renderUI({
    if (is.null(rv$settings_saved)) return(NULL)
    tags$p(class = "text-success", style = "margin-top:10px", sprintf("Збережено о %s. Нові налаштування подіють під час наступного розрахунку порогів.", rv$settings_saved))
  })

  if (exists("dashboard_server")) dashboard_server(input, output, session, P, rv)

  session$onSessionEnded(function() { j <- isolate(rv$job); if (!is.null(j) && j$is_alive()) j$kill() })
}

shinyApp(ui, server)
