# Перелік територій будується з назв аркушів: кожен аркуш це територія. Вид території вгадується з назви, користувач
# виправляє його і вказує для громад район і населення. Наявний territories.csv проєкту зберігається: його рядки
# зіставляються з аркушами за файлом і аркушем, тому вже введене не губиться.
TYPES <- c("область", "район", "район без центру", "місто", "громада", "не використовувати")
HKINDS <- c("міська", "селищна", "сільська")

guess_type <- function(name) {
  n <- tolower(name)
  if (grepl("област", n)) return("область")
  if (grepl("без центру|без м\\.|без міста|^без ", n)) return("район без центру")
  if (grepl("район", n)) return("район")
  if (grepl("^м\\.|^місто", n)) return("місто")
  if (grepl("громад|\\bотг\\b|\\bтг\\b", n)) return("громада")
  "громада"
}

guess_kind_of_hromada <- function(name) {
  n <- tolower(name)
  if (grepl("міськ", n)) "міська" else if (grepl("селищн", n)) "селищна" else if (grepl("сільськ", n)) "сільська" else NA_character_
}

# Таблиця для редагування: аркуші проєкту, доповнені вже збереженим переліком
territory_table <- function(proj) {
  sh <- scan_sheets(file.path(proj, "datasets"))
  old_f <- file.path(proj, "territories.csv")
  old <- if (file.exists(old_f)) read.csv(old_f, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"), check.names = FALSE) else NULL
  if (!is.null(old) && names(old)[1] != "id") names(old)[1] <- "id"
  rows <- lapply(seq_len(nrow(sh)), function(i) {
    f <- sh$file[i]; s <- sh$sheet[i]
    o <- if (!is.null(old)) old[old$file == f & old$sheet == s, , drop = FALSE] else NULL
    if (!is.null(o) && nrow(o)) {
      o <- o[1, ]
      tp <- if (!is.na(o$level) && o$level %in% TYPES) o$level else if (identical(o$kind, "small")) "громада" else guess_type(o$name)
      dist_id <- if (tp == "громада") o$group else NA
      hk <- if (tp != "громада") NA_character_ else if (!is.null(o$type) && !is.na(o$type) && o$type %in% HKINDS) o$type else guess_kind_of_hromada(o$name)
      data.frame(Код = o$id, Назва = o$name, Тип = tp, `Вид громади` = hk, Район = NA_character_, `.dist_id` = dist_id, `.ref_id` = o$reference_id,
                 Населення = suppressWarnings(as.numeric(o$population)), Файл = f, Аркуш = s, Сезонів = sh$seasons[i], check.names = FALSE)
    } else {
      tp <- guess_type(s)
      data.frame(Код = NA_character_, Назва = s, Тип = tp, `Вид громади` = if (tp == "громада") guess_kind_of_hromada(s) else NA_character_, Район = NA_character_, `.dist_id` = NA, `.ref_id` = NA,
                 Населення = NA_real_, Файл = f, Аркуш = s, Сезонів = sh$seasons[i], check.names = FALSE)
    }
  })
  tb <- if (length(rows)) do.call(rbind, rows) else data.frame(Код = character(0), Назва = character(0), Тип = character(0), `Вид громади` = character(0), Район = character(0), `.dist_id` = character(0),
                                                              `.ref_id` = character(0), Населення = numeric(0), Файл = character(0), Аркуш = character(0), Сезонів = integer(0), check.names = FALSE)
  # район для громад і районів без центру: з наявного переліку (код району), інакше з назви
  dn <- setNames(tb$Назва, tb$Код)
  for (i in seq_len(nrow(tb))) {
    if (tb$Тип[i] == "громада" && !is.na(tb$.dist_id[i]) && tb$.dist_id[i] %in% names(dn)) tb$Район[i] <- dn[[tb$.dist_id[i]]]
    if (tb$Тип[i] == "район без центру") {
      stem <- trimws(sub("(?i)\\s*без центру.*$", "", tb$Назва[i], perl = TRUE))
      hit <- tb$Назва[tb$Тип == "район" & startsWith(tb$Назва, stem)]
      if (length(hit)) tb$Район[i] <- hit[1]
    }
  }
  tb
}

# Перевірка переліку: повертає таблицю повідомлень (рівень, територія, що не так, як виправити)
check_territories <- function(tb) {
  out <- list()
  add <- function(level, who, what, fix) out[[length(out) + 1]] <<- data.frame(Рівень = level, Територія = who, `Що не так` = what, `Як виправити` = fix, check.names = FALSE)
  use <- tb[tb$Тип != "не використовувати", , drop = FALSE]
  districts <- use$Назва[use$Тип == "район"]
  if (!any(use$Тип %in% c("область", "район", "місто", "район без центру"))) add("помилка", "", "немає жодної основної території (область, район, місто)", "Позначте тип хоча б для області або районів")
  dup <- use$Назва[duplicated(use$Назва)]
  for (d in unique(dup)) add("помилка", d, "дві території з однаковою назвою", "Змініть назву однієї з них у колонці «Назва»")
  bad <- use$Назва[!(use$Тип %in% TYPES)]
  for (b in bad) add("помилка", b, "тип території не визначено", paste("Оберіть тип:", paste(TYPES, collapse = ", ")))
  for (i in which(use$Тип == "громада")) {
    nm <- use$Назва[i]
    if (is.na(use$Район[i]) || !nzchar(use$Район[i])) add("помилка", nm, "не вказано район громади", "Впишіть у колонку «Район» назву району так само, як вона записана в переліку")
    else if (!(use$Район[i] %in% districts)) add("помилка", nm, sprintf("район «%s» не знайдено серед територій типу «район»", use$Район[i]), "Перевірте написання або тип району")
    if (is.na(use$`Вид громади`[i]) || !(use$`Вид громади`[i] %in% HKINDS)) add("помилка", nm, "не вказано вид громади", "Впишіть у колонку «Вид громади» одне зі слів: міська, селищна, сільська. Від виду залежить еталон: міська громада спирається на весь район, селищна і сільська на район без центру")
    if (is.na(use$Населення[i]) || use$Населення[i] <= 0) add("помилка", nm, "не вказано населення громади", "Впишіть населення в колонку «Населення» або завантажте файл населення")
    if (!is.na(use$Сезонів[i]) && use$Сезонів[i] < 3) add("увага", nm, sprintf("лише %d сезон(и) даних", use$Сезонів[i]), "Для порогу потрібно щонайменше 3 сезони; поріг громади спиратиметься на район")
  }
  for (i in which(use$Тип == "район без центру")) if (is.na(use$Район[i]) || !(use$Район[i] %in% districts))
    add("увага", use$Назва[i], "не знайдено район, до якого належить ця територія", "Впишіть у колонку «Район» назву відповідного району")
  if (length(out)) do.call(rbind, out) else NULL
}

# Населення з файлу: перша колонка назва, друга населення (Excel або CSV). Повертає оновлену таблицю і назви без збігу.
apply_population <- function(tb, path) {
  p <- if (grepl("\\.csv$", path, ignore.case = TRUE)) read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE) else as.data.frame(readxl::read_excel(path))
  nm <- trimws(as.character(p[[1]])); pop <- suppressWarnings(as.numeric(gsub("[ ,]", "", as.character(p[[2]]))))
  hit <- match(nm, tb$Назва)
  tb$Населення[hit[!is.na(hit)]] <- pop[!is.na(hit)]
  if (ncol(p) >= 3) { ds <- trimws(as.character(p[[3]])); tb$Район[hit[!is.na(hit)]] <- ds[!is.na(hit)] }
  miss <- nm[is.na(hit) & nzchar(nm)]
  sugg <- vapply(miss, function(m) { a <- agrep(m, tb$Назва, max.distance = 0.2, value = TRUE); if (length(a)) a[1] else "" }, character(1))
  list(tb = tb, unmatched = data.frame(`Назва у файлі` = miss, `Схожа назва в переліку` = sugg, check.names = FALSE))
}

# Запис territories.csv у форматі ядра. Еталон громади: міська громада спирається на весь район, сільська і селищна на
# район без центру, якщо він є, інакше на весь район.
write_territories <- function(tb, proj) {
  old_f <- file.path(proj, "territories.csv")
  old <- if (file.exists(old_f)) read.csv(old_f, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"), check.names = FALSE) else NULL
  use <- tb[tb$Тип != "не використовувати", , drop = FALSE]
  ids <- use$Код
  ids[is.na(ids) | !nzchar(ids)] <- sprintf("T%03d", seq_len(sum(is.na(ids) | !nzchar(ids))) + 100)
  while (any(duplicated(ids))) ids[duplicated(ids)] <- paste0(ids[duplicated(ids)], "x")
  use$Код <- ids
  id_of <- setNames(use$Код, use$Назва)
  nc_of <- setNames(use$Код[use$Тип == "район без центру"], use$Район[use$Тип == "район без центру"])
  obl <- use$Код[use$Тип == "область"][1]
  ref <- group <- rep(NA_character_, nrow(use)); htype <- rep(NA_character_, nrow(use))
  for (i in seq_len(nrow(use))) {
    if (use$Тип[i] == "громада") {
      d <- id_of[use$Район[i]]; htype[i] <- use$`Вид громади`[i]; group[i] <- d
      ref[i] <- if (!identical(htype[i], "міська") && !is.na(nc_of[use$Район[i]])) nc_of[[use$Район[i]]] else d
    } else if (use$Тип[i] == "район") group[i] <- obl
  }
  keep <- function(col) if (!is.null(old) && col %in% names(old)) old[[col]][match(use$Код, old[[1]])] else NA
  out <- data.frame(id = use$Код, name = use$Назва, level = use$Тип, kind = ifelse(use$Тип == "громада", "small", "main"), file = use$Файл, sheet = use$Аркуш,
                    population = ifelse(use$Тип == "громада", use$Населення, NA), reference_id = ref, type = htype, group = group,
                    export_dim = keep("export_dim"), district_confirm = keep("district_confirm"), check.names = FALSE)
  write.csv(out, old_f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(out)
}
