# Перевірка файлів з рядами: кожен аркуш кожного файлу в datasets. Повертає таблицю повідомлень з точним місцем
# (файл, аркуш, клітинка) і порадою, як виправити. Рівень «помилка» не дає запустити розрахунок, «увага» лише попереджає.
WEEKS_ALL <- c(40:52, 1:20)

# Сезон ще триває, якщо сьогодні не пізніше неділі 20-го тижня (ISO) його другого року; ядро такий сезон відкидає саме
season_running <- function(s, end_week = 20, today = Sys.Date()) {
  if (!grepl("^[0-9]{4}/[0-9]{4}$", s)) return(FALSE)
  y <- as.integer(substr(s, 6, 9)); j4 <- as.Date(sprintf("%d-01-04", y))
  today <= j4 - (as.integer(format(j4, "%u")) - 1) + end_week * 7 - 1
}

if (!exists("plur")) plur <- function(n, one, few, many) {
  n <- abs(as.integer(n))
  if (n %% 10 == 1 && n %% 100 != 11) one else if (n %% 10 %in% 2:4 && !(n %% 100 %in% 12:14)) few else many
}

col_letter <- function(i) {
  s <- ""
  while (i > 0) { r <- (i - 1) %% 26; s <- paste0(LETTERS[r + 1], s); i <- (i - 1) %/% 26 }
  s
}

msg <- function(level, file, sheet, where, what, fix) data.frame(Рівень = level, Файл = file, Аркуш = sheet, Де = where, `Що не так` = what, `Як виправити` = fix, check.names = FALSE)

dataset_files <- function(dir) {
  f <- list.files(dir, pattern = "\\.xlsx$", full.names = TRUE)
  f[!startsWith(basename(f), "~$")]
}

# Усі аркуші всіх файлів: файл, аркуш, сезони
scan_sheets <- function(dir) {
  out <- list()
  for (f in dataset_files(dir)) {
    sh <- tryCatch(readxl::excel_sheets(f), error = function(e) character(0))
    for (s in sh) {
      hdr <- tryCatch(names(readxl::read_excel(f, sheet = s, n_max = 0)), error = function(e) character(0))
      seasons <- hdr[grepl("^[0-9]{4}/[0-9]{4}$", hdr)]
      out[[length(out) + 1]] <- data.frame(file = basename(f), sheet = s, seasons = length(seasons),
                                           first = if (length(seasons)) min(seasons) else NA, last = if (length(seasons)) max(seasons) else NA)
    }
  }
  if (length(out)) do.call(rbind, out) else data.frame(file = character(0), sheet = character(0), seasons = integer(0), first = character(0), last = character(0))
}

check_sheet <- function(path, sheet) {
  fn <- basename(path); res <- list()
  add <- function(...) res[[length(res) + 1]] <<- msg(...)
  x <- tryCatch(as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text", .name_repair = "minimal")), error = function(e) NULL)
  if (is.null(x)) { add("помилка", fn, sheet, "", "аркуш не вдалося прочитати", "Відкрийте файл в Excel, перевірте, що він не пошкоджений, і збережіть як .xlsx"); return(do.call(rbind, res)) }
  if (ncol(x) < 2 || nrow(x) == 0) { add("помилка", fn, sheet, "", "аркуш порожній або має лише одну колонку", "У першій колонці мають бути тижні, далі колонка на кожен сезон"); return(do.call(rbind, res)) }
  hdr <- names(x)
  # сезони
  for (j in 2:ncol(x)) {
    h <- hdr[j]
    if (!nzchar(h) && all(is.na(x[[j]]))) next
    if (!grepl("^[0-9]{4}/[0-9]{4}$", h)) {
      add("помилка", fn, sheet, paste0(col_letter(j), "1"), sprintf("назва колонки «%s» не схожа на сезон", h), "Назвіть колонку сезоном у вигляді 2023/2024 або видаліть її")
    } else if (as.integer(substr(h, 6, 9)) != as.integer(substr(h, 1, 4)) + 1) {
      add("помилка", fn, sheet, paste0(col_letter(j), "1"), sprintf("сезон «%s»: роки не йдуть підряд", h), "Сезон охоплює два сусідні роки, наприклад 2023/2024")
    }
  }
  dup <- hdr[duplicated(hdr) & nzchar(hdr)]
  for (d in unique(dup)) add("помилка", fn, sheet, "рядок 1", sprintf("сезон «%s» записано двічі", d), "Залиште одну колонку цього сезону")
  # тижні
  w <- suppressWarnings(as.numeric(x[[1]]))
  bad_w <- which(!is.na(x[[1]]) & (is.na(w) | !(w %in% c(WEEKS_ALL, 21:39, 53))))
  for (i in head(bad_w, 5)) add("помилка", fn, sheet, paste0("A", i + 1), sprintf("у першій колонці «%s», а має бути номер тижня", x[[1]][i]), "У першій колонці лише номери тижнів: від 40 до 52, потім від 1 до 20")
  miss <- setdiff(WEEKS_ALL, w)
  if (length(miss) && length(miss) < 33) add("увага", fn, sheet, "колонка A", paste("немає рядків для тижнів", paste(miss, collapse = ", ")), "Додайте ці тижні, навіть якщо даних за них немає (клітинки можна лишити порожніми)")
  if (length(miss) == 33) add("помилка", fn, sheet, "колонка A", "у першій колонці немає номерів тижнів", "У першій колонці мають бути тижні від 40 до 52, потім від 1 до 20")
  if (any(duplicated(w[!is.na(w)]))) add("помилка", fn, sheet, "колонка A", paste("тиждень повторюється:", paste(unique(w[duplicated(w) & !is.na(w)]), collapse = ", ")), "Кожен тиждень має бути в одному рядку")
  # значення
  for (j in 2:ncol(x)) {
    if (!grepl("^[0-9]{4}/[0-9]{4}$", hdr[j])) next
    v <- x[[j]]; num <- suppressWarnings(as.numeric(gsub(",", ".", v)))
    txt <- which(!is.na(v) & nzchar(trimws(v)) & is.na(num))
    if (length(txt)) add("помилка", fn, sheet, paste(paste0(col_letter(j), head(txt, 4) + 1), collapse = ", "),
                         sprintf("у сезоні %s є текст замість числа (%s)", hdr[j], paste0("«", head(v[txt], 3), "»", collapse = ", ")), "Впишіть число або залиште клітинку порожньою")
    neg <- which(!is.na(num) & num < 0)
    if (length(neg)) add("помилка", fn, sheet, paste(paste0(col_letter(j), head(neg, 4) + 1), collapse = ", "), sprintf("у сезоні %s є від'ємні значення", hdr[j]), "Показник на 100 тис. не може бути від'ємним")
    big <- which(!is.na(num) & num > 10000)
    if (length(big)) add("увага", fn, sheet, paste(paste0(col_letter(j), head(big, 4) + 1), collapse = ", "), sprintf("у сезоні %s значення понад 10 000", hdr[j]),
                         "Перевірте, що це показник на 100 тис. населення за тиждень, а не кількість випадків")
    nw <- sum(!is.na(num))
    if (nw > 0 && nw < 10 && !season_running(hdr[j])) add("увага", fn, sheet, col_letter(j), sprintf("у сезоні %s лише %d %s з даними", hdr[j], nw, plur(nw, "тиждень", "тижні", "тижнів")),
                                                          "Сезон з менш ніж 10 тижнями даних у розрахунок не потрапить")
  }
  if (length(res)) do.call(rbind, res) else NULL
}

check_datasets <- function(dir) {
  files <- dataset_files(dir)
  if (!length(files)) return(msg("помилка", "", "", "", "немає жодного файлу з рядами", "Завантажте файли Excel на вкладці «Дані»"))
  res <- list()
  sheets <- scan_sheets(dir)
  d <- sheets$sheet[duplicated(sheets$sheet)]
  for (s in unique(d)) res[[length(res) + 1]] <- msg("помилка", paste(sheets$file[sheets$sheet == s], collapse = ", "), s, "", "аркуш з такою назвою є в кількох файлах",
                                                       "Назва аркуша це назва території, вона має бути єдиною: перейменуйте або видаліть зайвий аркуш")
  for (f in files) for (s in tryCatch(readxl::excel_sheets(f), error = function(e) character(0))) res[[length(res) + 1]] <- check_sheet(f, s)
  res <- res[!vapply(res, is.null, logical(1))]
  if (length(res)) do.call(rbind, res) else NULL
}
