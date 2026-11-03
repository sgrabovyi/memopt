# Перевірка файлів з рядами: кожен аркуш кожного файлу в datasets. Повертає таблицю повідомлень з точним місцем
# (файл, аркуш, клітинка) і порадою, як виправити. Рівень «помилка» не дає запустити розрахунок, «увага» лише попереджає.
WEEKS_ALL <- c(40:52, 1:20)

# Сезон ще триває, якщо сьогодні не пізніше неділі 20-го тижня (ISO) його другого року; ядро такий сезон відкидає саме
season_running <- function(s, end_week = 20, today = Sys.Date()) {
  if (!grepl("^[0-9]{4}/[0-9]{4}$", s)) return(FALSE)
  y <- as.integer(substr(s, 6, 9)); j4 <- as.Date(sprintf("%d-01-04", y))
  today <= j4 - (as.integer(format(j4, "%u")) - 1) + end_week * 7 - 1
}

# Сезон «РРРР/РРРР+1» має 53-й тиждень, якщо 28 грудня року РРРР припадає на 53-й ISO-тиждень (те саме правило в ядрі)
has_week53 <- function(s) grepl("^[0-9]{4}/[0-9]{4}$", s) && format(as.Date(sprintf("%s-12-28", substr(s, 1, 4))), "%V") == "53"

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

# Файл з населенням громад замість рядів: у заголовку є «населення», а сезонів немає
is_population_file <- function(path) {
  sh <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
  if (!length(sh)) return(FALSE)
  hdr <- tolower(tryCatch(names(readxl::read_excel(path, sheet = sh[1], n_max = 0)), error = function(e) character(0)))
  any(grepl("населен", hdr)) && !any(grepl("^[0-9]{4}/[0-9]{4}$", hdr))
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

# Усі сезони, що трапляються в назвах колонок файлів проєкту, за зростанням
all_seasons <- function(dir) {
  s <- character(0)
  for (f in dataset_files(dir)) for (sh in tryCatch(readxl::excel_sheets(f), error = function(e) character(0))) {
    h <- tryCatch(names(readxl::read_excel(f, sheet = sh, n_max = 0)), error = function(e) character(0))
    s <- c(s, h[grepl("^[0-9]{4}/[0-9]{4}$", h)])
  }
  sort(unique(s))
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
  has_data <- function(rows) any(vapply(2:ncol(x), function(j) any(!is.na(x[[j]][rows]) & nzchar(trimws(x[[j]][rows]))), logical(1)))
  r53 <- which(w == 53)
  if (length(r53)) for (j in 2:ncol(x)) {
    if (!grepl("^[0-9]{4}/[0-9]{4}$", hdr[j])) next
    v <- x[[j]][r53[1]]; filled <- !is.na(v) && nzchar(trimws(v))
    if (filled && !has_week53(hdr[j]))
      add("помилка", fn, sheet, paste0(col_letter(j), r53[1] + 1), sprintf("у сезоні %s немає 53-го тижня, а в рядку 53 є значення", hdr[j]),
          sprintf("Рік %s має 52 тижні: перенесіть значення в правильний рядок або видаліть його", substr(hdr[j], 1, 4)))
    else if (filled) add("довідка", fn, sheet, paste0(col_letter(j), r53[1] + 1), sprintf("у сезоні %s є дані за 53-й тиждень", hdr[j]),
                         "Нічого робити не треба: у сезоні з 53-м тижнем святковими вважаються тижні 53, 1 і 2 (у звичайному сезоні 52, 1 і 2), вони в розрахунок не беруться, а 52-й тиждень іде в розрахунок зі справжніми даними")
  }
  rsum <- which(w %in% 21:39)
  if (length(rsum) && has_data(rsum)) add("довідка", fn, sheet, "колонка A", "є дані за літні тижні 21-39",
                                          "Нічого робити не треба: пороги MEM рахуються лише за тижнями від 40 до 20")
  # однакові сезони на одному аркуші: зазвичай колонку скопійовано помилково
  sc <- which(grepl("^[0-9]{4}/[0-9]{4}$", hdr))
  if (length(sc) > 1) for (a in seq_along(sc)) for (b in seq_along(sc)) if (a < b) {
    va <- suppressWarnings(as.numeric(gsub(",", ".", x[[sc[a]]]))); vb <- suppressWarnings(as.numeric(gsub(",", ".", x[[sc[b]]])))
    ok <- !is.na(va) & !is.na(vb)
    if (sum(ok) >= 10 && all(va[ok] == vb[ok]) && sum(!is.na(va)) == sum(!is.na(vb)))
      add("помилка", fn, sheet, paste0(col_letter(sc[a]), ", ", col_letter(sc[b])), sprintf("сезони %s і %s мають однакові значення", hdr[sc[a]], hdr[sc[b]]),
          "Схоже, колонку скопійовано: вставте правильні дані одного з сезонів")
  }
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
  sig <- character(0); sig_where <- character(0)
  pop <- vapply(files, is_population_file, logical(1))
  for (f in files[pop]) res[[length(res) + 1]] <- msg("помилка", basename(f), "", "", "це файл з населенням громад, а не ряди захворюваності",
                                                     "Видаліть його тут (клацніть рядок файлу і натисніть «Видалити позначений файл») і завантажте на кроці 3, «Території»")
  for (f in files[!pop]) for (s in tryCatch(readxl::excel_sheets(f), error = function(e) character(0))) {
    res[[length(res) + 1]] <- check_sheet(f, s)
    # однакові дані на різних аркушах: зазвичай аркуш території скопійовано з іншого
    v <- tryCatch(suppressWarnings(as.matrix(readxl::read_excel(f, sheet = s, col_types = "numeric"))[, -1, drop = FALSE]), error = function(e) NULL)
    if (!is.null(v) && sum(!is.na(v)) >= 20) {
      key <- paste(round(v[!is.na(v)], 3), collapse = ";")
      hit <- match(key, sig)
      if (!is.na(hit)) res[[length(res) + 1]] <- msg("помилка", basename(f), s, "", sprintf("дані аркуша повністю збігаються з аркушем «%s»", sig_where[hit]),
                                                     "Схоже, аркуш скопійовано з іншої території: вставте дані цієї території")
      sig <- c(sig, key); sig_where <- c(sig_where, s)
    }
  }
  res <- res[!vapply(res, is.null, logical(1))]
  if (length(res)) do.call(rbind, res) else NULL
}
