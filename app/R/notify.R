# Сповіщення поштою (SMTP) і в Telegram. Налаштування в config.json проєкту (notifications), паролі й токени окремо
# в secrets.json тієї самої папки: цей файл не потрапляє ні в налаштування, ні в пакет. Цей файл використовують
# і застосунок, і розрахунок за розкладом (run_scheduled.R), тому він не залежить від shiny.

read_secrets <- function(proj) {
  f <- file.path(proj, "secrets.json")
  if (file.exists(f)) jsonlite::fromJSON(f, simplifyVector = FALSE) else list()
}

write_secrets <- function(proj, s) {
  writeLines(jsonlite::toJSON(s, auto_unbox = TRUE, pretty = TRUE, null = "null"), file.path(proj, "secrets.json"), useBytes = TRUE)
}

# Заголовок листа в UTF-8 (кирилиця в темі)
mime_subject <- function(x) paste0("=?UTF-8?B?", jsonlite::base64_enc(charToRaw(enc2utf8(x))), "?=")

send_email <- function(n, sec, subject, text) {
  to <- unlist(n$emails)
  if (!length(to)) stop("не вказано жодної адреси")
  sm <- n$smtp
  if (is.null(sm$server) || !nzchar(sm$server)) stop("не вказано поштовий сервер")
  port <- as.integer(if (is.null(sm$port)) 465 else sm$port)
  url <- sprintf("%s://%s:%d", if (port == 465) "smtps" else "smtp", sm$server, port)
  from <- if (!is.null(sm$from) && nzchar(sm$from)) sm$from else sm$user
  msg <- paste0("From: ", from, "\r\nTo: ", paste(to, collapse = ", "), "\r\nSubject: ", mime_subject(subject),
                "\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=UTF-8\r\nContent-Transfer-Encoding: 8bit\r\n\r\n",
                enc2utf8(text), "\r\n")
  curl::send_mail(mail_from = from, mail_rcpt = to, message = msg, smtp_server = url,
                  username = sm$user, password = sec$smtp_password, use_ssl = if (port == 465) "force" else "try", verbose = FALSE)
  invisible(TRUE)
}

send_telegram <- function(n, sec, text) {
  token <- sec$telegram_token
  chats <- unlist(n$telegram$chats)
  if (is.null(token) || !nzchar(token)) stop("не вказано токен бота")
  if (!length(chats)) stop("не вказано жодного чату")
  for (ch in chats) {
    h <- curl::new_handle()
    curl::handle_setform(h, chat_id = as.character(ch), text = enc2utf8(text))
    r <- curl::curl_fetch_memory(sprintf("https://api.telegram.org/bot%s/sendMessage", token), handle = h)
    if (r$status_code != 200) stop(sprintf("Telegram відповів кодом %d для чату %s: %s", r$status_code, ch, rawToChar(r$content)))
  }
  invisible(TRUE)
}

# Надіслати сповіщення обраним способом. Повертає опис результату для кожного каналу (помилки не зупиняють інші канали).
notify <- function(proj, subject, text) {
  cfg <- jsonlite::fromJSON(file.path(proj, "config.json"), simplifyVector = FALSE)
  n <- cfg$notifications; if (is.null(n)) n <- list()
  method <- if (is.null(n$method)) "none" else n$method
  sec <- read_secrets(proj)
  res <- character(0)
  if (method %in% c("email", "both")) res <- c(res, tryCatch({ send_email(n, sec, subject, text); "пошта: надіслано" }, error = function(e) paste("пошта: помилка:", conditionMessage(e))))
  if (method %in% c("telegram", "both")) res <- c(res, tryCatch({ send_telegram(n, sec, paste0(subject, "\n\n", text)); "Telegram: надіслано" }, error = function(e) paste("Telegram: помилка:", conditionMessage(e))))
  if (!length(res)) res <- "сповіщення вимкнено"
  res
}

# ----- розклад: завдання планувальника Windows для проєкту
task_name <- function(project) paste0("memopt ", project)

task_status <- function(project) {
  out <- suppressWarnings(system2("schtasks", c("/Query", "/TN", paste0("\"", task_name(project), "\""), "/FO", "LIST"), stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) return(NULL)
  out
}

task_create <- function(project, proj_dir, root, day = "MON", time = "09:00") {
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  # команда завдання: три шляхи в лапках; для schtasks внутрішні лапки екрануються зворотною скісною рискою
  q <- function(p) paste0('\\"', normalizePath(p, winslash = "\\", mustWork = FALSE), '\\"')
  tr <- paste0('"', q(rscript), " ", q(file.path(root, "run_scheduled.R")), " ", q(proj_dir), '"')
  out <- suppressWarnings(system2("schtasks", c("/Create", "/F", "/SC", "WEEKLY", "/D", day, "/ST", time, "/TN", paste0('"', task_name(project), '"'), "/TR", tr), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  if (!is.null(st) && st != 0) stop(paste(out, collapse = " "))
  invisible(out)
}

task_delete <- function(project) {
  suppressWarnings(system2("schtasks", c("/Delete", "/F", "/TN", paste0("\"", task_name(project), "\"")), stdout = TRUE, stderr = TRUE))
}
