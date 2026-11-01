# Встановлення пакетів R, потрібних для memopt. Запускається файлом "2_Встановити_пакети.bat"
# або вручну: Rscript install_packages.R. Потрібен доступ до інтернету (один раз).
options(repos = c(CRAN = "https://cloud.r-project.org"), warn = 1)

# Якщо R встановлено в Program Files без прав адміністратора, пакети ставляться в особисту бібліотеку.
lib_user <- Sys.getenv("R_LIBS_USER")
if (nzchar(lib_user) && !file.exists(lib_user)) dir.create(lib_user, recursive = TRUE, showWarnings = FALSE)
if (nzchar(lib_user)) .libPaths(c(lib_user, .libPaths()))
lib_target <- .libPaths()[1]
cat("Версія R:", R.version.string, "\n")
cat("Пакети ставляться у:", lib_target, "\n\n")
if (getRversion() < "4.2") cat("УВАГА: потрібен R 4.2 або новіший. Оновіть R з https://cran.r-project.org/bin/windows/base/\n")

# Обов'язкові: без них розрахунок не запуститься.
#   mem       метод рухомої епідемії (Lozano, Vega), версія не старіша за 2.18
#   readxl    читання рядів з Excel
#   openxlsx  запис книги результатів
#   jsonlite  читання config.json
#   dplyr, tidyr  обробка таблиць
#   shiny, bslib, DT, callr  застосунок memopt (вікно в браузері, таблиці, розрахунок у фоні)
#   curl      сповіщення поштою і в Telegram
required <- c("mem", "readxl", "openxlsx", "jsonlite", "dplyr", "tidyr", "shiny", "bslib", "DT", "callr", "curl")

installed <- function() rownames(installed.packages())
missing_req <- setdiff(required, installed())
if (length(missing_req)) {
  cat("Встановлюю пакети:", paste(missing_req, collapse = ", "), "\n")
  install.packages(missing_req, lib = lib_target, dependencies = TRUE)
} else cat("Усі пакети вже встановлено.\n")

if ("mem" %in% installed() && packageVersion("mem") < "2.18") {
  cat("Оновлюю mem до поточної версії з CRAN\n")
  install.packages("mem", lib = lib_target)
}

cat("\nПідсумок\n")
still <- setdiff(required, installed())
if (length(still)) {
  cat("НЕ встановлено:", paste(still, collapse = ", "), "\n")
  cat("Розрахунок не запуститься. Перевірте інтернет і запустіть цей файл ще раз.\n")
} else {
  cat("Усі пакети встановлено:\n")
  for (p in required) cat("  ", p, as.character(packageVersion(p)), "\n")
  cat("Можна запускати 3_Розрахувати_пороги.bat\n")
}
