# Незалежна розмітка епідемічних тижнів моделлю двох прихованих станів (як у QUEST, Sciensano 2025; Le Strat і Carrat 1999)
# для перевірки memopt. Не входить у пакет для ЦГЗ. Потрібен пакет depmixS4.
#
# Модель бачить усі сезони території разом (кожен сезон окремий ряд) і для кожного тижня дає імовірність того, що він
# епідемічний. Межі епідемії сезону: найдовший відрізок тижнів з імовірністю понад 0,5 (розриви до gap тижнів зшиваються).
# Сезон без такого відрізка довжиною не менше min_len тижнів вважається сезоном без епідемії.

mo_hmm_fit <- function(d, nstart = 100, seed = 42) {
  S <- names(d); n <- nrow(d)
  df <- data.frame(rate = unlist(d, use.names = FALSE))
  set.seed(seed)
  m <- depmixS4::depmix(rate ~ 1, data = df, nstates = 2, ntimes = rep(n, length(S)), family = stats::gaussian())
  f <- NULL
  invisible(utils::capture.output(f <- tryCatch(
    depmixS4::fit(m, emcontrol = depmixS4::em.control(tol = 1e-12, maxit = 2000), nstart = nstart), error = function(e) NULL)))
  if (is.null(f)) return(NULL)
  p <- depmixS4::getpars(f); k <- length(p)
  mu <- c(p[k - 3], p[k - 1]); sdv <- c(p[k - 2], p[k]); hi <- which.max(mu); lo <- 3 - hi
  tr <- matrix(p[3:6], 2, byrow = TRUE)
  post <- depmixS4::posterior(f, type = "smoothing")[, hi]
  list(mean_low = mu[lo], mean_epi = mu[hi], sd_low = sdv[lo], sd_epi = sdv[hi], stay_low = tr[lo, lo], stay_epi = tr[hi, hi],
       prob = matrix(post, nrow = n, dimnames = list(rownames(d), S)))
}

mo_hmm_period <- function(p, gap = 2, min_len = 3) {
  idx <- which(!is.na(p) & p > 0.5)
  if (!length(idx)) return(c(start = NA_real_, end = NA_real_))
  brk <- c(0, which(diff(idx) > gap + 1), length(idx))
  runs <- lapply(seq_len(length(brk) - 1), function(i) idx[(brk[i] + 1):brk[i + 1]])
  len <- vapply(runs, function(r) max(r) - min(r) + 1, numeric(1))
  r <- runs[[which.max(len)]]
  if (max(len) < min_len) return(c(start = NA_real_, end = NA_real_))
  c(start = min(r), end = max(r))
}

# Межі епідемії за моделлю для всіх сезонів: список як у memtiming (start, end індекси рядків) і сезони без епідемії
mo_hmm_truth <- function(h) {
  S <- colnames(h$prob)
  tms <- lapply(S, function(s) mo_hmm_period(h$prob[, s])); names(tms) <- S
  list(tms = tms, noepi = S[vapply(tms, function(t) any(is.na(t)), logical(1))])
}
