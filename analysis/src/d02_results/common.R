# Shared setup for the regression scripts: repository root, panels, units, samples,
# the DiD estimator (firm and day fixed effects, standard errors double-clustered by
# firm and day) and the table writer. Sourced by run_all.R and by each block.
suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(fixest)
})

ROOT <- Sys.getenv("GEO_MARKET_ROOT")          # set by run.py; otherwise walk up to the folder holding run.py
if (ROOT == "") {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  p <- if (length(f)) dirname(normalizePath(f)) else normalizePath(getwd())
  while (!file.exists(file.path(p, "run.py")) && dirname(p) != p) p <- dirname(p)
  ROOT <- p
}
DATA <- file.path(ROOT, "analysis", "data")
TABLES <- file.path(ROOT, "analysis", "output", "tables", "regressions")
FIGURES <- file.path(ROOT, "analysis", "output", "figures")
EVENT <- as.Date("2022-02-24")
SAMPLES <- c("matched", "main")            # headline, appendix
INTERNAL <- c("balanced", "eb")            # footnote only: tables go to TABLES/internal
CONTROLS <- "lmv + invp"
BINS <- c(0, 500, 1000, 1500, 2000, 3000, Inf)
BIN_LABELS <- c("0-500", "500-1000", "1000-1500", "1500-2000", "2000-3000", ">3000")
setFixest_notes(FALSE)
# table style of the manuscript: variables of interest first and the controls last (order on the raw
# names), one "TWFEs" row, then No. Obs. / within R2 / adjusted within R2
setFixest_etable(digits = 3, digits.stats = "r5", se.below = TRUE, depvar = TRUE,
                 fitstat = ~ n + wr2 + war2,
                 order = "!%^(lmv|invp)$", interaction.order = "!^War$",
                 fixef.group = list("TWFEs" = "Firm|Day"),
                 style.tex = style.tex("aer", model.format = "(1)", yesNo = c("Yes", "No"),
                                       fixef.suffix = " FEs", fixef.where = "var",
                                       fixef.title = "\\midrule", stats.title = ""))

DICT <- c(qspread = "Quoted spread (\\%)", espread = "Effective spread (\\%)",
          pimpact = "Price impact (\\%)", rspread = "Realized spread (\\%)",
          ldvol = "Dollar volume (log)", ltrades = "Trades (log)", vol = "Volatility (\\%)",
          nbr1 = "Neighbor$_1$", nbr2 = "Neighbor$_2$", nbr12 = "Neighbor$_{1,2}$", nearby = "Nearby",
          post = "War", negdist = "$-$Distance (1,000 km)", negdist_z = "$-$Distance (z)",
          intensity = "Neighbor$_{1,2}$ $\\times$ $-$Distance (z)",
          ret_war = "War return", prevol_z = "Pre-war volatility (z)", trend = "Trend",
          lmv = "Log market value", invp = "1/Price",
          dist_bin = "Distance", pimpact_c = "Price impact (\\%)", ric = "Firm", date = "Day",
          treat_post = "Neighbor$_{1,2}$ $\\times$ War", pi_tercile = "Pre-war price impact",
          n = "No. Obs.", wr2 = "R$^2$", war2 = "Adj-R$^2$")

# ---------- data ----------
load_panel <- function(which = "main") {
  cols <- c("ric", "date", "ctriso3", "indm", "nbr_1", "nbr_1_or_2", "dist_ukr", "daily_log_return",
            "qspread_mean", "espread_mean", "price_impact_mean", "realized_spread_mean",
            "dollar_volume_sum", "trades_count", "quotes_count", "volatility", "mktval", "price")
  d <- as.data.table(read_parquet(file.path(DATA, sprintf("daily_%s.parquet", which)), col_select = all_of(cols)))
  a <- as.data.table(read_parquet(file.path(DATA, "psm_assignments.parquet"),
                                  col_select = c("ric", "matched_group", "eb_weight")))
  d <- merge(d, a, by = "ric", all.x = TRUE)
  d[, date := as.Date(substr(as.character(date), 1, 10))]
  setorder(d, ric, date)
  d[, post := as.integer(date >= EVENT)]
  days <- sort(unique(d$date))
  d[, day_rel := match(date, days) - match(EVENT, days)]
  # outcomes (spreads and impacts in percent, activity in logs, volatility in percent)
  d[, `:=`(qspread = 100 * qspread_mean, espread = 100 * espread_mean,
           pimpact = 100 * price_impact_mean, rspread = 100 * realized_spread_mean,
           ldvol = log1p(dollar_volume_sum), ltrades = log1p(trades_count), vol = 100 * volatility,
           lmv = log(mktval), invp = 1 / price)]
  # treatments
  d[, `:=`(nbr1 = as.integer(nbr_1 == 1), nbr12 = as.integer(nbr_1_or_2 == 1))]
  d[, nbr2 := as.integer(nbr12 == 1 & nbr1 == 0)]
  d[, negdist := -dist_ukr / 1000]                 # per 1,000 km: dose-response tables only
  d[, dist_bin := cut(dist_ukr, BINS, labels = BIN_LABELS, right = TRUE)]
  d[, dist_bin := relevel(factor(dist_bin, levels = BIN_LABELS), ref = ">3000")]
  d[, ret_war := daily_log_return[date == EVENT][1], by = ric]      # firm return on 24 Feb (NA if none)
  d[, prevol := mean(vol[post == 0], na.rm = TRUE), by = ric]
  d[, pre_pimpact := mean(pimpact[post == 0], na.rm = TRUE), by = ric]
  d[, trend := as.numeric(date - min(date))]
  d[]
}

# sample = "matched" (pairs of the matching), "main" (all firms), "balanced" (perfectly
# balanced panel), "eb" (main, controls weighted by the entropy-balancing weights)
get_sample <- function(d_main, d_bal, sample) {
  d <- if (sample == "balanced") copy(d_bal) else copy(d_main)
  if (sample == "matched") d <- d[matched_group %in% c(0, 1)]
  if (sample == "eb") d <- d[!is.na(eb_weight)]
  d[, w := if (sample == "eb") eb_weight else 1]
  d[, nearby := nbr12]
  d[, prevol_z := (prevol - mean(prevol, na.rm = TRUE)) / sd(prevol, na.rm = TRUE)]
  # minus distance standardized within the estimation sample (one unit = one SD closer to Ukraine)
  d[, negdist_z := -(dist_ukr - mean(dist_ukr, na.rm = TRUE)) / sd(dist_ukr, na.rm = TRUE)]
  d[, intensity := nbr12 * negdist_z]
  d[, pi_tercile := cut(pre_pimpact, quantile(pre_pimpact, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                        labels = c("Low", "Mid", "High"), include.lowest = TRUE)]
  d[]
}

out_dir <- function(sample) {
  p <- if (sample %in% INTERNAL) file.path(TABLES, "internal", sample) else file.path(TABLES, sample)
  dir.create(p, recursive = TRUE, showWarnings = FALSE); p
}

# ---------- estimation ----------
did <- function(y, rhs, data, controls = TRUE, fe = "ric + date") {
  f <- as.formula(sprintf("%s ~ %s%s | %s", y, rhs, if (controls) paste0(" + ", CONTROLS) else "", fe))
  feols(f, data = data, weights = ~w, cluster = ~ ric + date)
}

# lines \begin{tabular} ... \end{tabular} of an etable (no float, no notes)
tabular_lines <- function(models, ...) {
  args <- c(list(models, tex = TRUE, float = FALSE, dict = DICT, interaction.combine = " $\\times$ "), list(...))
  x <- do.call(etable, args)   # etable cannot take `...` directly
  x <- x[grep("begin\\{tabular\\}", x):grep("end\\{tabular\\}", x)]
  stopifnot(grepl("toprule", x[2]), grepl("bottomrule", x[length(x) - 1]))
  # R2 rows in percent, as in the manuscript
  r2 <- grepl("^\\s*(Adj-)?R\\$\\^2\\$\\s*&", x)
  x[r2] <- vapply(x[r2], function(l) {
    parts <- strsplit(sub("\\\\+\\s*$", "", l), "&")[[1]]
    vals <- suppressWarnings(as.numeric(trimws(parts[-1])))
    parts[-1] <- ifelse(is.na(vals), parts[-1], sprintf(" %.2f\\%% ", 100 * vals))
    paste0(paste(parts, collapse = "&"), "\\\\")
  }, character(1))
  x
}

# manuscript layout: [!htp], caption above with the title in bold and the notes in footnotesize,
# adjustbox; panels (same columns) stacked under one caption with a "Panel A: ..." row
save_panels <- function(panels, titles, file, sample, title, label, notes = NULL, ...) {
  path <- file.path(out_dir(sample), paste0(file, ".tex"))
  K <- length(panels[[1]]) + 1
  stopifnot(all(lengths(panels) == K - 1))
  body <- unlist(lapply(seq_along(panels), function(i) {
    x <- tabular_lines(panels[[i]], depvar = is.null(titles), ...)
    hdr <- if (is.null(titles)) NULL else
      sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel %s: %s}} \\\\", K, LETTERS[i], titles[i])
    c(if (i == 1) x[1:2] else "\\midrule", hdr, x[3:(length(x) - 2)])
  }))
  cap <- sprintf("\\textbf{%s}%s", title, if (is.null(notes)) "" else sprintf(" \\\\ \\footnotesize{%s}", notes))
  L <- c("\\begin{table}[!htp]\\centering", sprintf("\\caption{%s}\\label{%s}", cap, label),
         "\\begin{adjustbox}{max width=\\textwidth}", body, "\\bottomrule", "\\end{tabular}",
         "\\end{adjustbox}", "\\end{table}")
  writeLines(L, path); cat("  ->", sub(paste0(TABLES, "/"), "", path), "\n")
  invisible(path)
}
save_table <- function(models, file, sample, title = NULL, label = NULL, notes = NULL, ...)
  save_panels(list(models), NULL, file, sample, title, label, notes, ...)

stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d, big.mark = ",")

# hand-written booktabs table (matrix of strings) for descriptives
write_tex <- function(rows, header, file, sample, caption, label, notes = NULL, align = NULL, long = FALSE) {
  path <- file.path(out_dir(sample), paste0(file, ".tex"))
  ncol <- length(header)
  align <- if (is.null(align)) paste0("l", strrep("r", ncol - 1)) else align
  head <- paste(paste(header, collapse = " & "), "\\\\ \\midrule")
  note <- if (is.null(notes)) NULL else sprintf("\\multicolumn{%d}{p{0.95\\textwidth}}{\\footnotesize %s} \\\\", ncol, notes)
  if (long) {   # needs \usepackage{longtable}
    L <- c(sprintf("\\begin{longtable}{%s}", align), sprintf("\\caption{%s}\\label{%s} \\\\", caption, label),
           "\\toprule", head, "\\endfirsthead",
           sprintf("\\multicolumn{%d}{l}{\\footnotesize\\textit{(continued)}} \\\\ \\toprule", ncol), head, "\\endhead",
           sprintf("\\midrule \\multicolumn{%d}{r}{\\footnotesize\\textit{continued on next page}} \\\\", ncol), "\\endfoot",
           "\\bottomrule", note, "\\endlastfoot", rows, "\\end{longtable}")
  } else {
    cap <- sprintf("\\textbf{%s}%s", caption, if (is.null(notes)) "" else sprintf(" \\\\ \\footnotesize{%s}", notes))
    L <- c("\\begin{table}[!htp]\\centering", sprintf("\\caption{%s}\\label{%s}", cap, label),
           "\\begin{adjustbox}{max width=\\textwidth}", sprintf("\\begin{tabular}{%s}", align), "\\toprule", head, rows,
           "\\bottomrule", "\\end{tabular}", "\\end{adjustbox}", "\\end{table}")
  }
  writeLines(L, path); cat("  ->", sub(paste0(TABLES, "/"), "", path), "\n")
  invisible(path)
}

# coefficient, SE, t and p of a linear combination c'beta of a fitted model
lincomb <- function(m, w) {
  b <- coef(m)[names(w)]; V <- vcov(m)[names(w), names(w), drop = FALSE]
  est <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w)); t <- est / se
  c(est = est, se = se, t = t, p = 2 * pnorm(-abs(t)))
}
