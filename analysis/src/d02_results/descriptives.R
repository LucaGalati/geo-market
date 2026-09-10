# Summary statistics, Nearby vs Distant comparisons (clustered), firms by industry.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
VARS <- c("qspread", "espread", "pimpact", "rspread", "ldvol", "ltrades", "vol", "lmv")

for (sample in c(SAMPLES, INTERNAL)) {
  d <- get_sample(D_MAIN, D_BAL, sample)
  # ---- summary statistics: panels all / pre / post ----
  rows <- c()
  for (pn in list(c("A", "Full window", "all"), c("B", "Before the invasion", "pre"), c("C", "On and after 24 February 2022", "post"))) {
    dd <- switch(pn[3], all = d, pre = d[post == 0], post = d[post == 1])
    rows <- c(rows, sprintf("\\multicolumn{7}{l}{\\textit{Panel %s: %s (%s firm-days, %s firms)}} \\\\", pn[1], pn[2],
                            format(nrow(dd), big.mark = ","), format(uniqueN(dd$ric), big.mark = ",")))
    for (v in VARS) {
      x <- dd[[v]]; x <- x[is.finite(x)]
      rows <- c(rows, sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", DICT[[v]], format(length(x), big.mark = ","),
                              fmt(mean(x)), fmt(sd(x)), fmt(quantile(x, .25)), fmt(median(x)), fmt(quantile(x, .75))))
    }
    rows <- c(rows, "\\addlinespace")
  }
  write_tex(rows, c("", "N", "Mean", "SD", "P25", "Median", "P75"), "descriptive_stats", sample,
            caption = "Summary statistics", label = paste0("tab:descriptives_", sample),
            notes = "Spreads, price impact and realized spread in percent of the midpoint; dollar volume in U.S. dollars and number of trades in logs; volatility is the daily standard deviation of five-minute midpoint returns in percent; market value in logs of U.S. dollars.")
  # ---- Nearby vs Distant: means, differences with clustered SEs, univariate DiD ----
  rows <- c()
  for (v in VARS[1:7]) {
    pre <- d[post == 0]; pst <- d[post == 1]
    m_pre <- feols(as.formula(paste(v, "~ nearby")), pre, weights = ~w, cluster = ~ ric + date)
    m_pst <- feols(as.formula(paste(v, "~ nearby")), pst, weights = ~w, cluster = ~ ric + date)
    m_did <- feols(as.formula(paste(v, "~ nearby * post")), d, weights = ~w, cluster = ~ ric + date)
    ct <- function(m, k) { x <- coeftable(m); c(x[k, 1], x[k, 2], x[k, 4]) }
    a <- ct(m_pre, "nearby"); b <- ct(m_pst, "nearby"); g <- ct(m_did, "nearby:post")
    rows <- c(rows, sprintf("%s & %s & %s & %s%s & %s & %s & %s%s & %s%s \\\\", DICT[[v]],
                            fmt(weighted.mean(pre[nearby == 1][[v]], pre[nearby == 1]$w, na.rm = TRUE)),
                            fmt(weighted.mean(pre[nearby == 0][[v]], pre[nearby == 0]$w, na.rm = TRUE)), fmt(a[1]), stars(a[3]),
                            fmt(weighted.mean(pst[nearby == 1][[v]], pst[nearby == 1]$w, na.rm = TRUE)),
                            fmt(weighted.mean(pst[nearby == 0][[v]], pst[nearby == 0]$w, na.rm = TRUE)), fmt(b[1]), stars(b[3]),
                            fmt(g[1]), stars(g[3])),
              sprintf(" & & & (%s) & & & (%s) & (%s) \\\\", fmt(a[2]), fmt(b[2]), fmt(g[2])))
  }
  write_tex(rows, c("", "Nearby", "Distant", "Difference", "Nearby", "Distant", "Difference", "Diff.-in-diff."),
            "mean_comparisons", sample, caption = "Nearby versus Distant firms before and after the invasion",
            label = paste0("tab:mean_comparisons_", sample),
            notes = "Columns (2)-(4) refer to the days before 24 February 2022, columns (5)-(7) to the days on and after; the last column is the difference-in-differences. Standard errors clustered by firm and day in parentheses. *, **, *** denote significance at 10, 5 and 1 percent.")
  # ---- firms by industry ----
  f <- d[!duplicated(ric), .(firms = .N, nearby = sum(nearby), distant = sum(1 - nearby)), by = .(industry = indm)][order(-firms)]
  f[is.na(industry) | industry == "", industry := "Unclassified"]
  rows <- c(sprintf("%s & %s & %s & %s \\\\", gsub("&", "\\\\&", f$industry), format(f$firms, big.mark = ","),
                    format(f$nearby, big.mark = ","), format(f$distant, big.mark = ",")),
            sprintf("\\midrule Total & %s & %s & %s \\\\", format(sum(f$firms), big.mark = ","), format(sum(f$nearby), big.mark = ","), format(sum(f$distant), big.mark = ",")))
  write_tex(rows, c("Industry", "Firms", "Nearby", "Distant"), "table_industries", sample,
            caption = "Firms by industry", label = paste0("tab:industries_", sample), long = nrow(f) > 30)
}
