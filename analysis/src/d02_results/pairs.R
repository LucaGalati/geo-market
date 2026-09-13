# Matched-pair differences, as recommended by Davies and Kim (2009, JFM) for matched samples in
# microstructure: for every pair, the pre-invasion level and the post-minus-pre change of the Nearby
# firm minus those of its control; Wilcoxon signed-rank test (main) and paired t-test. Matched sample.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- NULL }
VARS <- c("qspread", "espread", "pimpact", "rspread", "ldvol", "ltrades", "vol")
d <- get_sample(D_MAIN, D_BAL, "matched")
pre <- d[post == 0, lapply(.SD, mean, na.rm = TRUE), by = .(ric, nearby, matched_partner), .SDcols = VARS]
pst <- d[post == 1, lapply(.SD, mean, na.rm = TRUE), by = .(ric), .SDcols = VARS]
firm <- merge(pre, pst, by = "ric", suffixes = c("_pre", "_post"))
for (v in VARS) firm[, (paste0(v, "_chg")) := get(paste0(v, "_post")) - get(paste0(v, "_pre"))]
pairs <- merge(firm[nearby == 1], firm[nearby == 0], by.x = "matched_partner", by.y = "ric", suffixes = c("_t", "_c"))
cat(sprintf("[pairs|matched] %s pairs with data before and after the invasion\n", format(nrow(pairs), big.mark = ",")))

rows <- c()
for (pn in list(c("A", "Pre-invasion levels", "pre"), c("B", "Post-minus-pre changes", "chg"))) {
  rows <- c(rows, sprintf("\\multicolumn{8}{l}{\\textit{Panel %s: %s}} \\\\", pn[1], pn[2]))
  for (v in VARS) {
    x <- pairs[[paste0(v, "_", pn[3], "_t")]]; y <- pairs[[paste0(v, "_", pn[3], "_c")]]
    ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
    w <- wilcox.test(x, y, paired = TRUE, exact = FALSE)
    tt <- t.test(x, y, paired = TRUE)
    rows <- c(rows, sprintf("%s & %s & %s & %s & %s%s & %s & %s & %s \\\\", DICT[[v]], format(length(x), big.mark = ","),
                            fmt(mean(x)), fmt(mean(y)), fmt(mean(x - y)), stars(w$p.value), fmt(median(x - y)),
                            fmt(as.numeric(tt$statistic), 2), formatC(w$p.value, format = "f", digits = 3)))
  }
  rows <- c(rows, "\\addlinespace")
}
write_tex(rows, c("", "Pairs", "Nearby", "Control", "Difference", "Median diff.", "$t$ (paired)", "Wilcoxon $p$"),
          "pair_differences", "matched", caption = "Matched-pair differences before and after the invasion",
          label = "tab:pairs_matched",
          notes = "This table compares each Nearby firm with its matched Distant control, as \\citet{davies2009matched} recommend for matched samples. Nearby firms are headquartered in a country bordering Ukraine or in a country bordering one of those and their controls elsewhere, matched one to one on the pre-invasion market capitalization and share price (Section~\\ref{sec:matching}). Panel A reports the firm-level means over the 20 trading days before 24 February 2022 for the Nearby firms, for their controls and for the pair difference; Panel B reports the change from that mean to the mean over the 20 trading days on and after 24 February 2022, and the pair difference of these changes. Spreads, price impact and realized spread are in percent of the midpoint, the effective spread and its two components being half-spreads; dollar volume and the number of trades are in logs; volatility is the daily standard deviation of the five-minute midpoint returns, in percent. Each daily variable is winsorized at the 1st and 99th percentiles within the matched sample before the firm means are taken. The last two columns report the paired $t$-statistic and the $p$-value of the Wilcoxon signed-rank test of the pair differences \\citep{davies2009matched}; stars on the mean difference follow the Wilcoxon test: * $p<0.05$, ** $p<0.01$, *** $p<0.001$. See \\ref{sec:appendixA} for variable definitions.")
