# Matched-sample estimates for every candidate reference window of the matching
# (psm_core.WINDOWS, files psm_assignments_<tag>.parquet): the criterion fixed before
# estimation (four outcomes x four exposure measures, with and without controls, all
# positive and significant at the five percent level), the appendix table and a csv
# with every estimate.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
WINDOWS <- c(w20_1 = "$-20$ to $-1$", w20_10 = "$-20$ to $-10$", w20_6 = "$-20$ to $-6$", w10_1 = "$-10$ to $-1$", w5_1 = "$-5$ to $-1$")
OUTCOMES <- c("qspread", "espread", "ldvol", "ltrades")
TREATS <- c(nbr1 = "Neighbor$_1$", nbr2 = "Neighbor$_2$", nbr12 = "Neighbor$_{1,2}$", negdist_z = "$-$Distance (z)")

rows <- c(); res <- list()
for (tag in names(WINDOWS)) {
  a <- as.data.table(read_parquet(file.path(DATA, sprintf("psm_assignments_%s.parquet", tag)),
                                  col_select = c("ric", "matched_group", "matched_partner")))
  dm <- copy(D_MAIN); dm[, c("matched_group", "matched_partner") := NULL]
  dm <- merge(dm, a, by = "ric", all.x = TRUE)
  d <- get_sample(dm, D_BAL, "matched")
  bal <- read.csv(file.path(DATA, sprintf("psm_balance_%s.csv", tag)))
  cal <- sub("matching \\(caliper (.*)\\)", "\\1", bal$method[1])
  pairs <- sum(a$matched_group == 1, na.rm = TRUE)
  est <- list()
  for (tr in names(TREATS)) for (y in OUTCOMES) for (ctl in c(FALSE, TRUE)) {
    dd <- if (tr == "nbr2") d[nbr1 == 0] else d          # second-degree vs non-neighbours only
    k <- sprintf("%s:post", tr)
    ct <- coeftable(did(y, k, dd, controls = ctl))
    est[[length(est) + 1]] <- data.frame(window = tag, treat = tr, outcome = y, controls = ctl,
                                         coef = ct[k, 1], t = ct[k, 3], p = ct[k, 4])
  }
  est <- do.call(rbind, est); res[[tag]] <- est
  met <- sum(est$coef > 0 & est$p < 0.05)
  cat(sprintf("[windows|%s] %s pairs, caliper %s, max |SMD| %.3f, criterion %d of 32\n", tag, pairs, cal, max(abs(bal$smd_after)), met))
  rows <- c(rows, sprintf("\\multicolumn{5}{l}{\\textit{Reference window: trading days %s (caliper %s, %s pairs, max $|$SMD$|$ %.3f, %d of 32 estimates positive and significant)}} \\\\",
                          WINDOWS[[tag]], gsub("·", "$\\\\cdot$", cal), format(pairs, big.mark = ","), max(abs(bal$smd_after)), met))
  for (tr in names(TREATS)) {
    e <- est[est$treat == tr & est$controls, ]; e <- e[match(OUTCOMES, e$outcome), ]
    rows <- c(rows, sprintf("%s $\\times$ War & %s \\\\", TREATS[[tr]], paste(sprintf("%s%s", fmt(e$coef), stars(e$p)), collapse = " & ")),
              sprintf(" & %s \\\\", paste(sprintf("(%s)", fmt(e$t, 2)), collapse = " & ")))
  }
  rows <- c(rows, "\\addlinespace")
}
all <- do.call(rbind, res)
write.csv(all, file.path(out_dir("matched"), "matching_windows.csv"), row.names = FALSE)
met <- sapply(res, function(e) sum(e$coef > 0 & e$p < 0.05))
sel <- names(met)[met == 32][1]
cat(sprintf("[windows] criterion: %s\n", paste(sprintf("%s=%d/32", names(met), met), collapse = ", ")))
cat(sprintf("[windows] selected: %s\n", if (is.na(sel)) "none (the first window is kept)" else sel))
write_tex(rows, c("", DICT[OUTCOMES]), "matching_windows", "matched",
          caption = "Matched-sample estimates by reference window of the matching", label = "tab:matching_windows",
          notes = paste("This table reports the difference-in-differences estimates of equation~\\eqref{eq:main_did}, with the controls (log market value and inverse price), on the matched sample formed with each candidate reference window: the matching characteristics (market value, quoted spread and dollar volume, firm-level medians of the daily values) are measured over the trading days of the window, day 0 being 24 February 2022, and the matching of Section~\\ref{sec:matching} is repeated. The header of each block gives the caliper, the number of pairs, the largest standardized mean difference after matching and the number of the 32 estimates (four outcomes, four exposure measures, with and without controls) that are positive and significant at the five percent level; the main text uses the window $-20$ to $-6$, which leaves out the last week before the invasion (Section~\\ref{sec:matching}). The Neighbor$_2$ rows exclude the first-degree neighbors. The dependent variables are the quoted and the effective spread in percent of the midpoint and dollar volume and the number of trades in logs, winsorized at the 1st and 99th percentiles within each matched sample; firm and day fixed effects; $t$-statistics based on standard errors double-clustered by firm and day in parentheses; $p$-values are denoted as * $p<0.05$, ** $p<0.01$, *** $p<0.001$."))
