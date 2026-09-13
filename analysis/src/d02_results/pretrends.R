# Pre-invasion trends: treatment x linear trend on the pre-period only (firm and day
# fixed effects), and the same by distance bin (strong parallel trends of CGS 2025).
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
OUTCOMES <- c("qspread", "espread", "ldvol", "ltrades")
NOTE_PRE <- "This table tests the parallel-trends assumption of the difference-in-differences design on the pre-invasion days only (27 January - 23 February 2022, the 20 trading days before the invasion). Each column is a separate regression of the outcome named in the header on the interaction of a linear trend with the exposure measure named in the row, $y_{i,d} = \\beta\\,(\\text{Treat}_{i}\\times\\text{Trend}_{d}) + X_{i,d}'\\gamma + \\alpha_i + \\lambda_d + \\varepsilon_{i,d}$, where $i$ and $d$ index firms and local trading days, $\\text{Treat}_{i}$ is the measure named in the row, or the set of measures of the rows when the column reports more than one, and $\\alpha_i$ and $\\lambda_d$ are firm and day fixed effects (row TWFEs). Trend is the number of calendar days since the start of the window, so that $\\beta$ is the difference in the daily change of the outcome, per calendar day, between firms one unit apart in exposure, and is zero under parallel trends. Neighbor$_1$ is an indicator for the firms headquartered in a country bordering Ukraine, Neighbor$_2$ for the firms headquartered in a country bordering one of those, estimated without the first-degree neighbors, and Neighbor$_{1,2}$ for either; $-$Distance (z) is minus the great-circle distance of the headquarters from Ukraine, standardized within the sample; War return is the firm's log return on 24 February 2022; Pre-war volatility (z) is the firm's average daily volatility before the invasion, standardized across firms; the distance bins group the firms by the distance of the headquarters from Ukraine in km. $t$-statistics based on standard errors double-clustered by firm and day in parentheses; $p$-values are denoted as * $p<0.05$, ** $p<0.01$, *** $p<0.001$. Controls: log market value and inverse price."

for (sample in c(SAMPLES, INTERNAL)) {
  d <- get_sample(D_MAIN, D_BAL, sample)[post == 0]
  for (y in OUTCOMES) {
    m <- list()
    for (tr in c("nbr1", "nbr2", "nbr12", "negdist_z", "ret_war", "prevol_z")) {
      dd <- if (tr == "nbr2") d[nbr1 == 0] else d
      m[[length(m) + 1]] <- did(y, sprintf("%s:trend", tr), dd, controls = TRUE)
    }
    save_table(m, paste0("pretrend_", y), sample, title = sprintf("Pre-invasion trends: %s", DICT[[y]]),
               label = paste0("tab:pretrend_", y, "_", sample), notes = NOTE_PRE)
  }
  m <- lapply(OUTCOMES, function(y) did(y, 'i(dist_bin, trend, ref = ">3000")', d, controls = TRUE))
  save_table(m, "pretrend_dose_bins", sample, title = "Pre-invasion trends by distance bin",
             label = paste0("tab:pretrend_bins_", sample),
             notes = paste(NOTE_PRE, "Reference: firms more than 3,000 km from Ukraine."))
}
