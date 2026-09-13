# Baseline difference-in-differences: neighbour dummies, negative distance, economic
# treatment, intensity, volatility as outcome. Firm and day fixed effects, SEs clustered
# by firm and day. Headline sample = matched, appendix = main, internal = balanced.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
OUTCOMES <- c("qspread", "espread", "ldvol", "ltrades")

for (sample in c(SAMPLES, INTERNAL)) {
  d <- get_sample(D_MAIN, D_BAL, sample)
  cat(sprintf("[baseline|%s] %s firms, %s firm-days\n", sample, format(uniqueN(d$ric), big.mark = ","), format(nrow(d), big.mark = ",")))
  # neighbour dummies: two-panel tables as in the manuscript (liquidity: quoted / effective spread;
  # activity: dollar volume / trades), 3 treatments x (bare, controls)
  nbr_models <- function(y) unlist(lapply(c("nbr1", "nbr2", "nbr12"), function(tr) {
    dd <- if (tr == "nbr2") d[nbr1 == 0] else d          # second-degree vs non-neighbours only
    list(did(y, sprintf("%s:post", tr), dd, controls = FALSE), did(y, sprintf("%s:post", tr), dd, controls = TRUE))
  }), recursive = FALSE)
  for (pair in list(c("qspread", "espread"), c("ldvol", "ltrades"))) {
    nm <- if (pair[1] == "qspread") "liquidity" else "activity"
    save_panels(lapply(pair, nbr_models), DICT[pair], paste0("did_neighbour_", nm), sample,
                title = sprintf("Difference-in-Differences Regressions for %s: Neighbor", if (nm == "liquidity") "Liquidity Measures" else "Trading Activity"),
                label = paste0("tab:did_nbr_", nm, "_", sample), notes = paste(NOTE_FE, "Neighbor$_1$ equals one for firms headquartered in a country bordering Ukraine (Belarus, Poland, Slovakia, Hungary, Romania, Moldova), Neighbor$_2$ for firms headquartered in a country bordering one of those, and Neighbor$_{1,2}$ for either. Columns (3) and (4) exclude the first-degree firms, so that the second-degree indicator is estimated against firms that are not neighbors of Ukraine at all; this is why the number of observations in those columns is smaller than in the others."))
  }
  # negative distance (linear TWFE summary), all outcomes
  m <- unlist(lapply(OUTCOMES, function(y) list(did(y, "negdist_z:post", d, FALSE), did(y, "negdist_z:post", d, TRUE))), recursive = FALSE)
  save_table(m, "did_distance", sample, title = "Distance to Ukraine (linear two-way fixed effects)",
             label = paste0("tab:did_distance_", sample),
             notes = paste(NOTE_FE, "$-$Distance (z) is minus the great-circle distance of the headquarters from Ukraine, standardized within the sample (one unit is one standard deviation closer to Ukraine), so a positive coefficient means a larger effect for closer firms."))
  # economic treatment: the firm's log return on 24 February 2022
  m <- unlist(lapply(OUTCOMES, function(y) list(did(y, "ret_war:post", d, FALSE), did(y, "ret_war:post", d, TRUE))), recursive = FALSE)
  save_table(m, "did_economic", sample, title = "Economic treatment: war-day return",
             label = paste0("tab:did_economic_", sample),
             notes = paste(NOTE_FE, "War return is the firm's log return on 24 February 2022."))
  # intensity: neighbour dummy plus neighbour x negative distance
  m <- unlist(lapply(OUTCOMES, function(y) list(did(y, "nbr12:post + intensity:post", d, FALSE),
                                                did(y, "nbr12:post + intensity:post", d, TRUE))), recursive = FALSE)
  save_table(m, "did_intensity", sample, title = "Neighbor effect and its intensity in distance",
             label = paste0("tab:did_intensity_", sample),
             notes = paste(NOTE_FE, "$-$Distance (z) is minus the great-circle distance of the headquarters from Ukraine, standardized within the sample (one unit is one standard deviation closer to Ukraine), so a positive coefficient means a larger effect for closer firms. The row Neighbor$_{1,2}$ $\\times$ $-$Distance (z) $\\times$ War is the additional effect of one standard deviation of proximity within the exposed group, and the row Neighbor$_{1,2}$ $\\times$ War the effect for an exposed firm at the average distance of the sample."))
  # volatility as outcome
  m <- list(feols(vol ~ post, d, weights = ~w, cluster = ~ ric + date, fixef = "ric"),
            feols(as.formula(sprintf("vol ~ post + %s | ric", CONTROLS)), d, weights = ~w, cluster = ~ ric + date))
  for (tr in c("nbr1", "nbr2", "nbr12", "negdist_z")) {
    dd <- if (tr == "nbr2") d[nbr1 == 0] else d
    m[[length(m) + 1]] <- did("vol", sprintf("%s:post", tr), dd, FALSE)
    m[[length(m) + 1]] <- did("vol", sprintf("%s:post", tr), dd, TRUE)
  }
  save_table(m, "did_volatility_outcome", sample, title = "Volatility around the invasion",
             label = paste0("tab:did_vol_", sample),
             notes = paste(NOTE_FE, "Volatility, the daily standard deviation of the five-minute midpoint returns in percent, is the dependent variable of every column. Neighbor$_1$, Neighbor$_2$ and Neighbor$_{1,2}$ indicate a firm headquartered in a first-degree, in a second-degree, and in a first- or second-degree neighbor of Ukraine; the Neighbor$_2$ columns leave the first-degree neighbors out of the sample. $-$Distance (z) is minus the great-circle distance of the headquarters from Ukraine, standardized within the sample (one unit is one standard deviation closer to Ukraine), so a positive coefficient means a larger effect for closer firms. Columns (1) and (2) have firm fixed effects only and no day fixed effects, so that the coefficient of War is the change in volatility common to all firms; the remaining columns have both, as the Firm FEs and Day FEs rows report."),
             fixef.group = FALSE)
}
