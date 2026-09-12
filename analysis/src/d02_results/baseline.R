# Baseline difference-in-differences: neighbour dummies, negative distance, economic
# treatment, intensity, volatility as outcome. Firm and day fixed effects, SEs clustered
# by firm and day. Headline sample = matched, appendix = main, internal = balanced / eb.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
OUTCOMES <- c("qspread", "espread", "ldvol", "ltrades")
NOTE_FE <- "Variables winsorized at the 1st and 99th percentiles within the sample. Firm and day fixed effects; standard errors double-clustered by firm and day in parentheses. Controls: log market value and inverse price. War = 1 on and after 24 February 2022."

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
                label = paste0("tab:did_nbr_", nm, "_", sample), notes = NOTE_FE)
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
  save_table(m, "did_intensity", sample, title = "Neighbour effect and its intensity in distance",
             label = paste0("tab:did_intensity_", sample), notes = NOTE_FE)
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
             notes = paste("Volatility is the daily standard deviation of five-minute midpoint returns, in percent. Columns (1)-(2) have firm fixed effects only;", NOTE_FE),
             fixef.group = FALSE)
}
