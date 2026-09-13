# Mechanism: (1) decomposition of the effective spread into realized spread and price
# impact; (2) mediation by the contemporaneous price impact with the Gelbach (2016)
# decomposition of the change in the coefficient; (3) heterogeneity by pre-invasion
# price impact tercile.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
NOTE_MECH <- paste(NOTE_FE, "Neighbor$_{1,2}$ = 1 for the firms headquartered in a first- or second-degree neighbor of Ukraine; War = 1 on and after 24 February 2022.")

for (sample in c(SAMPLES, INTERNAL)) {
  d <- get_sample(D_MAIN, D_BAL, sample)
  d[, pimpact_c := pimpact]
  d[, treat_post := nbr12 * post]
  # (1) decomposition
  m <- list()
  for (tr in c("nbr12", "negdist_z")) for (y in c("espread", "rspread", "pimpact")) m[[length(m) + 1]] <- did(y, sprintf("%s:post", tr), d)
  save_table(m, "mechanism_decomposition", sample, title = "Effective spread, realized spread and price impact",
             label = paste0("tab:mechanism_decomposition_", sample),
             notes = paste("The effective spread is the sum of the realized spread and the five-minute price impact, all three measured from the midpoint prevailing at the trade (half-spreads); because each daily variable is winsorized separately, the estimated coefficients need not add up exactly. $-$Distance (z) is minus the great-circle distance of the headquarters from Ukraine, standardized within the sample (one unit is one standard deviation closer to Ukraine), so a positive coefficient means a larger effect for closer firms.", NOTE_MECH))
  # (2) mediation and Gelbach decomposition
  m <- list(); share <- c()
  for (y in c("qspread", "espread")) {
    m0 <- did(y, "nbr12:post", d); m1 <- did(y, "nbr12:post + pimpact_c", d); aux <- did("pimpact", "nbr12:post", d)
    b0 <- coef(m0)[["nbr12:post"]]; b1 <- coef(m1)[["nbr12:post"]]
    delta <- coef(m1)[["pimpact_c"]] * coef(aux)[["nbr12:post"]]      # Gelbach: (b0 - b1) = gamma * rho
    m <- c(m, list(m0, m1)); share <- c(share, "", sprintf("%.2f", delta / b0))
  }
  save_table(m, "mechanism_mediation", sample, title = "Spreads with and without the contemporaneous price impact",
             label = paste0("tab:mechanism_mediation_", sample),
             extralines = list("Share of the Nearby effect explained by price impact (Gelbach)" = share),
            notes = paste("Columns (1) and (3) repeat the baseline regressions of the quoted and of the effective spread; columns (2) and (4) add the price impact of the same firm on the same day as a regressor. The share reported below the coefficients is the \\citet{gelbach2016} decomposition of the change in the Neighbor$_{1,2}$ $\\times$ War coefficient: the coefficient of price impact in the augmented regression times the effect of Neighbor$_{1,2}$ $\\times$ War on price impact, divided by the baseline coefficient. Price impact is itself affected by the war, so the share is descriptive and not a causal mediation.", NOTE_MECH))
  # (3) heterogeneity by pre-invasion price impact
  m <- lapply(c("qspread", "espread", "ldvol", "ltrades"), function(y) did(y, "i(pi_tercile, treat_post)", d))
  save_table(m, "mechanism_heterogeneity", sample, title = "Neighbor effect by pre-invasion price impact",
             label = paste0("tab:mechanism_heterogeneity_", sample),
             notes = paste("The firms are sorted into terciles of their average price impact over the 20 trading days before the invasion, computed within the sample under analysis; each tercile row reports the coefficient of Neighbor$_{1,2}$ $\\times$ War for the firms of that tercile, Low being the tercile whose trades carried the least information before the war and High the tercile whose trades carried the most.", NOTE_MECH))
}
