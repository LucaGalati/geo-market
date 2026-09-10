# Mechanism: (1) decomposition of the effective spread into realized spread and price
# impact; (2) mediation by the contemporaneous price impact with the Gelbach (2016)
# decomposition of the change in the coefficient; (3) heterogeneity by pre-invasion
# price impact tercile.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
NOTE_FE <- "Firm and day fixed effects; standard errors double-clustered by firm and day. Controls: log market value, inverse price, log number of quotes."

for (sample in c(SAMPLES, INTERNAL)) {
  d <- get_sample(D_MAIN, D_BAL, sample)
  d[, pimpact_c := pimpact]
  d[, treat_post := nbr12 * post]
  # (1) decomposition
  m <- list()
  for (tr in c("nbr12", "negdist")) for (y in c("espread", "rspread", "pimpact")) m[[length(m) + 1]] <- did(y, sprintf("%s:post", tr), d)
  save_table(m, "mechanism_decomposition", sample, title = "Effective spread, realized spread and price impact",
             label = paste0("tab:mechanism_decomposition_", sample),
             notes = paste("The effective half-spread is the sum of the realized half-spread and the five-minute price impact.", NOTE_FE))
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
             notes = paste("The share is the Gelbach (2016) decomposition of the change in the Nearby $\\times$ War coefficient when the contemporaneous price impact is added: the coefficient of price impact in the augmented regression times the effect of Nearby $\\times$ War on price impact, divided by the baseline coefficient. Price impact is a post-treatment variable, so the share is descriptive.", NOTE_FE))
  # (3) heterogeneity by pre-invasion price impact
  m <- lapply(c("qspread", "espread", "ldvol", "ltrades"), function(y) did(y, "i(pi_tercile, treat_post)", d))
  save_table(m, "mechanism_heterogeneity", sample, title = "Neighbour effect by pre-invasion price impact",
             label = paste0("tab:mechanism_heterogeneity_", sample),
             notes = paste("Terciles of the firm's average price impact before the invasion within the sample; each coefficient is Neighbor$_{1,2}$ $\\times$ War for the firms of that tercile.", NOTE_FE))
}
