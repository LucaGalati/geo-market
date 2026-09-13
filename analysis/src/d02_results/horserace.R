# Geographic treatment together with the economic treatments: the firm's war-day return
# and its pre-invasion volatility, each interacted with War.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
OUTCOMES <- c("qspread", "espread", "ldvol", "ltrades")
NOTE_HR <- "This table reports estimates of $y_{i,d} = \\beta\\,(\\text{Treat}^{\\text{geo}}_{i} \\times \\text{War}_{d}) + \\delta\\,(\\text{War return}_{i} \\times \\text{War}_{d}) + \\theta\\,(\\text{Pre-war volatility}_{i} \\times \\text{War}_{d}) + X_{i,d}'\\gamma + \\alpha_i + \\lambda_d + \\varepsilon_{i,d}$ (equation~\\eqref{eq:horserace}), which places the geographic exposure of a firm beside its own repricing on the day of the invasion and its riskiness before the war, so that $\\beta$ is the effect of proximity holding the two fixed. $i$ indexes firms and $d$ the local trading days of the window; War = 1 on and after 24 February 2022; $\\alpha_i$ and $\\lambda_d$ are firm and day fixed effects (row TWFEs). The geographic measure is the one named in the first row of each panel: Neighbor$_{1,2}$, an indicator for a headquarters in a first- or second-degree neighbor of Ukraine, or $-$Distance (z), minus the great-circle distance of the headquarters from Ukraine standardized within the sample, for which a positive coefficient means a larger effect for the firms closer to Ukraine. Column (1) keeps the geographic measure alone, column (2) adds the war-day return, column (3) the pre-war volatility and column (4) both. War return is the firm's log return on 24 February 2022 and Pre-war volatility (z) is the firm's average daily volatility over the 20 trading days before the invasion, standardized across firms; both are time-invariant and enter only through their interaction with War, and columns (2) and (4) drop the firms for which no return on 24 February 2022 can be computed, which is why they have fewer observations. Variables winsorized at the 1st and 99th percentiles within the sample. $t$-statistics based on standard errors double-clustered by firm and day in parentheses; $p$-values are denoted as * $p<0.05$, ** $p<0.01$, *** $p<0.001$. Controls: log market value and inverse price."

for (sample in c(SAMPLES, INTERNAL)) {
  d <- get_sample(D_MAIN, D_BAL, sample)
  hr_models <- function(y, geo) list(did(y, sprintf("%s:post", geo), d),
                                     did(y, sprintf("%s:post + ret_war:post", geo), d),
                                     did(y, sprintf("%s:post + prevol_z:post", geo), d),
                                     did(y, sprintf("%s:post + ret_war:post + prevol_z:post", geo), d))
  for (geo in c("nbr12", "negdist_z")) {
    for (pair in list(c("qspread", "espread"), c("ldvol", "ltrades"))) {
      nm <- sprintf("horserace_%s_%s", if (geo == "nbr12") "neighbour" else "distance", if (pair[1] == "qspread") "liquidity" else "activity")
      save_panels(lapply(pair, hr_models, geo = geo), DICT[pair], nm, sample,
                  title = sprintf("Geographic versus economic exposure: %s, %s", if (pair[1] == "qspread") "spreads" else "trading activity", if (geo == "nbr12") "neighbor indicator" else "distance"), label = paste0("tab:", nm, "_", sample), notes = NOTE_HR)
    }
  }
}
