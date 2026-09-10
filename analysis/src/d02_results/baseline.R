# Baseline difference-in-differences: neighbour dummies, negative distance, economic
# treatment, intensity, volatility as outcome. Firm and day fixed effects, SEs clustered
# by firm and day. Headline sample = matched, appendix = main, internal = balanced / eb.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
OUTCOMES <- c("qspread", "espread", "ldvol", "ltrades")
NOTE_FE <- "Firm and day fixed effects; standard errors double-clustered by firm and day in parentheses. Controls: log market value, inverse price, log number of quotes. War = 1 on and after 24 February 2022."

for (sample in c(SAMPLES, INTERNAL)) {
  d <- get_sample(D_MAIN, D_BAL, sample)
  cat(sprintf("[baseline|%s] %s firms, %s firm-days\n", sample, format(uniqueN(d$ric), big.mark = ","), format(nrow(d), big.mark = ",")))
  # neighbour dummies: one table per outcome, 3 treatments x (bare, controls)
  for (y in OUTCOMES) {
    m <- list()
    for (tr in c("nbr1", "nbr2", "nbr12")) {
      dd <- if (tr == "nbr2") d[nbr1 == 0] else d          # second-degree vs non-neighbours only
      m[[length(m) + 1]] <- did(y, sprintf("%s:post", tr), dd, controls = FALSE)
      m[[length(m) + 1]] <- did(y, sprintf("%s:post", tr), dd, controls = TRUE)
    }
    save_table(m, paste0("did_neighbour_", y), sample, title = sprintf("Neighbour effects on %s", DICT[[y]]),
               label = paste0("tab:did_nbr_", y, "_", sample), notes = NOTE_FE)
  }
  # negative distance (linear TWFE summary), all outcomes
  m <- unlist(lapply(OUTCOMES, function(y) list(did(y, "negdist:post", d, FALSE), did(y, "negdist:post", d, TRUE))), recursive = FALSE)
  save_table(m, "did_distance", sample, title = "Distance to Ukraine (linear two-way fixed effects)",
             label = paste0("tab:did_distance_", sample),
             notes = paste(NOTE_FE, "$-$Distance is minus the great-circle distance of the headquarters from Ukraine in thousands of km, so a positive coefficient means a larger effect for closer firms."))
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
  for (tr in c("nbr1", "nbr2", "nbr12", "negdist")) {
    dd <- if (tr == "nbr2") d[nbr1 == 0] else d
    m[[length(m) + 1]] <- did("vol", sprintf("%s:post", tr), dd, FALSE)
    m[[length(m) + 1]] <- did("vol", sprintf("%s:post", tr), dd, TRUE)
  }
  save_table(m, "did_volatility_outcome", sample, title = "Volatility around the invasion",
             label = paste0("tab:did_vol_", sample),
             notes = paste("Volatility is the daily standard deviation of five-minute midpoint returns, in percent. Columns (1)-(2) have firm fixed effects only;", NOTE_FE))
}
