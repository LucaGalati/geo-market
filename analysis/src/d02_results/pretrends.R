# Pre-invasion trends: treatment x linear trend on the pre-period only (firm and day
# fixed effects), and the same by distance bin (strong parallel trends of CGS 2025).
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
OUTCOMES <- c("qspread", "espread", "ldvol", "ltrades")
NOTE_PRE <- "Pre-invasion days only (27 January - 23 February 2022). Trend is the number of calendar days since the start of the window; firm and day fixed effects; standard errors double-clustered by firm and day. Controls: log market value and inverse price."

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
