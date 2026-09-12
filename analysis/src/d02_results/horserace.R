# Geographic treatment together with the economic treatments: the firm's war-day return
# and its pre-invasion volatility, each interacted with War.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
OUTCOMES <- c("qspread", "espread", "ldvol", "ltrades")
NOTE_HR <- "Variables winsorized at the 1st and 99th percentiles within the sample. Firm and day fixed effects; standard errors double-clustered by firm and day. Controls: log market value and inverse price. War return is the firm's log return on 24 February 2022; pre-war volatility is the firm's average daily volatility before the invasion, standardized across firms."

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
                  title = "Geographic versus economic exposure", label = paste0("tab:", nm, "_", sample), notes = NOTE_HR)
    }
  }
}
