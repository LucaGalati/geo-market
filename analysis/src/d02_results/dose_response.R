# Dose-response in distance (Callaway, Goodman-Bacon and Sant'Anna, AER 2025): ATT by
# distance bin relative to firms more than 3,000 km away (saturated two-way fixed effects
# regression), average causal responses per 1,000 km between adjacent bins and their
# dose-weighted average, the linear TWFE coefficient for comparison, and B-spline
# estimates with the contdid package on the two-period (pre/post firm means) panel.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
suppressPackageStartupMessages(library(ggplot2))
OUTCOMES <- c("qspread", "espread", "ldvol", "ltrades")
TREATED_BINS <- c("2000-3000", "1500-2000", "1000-1500", "500-1000", "0-500")   # increasing dose (proximity)
DOSE <- 3 - c(2.5, 1.75, 1.25, 0.75, 0.25)                                     # 3,000 km minus the bin midpoint, in 1,000 km
NOTE_DR <- "Dose = proximity to Ukraine, 3,000 km minus the distance of the headquarters (in thousands of km); firms more than 3,000 km away are the untreated reference. ATT(d): coefficient of the bin $\\times$ War interaction in a two-way fixed effects regression with the controls of the baseline tables; ACRT: change in ATT between adjacent bins per 1,000 km of proximity (delta-method standard errors); the dose-weighted average follows Corollary 3.1 of Callaway, Goodman-Bacon and Sant'Anna (2025). Standard errors double-clustered by firm and day."

for (sample in c(SAMPLES, INTERNAL)) {
  d <- get_sample(D_MAIN, D_BAL, sample)
  firms <- d[!duplicated(ric)]
  # ---- ATT by bin ----
  m <- lapply(OUTCOMES, function(y) did(y, 'i(dist_bin, post, ref = ">3000")', d))
  names(m) <- OUTCOMES
  save_table(m, "dose_response_bins", sample, title = "Effects by distance bin", label = paste0("tab:dose_bins_", sample), notes = NOTE_DR)
  # ---- ACRT between adjacent bins and dose-weighted average ----
  rows <- c(); plotdat <- list()
  w_bin <- sapply(TREATED_BINS, function(b) sum(firms$dist_bin == b)); p_treated <- w_bin / sum(w_bin)
  p_ge <- rev(cumsum(rev(p_treated)))                   # P(D >= d_j | D > 0)
  ED <- sum(p_treated * DOSE)                            # E[D | D > 0]
  for (y in OUTCOMES) {
    cn <- sprintf("dist_bin::%s:post", TREATED_BINS)
    att <- coef(m[[y]])[cn]; V <- vcov(m[[y]])[cn, cn]
    att_se <- sqrt(diag(V))
    acrt <- c(); acrt_se <- c()
    for (j in seq_along(cn)) {
      w <- setNames(rep(0, length(cn)), cn); w[j] <- 1 / (DOSE[j] - if (j > 1) DOSE[j - 1] else 0)
      if (j > 1) w[j - 1] <- -w[j]
      lc <- lincomb(m[[y]], w); acrt <- c(acrt, lc["est"]); acrt_se <- c(acrt_se, lc["se"])
    }
    dd <- c(DOSE[1], diff(DOSE))
    w_glob <- dd * p_ge / ED                                  # weights of Corollary 3.1(c)
    wg <- setNames(rep(0, length(cn)), cn)
    for (j in seq_along(cn)) { wj <- w_glob[j] / dd[j]; wg[j] <- wg[j] + wj; if (j > 1) wg[j - 1] <- wg[j - 1] - wj }
    glob <- lincomb(m[[y]], wg)
    att_glob <- lincomb(m[[y]], setNames(p_treated, cn))
    tw <- coeftable(did(y, "negdist:post", d))["negdist:post", ]
    rows <- c(rows, sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", DICT[[y]]))
    for (j in seq_along(cn)) rows <- c(rows, sprintf("%s km & %.2f & %s%s (%s) & %s%s (%s) & %s \\\\", TREATED_BINS[j], DOSE[j],
                                                    fmt(att[j]), stars(2 * pnorm(-abs(att[j] / att_se[j]))), fmt(att_se[j]),
                                                    fmt(acrt[j]), stars(2 * pnorm(-abs(acrt[j] / acrt_se[j]))), fmt(acrt_se[j]),
                                                    format(w_bin[j], big.mark = ",")))
    rows <- c(rows, sprintf("Dose-weighted average & & %s%s (%s) & %s%s (%s) & %s \\\\", fmt(att_glob["est"]), stars(att_glob["p"]), fmt(att_glob["se"]),
                            fmt(glob["est"]), stars(glob["p"]), fmt(glob["se"]), format(sum(w_bin), big.mark = ",")),
              sprintf("Linear TWFE ($-$Distance $\\times$ War, per 1,000 km) & & & %s%s (%s) & \\\\ \\addlinespace", fmt(tw[1]), stars(tw[4]), fmt(tw[2])))
    plotdat[[y]] <- data.frame(outcome = gsub("\\\\", "", DICT[[y]]), bin = factor(TREATED_BINS, levels = TREATED_BINS), dose = DOSE, att = att, se = att_se)
  }
  write_tex(rows, c("Distance bin", "Dose", "ATT(d)", "ACRT(d) per 1,000 km", "Firms"), "dose_response_acrt", sample,
            caption = "Dose-response in distance: average treatment effects and causal responses",
            label = paste0("tab:dose_acrt_", sample), notes = NOTE_DR)
  # ---- figure ----
  pd <- do.call(rbind, plotdat)
  g <- ggplot(pd, aes(x = dose, y = att)) + geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.08) + geom_point(size = 2) + geom_line() +
    facet_wrap(~ outcome, scales = "free_y") + labs(x = "Dose: proximity to Ukraine (3,000 km minus distance, in 1,000 km)", y = "ATT(d) relative to firms beyond 3,000 km") +
    theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank())
  fdir <- file.path(FIGURES, "dose_response", sample); dir.create(fdir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(fdir, "att_by_distance_bin.png"), g, width = 8, height = 6, dpi = 300)
  # ---- contdid (two-period panel of firm means; robustness, no controls) ----
  txt <- c()
  for (y in OUTCOMES) {
    dd <- d[, .(y = mean(get(y), na.rm = TRUE)), by = .(ric, post)]
    dd <- merge(dd, firms[, .(ric, dist_ukr)], by = "ric")
    dd[, `:=`(id = as.integer(factor(ric)), t = post + 1L, G = ifelse(dist_ukr <= 3000, 2L, 0L), D = pmax(0, 3 - dist_ukr / 1000))]
    dd <- dd[is.finite(y)]
    keep <- dd[, .N, by = id][N == 2]$id; dd <- dd[id %in% keep]
    res <- tryCatch({
      cd <- contdid::cont_did(yname = "y", dname = "D", gname = "G", tname = "t", idname = "id", data = as.data.frame(dd),
                              target_parameter = "slope", aggregation = "dose", treatment_type = "continuous",
                              dose_est_method = "parametric", degree = 3, num_knots = 1, control_group = "nevertreated",
                              biters = 200, cband = FALSE)
      paste(capture.output(print(summary(cd))), collapse = "\n")
    }, error = function(e) paste("contdid failed:", conditionMessage(e)))
    txt <- c(txt, sprintf("==== %s (%s) ====", DICT[[y]], sample), res, "")
  }
  writeLines(txt, file.path(out_dir(sample), "dose_response_contdid.txt")); cat("  ->", file.path(out_dir(sample), "dose_response_contdid.txt"), "\n")
}
