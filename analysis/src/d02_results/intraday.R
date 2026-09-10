# Opening/closing-hour abnormal quoted spreads: Nearby minus Distant by relative day,
# estimated as a regression with standard errors clustered by firm and day (the
# figures use Welch t-tests on the same z-scores). Same design as fig_intraday_core.py.
if (!exists("D_MAIN")) { source(file.path(dirname(sys.frame(1)$ofile), "common.R")); D_MAIN <- load_panel("main"); D_BAL <- load_panel("balanced") }
BENCH <- c(-20, -6); PRE <- c(-5, -1); POST <- c(0, 5); DAYS <- -5:5

DT <- as.data.table(read_parquet(file.path(DATA, "intraday_main.parquet"),
                                 col_select = c("ric", "datetime", "date_local", "nbr_1_or_2", "qspread_mean")))
A <- as.data.table(read_parquet(file.path(DATA, "psm_assignments.parquet"), col_select = c("ric", "matched_group")))
DT <- merge(DT, A, by = "ric", all.x = TRUE)
DT <- DT[!is.na(datetime) & !is.na(qspread_mean)]
days <- sort(unique(DT$date_local)); DT[, day_rel := match(date_local, days) - match(as.character(EVENT), days)]
DT[, `:=`(first_dt = min(datetime), last_dt = max(datetime)), by = .(ric, date_local)]
DT[, `:=`(is_open = datetime < first_dt + 3600, is_close = datetime > last_dt - 3600)]
hour_means <- function(flag) DT[get(flag) == TRUE, .(hour_mean = mean(qspread_mean * 100)), by = .(ric, date_local, day_rel, nbr_1_or_2, matched_group)]
OPEN <- hour_means("is_open"); CLOSE <- hour_means("is_close")

for (sample in SAMPLES) {
  sel <- function(h) { h <- copy(h); if (sample == "matched") h <- h[matched_group %in% c(0, 1)]; h[, nearby := nbr_1_or_2]; h }
  op <- sel(OPEN); cl <- sel(CLOSE)
  bench <- function(h) h[day_rel >= BENCH[1] & day_rel <= BENCH[2], .(mu = mean(hour_mean), sdv = sd(hour_mean)), by = nearby]
  z <- rbind(merge(cl[day_rel >= PRE[1] & day_rel <= PRE[2]], bench(cl), by = "nearby"),
             merge(op[day_rel >= POST[1] & day_rel <= POST[2]], bench(op), by = "nearby"))
  z[, z := (hour_mean - mu) / sdv]
  # one pooled regression with day fixed effects and a Nearby coefficient per day, so that
  # the standard errors can be clustered by firm and day (a per-day regression has one day cluster)
  z <- z[is.finite(z)]
  m <- feols(z ~ i(day_rel, nearby) | day_rel, z, cluster = ~ ric + date_local)
  ct <- coeftable(m)
  rows <- c()
  for (dd in DAYS) {
    s <- z[day_rel == dd]; k <- sprintf("day_rel::%d:nearby", dd)
    rows <- c(rows, sprintf("%+d & %s & %s & %s%s & (%s) & %s \\\\", dd, fmt(mean(s[nearby == 1]$z), 2), fmt(mean(s[nearby == 0]$z), 2),
                            fmt(ct[k, 1], 3), stars(ct[k, 4]), fmt(ct[k, 2], 3), format(nrow(s), big.mark = ",")))
  }
  write_tex(rows, c("Day", "Nearby", "Distant", "Difference", "SE", "Firm-days"), "intraday_difference", sample,
            caption = "Standardized abnormal quoted spread: Nearby minus Distant by day",
            label = paste0("tab:intraday_", sample),
            notes = "Closing-hour means for days $-5$ to $-1$ and opening-hour means for days 0 to $+5$, standardized with the group mean and standard deviation of the firm-day hour means over days $-20$ to $-6$. Difference = coefficient of Nearby $\\times$ day in a regression of the z-scores on day fixed effects and their interactions with the Nearby indicator; standard errors clustered by firm and day.")
}
