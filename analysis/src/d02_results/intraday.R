################################################################################
# --- LIBRARIES ---
################################################################################
library(data.table)
library(lubridate)
library(fixest)
library(broom)
library(dplyr)
library(texreg)

################################################################################
# --- LOAD DATA ---
################################################################################

DATA_ROOT <- "~/Desktop/geo-market/analysis/data"
INTRADAY  <- file.path(DATA_ROOT, "intraday_balanced_psm.csv") 
DT <- fread(INTRADAY)
DT[, datetime := ymd_hms(datetime, tz="UTC")]
DT <- DT[!is.na(datetime)]
setorder(DT, ric, datetime)

# Only valid nearby/distant
DT[, nbr_1_or_2 := as.numeric(nbr_1_or_2)]
DT <- DT[nbr_1_or_2 %in% c(0,1)]
DT[, group := fifelse(nbr_1_or_2 == 1, "Nearby",
                      fifelse(nbr_1_or_2 == 0, "Distant", NA_character_))]
DT <- DT[!is.na(group)]

DT[, date_utc := as.Date(datetime)]

################################################################################
# --- EVENT INDEX ---
################################################################################

event_day <- as.Date("2022-02-24")
days <- sort(unique(DT$date_utc))
day_to_idx <- setNames(seq_along(days), as.character(days))
event_idx <- day_to_idx[as.character(event_day)]
DT[, day_rel := day_to_idx[as.character(date_utc)] - event_idx]

DAYS <- seq(-5,5,1)

################################################################################
# --- OPENING & CLOSING WINDOWS ---
################################################################################

bounds <- DT[, .(
  first_dt = min(datetime),
  last_dt  = max(datetime)
), by = .(ric, date_utc)]

DT <- merge(DT, bounds, by=c("ric","date_utc"))

DT[, is_open_hour  := datetime >= first_dt & datetime <= first_dt + minutes(60)]
DT[, is_close_hour := datetime >= last_dt  - minutes(60) & datetime <= last_dt]

hourly_mean <- function(flag) {
  DT[get(flag)==TRUE,
     .(hour_mean = mean(qspread_mean * 100, na.rm=TRUE)),
     by = .(ric, date_utc, group, day_rel)]
}

open_hr  <- hourly_mean("is_open_hour")
close_hr <- hourly_mean("is_close_hour")

################################################################################
# --- BENCHMARKING (SAME AS PYTHON) ---
################################################################################

BENCH_WIN <- c(-10,-6)
PRE_WIN   <- c(-5,-1)
POST_WIN  <- c(0,5)

compute_bench <- function(hr, win) {
  x <- hr[day_rel %between% win]
  gday <- x[, .(m = mean(hour_mean)), by = .(group, day_rel)]
  g <- gday[, .(mean_bench = mean(m), std_bench = sd(m)), by = group]
  overall <- gday[, .(mean_bench = mean(m), std_bench = sd(m))]
  overall[, group := "Overall"]
  rbind(g, overall)
}

bench_close <- compute_bench(close_hr, BENCH_WIN)
bench_open  <- compute_bench(open_hr,  BENCH_WIN)

standardize <- function(hr, win, bench) {
  z <- hr[day_rel %between% win]
  z <- merge(z, bench, by = "group", all.x = TRUE)
  # fill missing mean/std with overall
  for (col in c("mean_bench", "std_bench")) {
    overall_val <- bench[group == "Overall", get(col)]
    z[is.na(get(col)), (col) := overall_val]
  }
  z[, z := (hour_mean - mean_bench) / std_bench]
  z[, .(ric, group, date_utc, day_rel, z)]
}

z_pre  <- standardize(close_hr, PRE_WIN,  bench_close)
z_post <- standardize(open_hr,  POST_WIN, bench_open)

z_all <- rbind(z_pre, z_post)

################################################################################
# --- WINSORIZATION (same as Python: 1–99%) ---
################################################################################

LOW_PCT <- 1
HIGH_PCT <- 99

lims <- quantile(z_all$z, probs = c(LOW_PCT/100, HIGH_PCT/100), na.rm = TRUE)
z_all[, z := pmin(pmax(z, lims[1]), lims[2])]
z_all <- z_all[is.finite(z)]

################################################################################
# --- DIFFERENCE TEST NEARBY − DISTANT with TWO-WAY CLUSTERED SEs ---
################################################################################

results <- list()

for (d in DAYS) {
  
  sub <- z_all[day_rel == d]
  # basic sanity checks
  sub <- sub[is.finite(z)]
  if (nrow(sub) < 10 ||
      length(unique(sub$group)) < 2 ||
      length(unique(sub$ric))   < 2 ||
      length(unique(sub$date_utc)) < 1) {
    results[[as.character(d)]] <- data.frame(
      day_rel = d, coef = NA_real_, se = NA_real_, p = NA_real_
    )
    next
  }
  
  sub[, nearby := as.integer(group == "Nearby")]
  
  # try/catch to avoid crashes from singular vcov
  res_row <- tryCatch({
    mod <- feols(z ~ nearby, data = sub,
                 cluster = ~ ric + date_utc)
    
    vc <- vcov(mod)
    if (any(!is.finite(vc))) {
      data.frame(day_rel = d, coef = NA_real_, se = NA_real_, p = NA_real_)
    } else {
      co <- coef(mod)["nearby"]
      se <- sqrt(vc["nearby", "nearby"])
      p  <- 2 * pnorm(-abs(co / se))
      data.frame(day_rel = d, coef = co, se = se, p = p)
    }
  }, error = function(e) {
    data.frame(day_rel = d, coef = NA_real_, se = NA_real_, p = NA_real_)
  })
  
  results[[as.character(d)]] <- res_row
}

tab <- bind_rows(results)

################################################################################
# --- LATEX TABLE ---
################################################################################


tab$stars <- ifelse(is.na(tab$p), "",
                    ifelse(tab$p < 0.01, "***",
                           ifelse(tab$p < 0.05, "**",
                                  ifelse(tab$p < 0.10, "*", ""))))

tab$coef_se <- ifelse(
  is.na(tab$coef),
  "NA",
  sprintf("%.4f%s (%.4f)", tab$coef, tab$stars, tab$se)
)

latex_tab <- tab %>%
  dplyr::select(day_rel, coef_se)

print(latex_tab)

# Write LaTeX table
sink("difference_nearby_distant_table.tex")
cat("\\begin{table}[ht]\n\\centering\n")
cat("\\caption{Difference Between Nearby and Distant Firms (Two-way Clustered SEs)}\n")
cat("\\begin{tabular}{cc}\n\\hline\n")
cat("Day Relative & Coefficient (SE) \\\\\n\\hline\n")
for (i in seq_len(nrow(latex_tab))) {
  cat(latex_tab$day_rel[i], " & ", latex_tab$coef_se[i], " \\\\\n")
}
cat("\\hline\n\\end{tabular}\n\\end{table}\n")
sink()

cat("✅ LaTeX table written to difference_nearby_distant_table.tex\n")



