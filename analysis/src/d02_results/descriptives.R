################################################################################
# --- IMPORT LIBRARIES ---
################################################################################

library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(zoo)
library(moments)
library(lfe)
library(car)
library(multiwayvcov)
library(texreg)
library(tibble)


################################################################################
# --- IMPORT DATA ---
################################################################################

source("~/Desktop/geo-market/analysis/R/requirements.R")
library(arrow)
DATA_ROOT <- "~/Desktop/geo-market/analysis/data"
SAMPLE    <- "main"   # "main" (unbalanced) or "balanced" (perfectly balanced)
df <- as.data.frame(read_parquet(file.path(DATA_ROOT, paste0("daily_", SAMPLE, ".parquet"))))
psm <- as.data.frame(read_parquet(file.path(DATA_ROOT, "psm_assignments.parquet"),
                                  col_select = c("ric", "matched_group", "eb_weight")))
df <- merge(df, psm, by = "ric", all.x = TRUE)
df$date <- as.Date(df$date)
df$postwar <- ifelse(df$date >= as.Date("2022-02-24"), 1, 0)
df <- df %>% arrange(date)
event_date <- as.Date("2022-02-24")

# restrict sample dates if needed
dates_unique <- sort(unique(df$date))
event_index <- which(dates_unique == event_date)
selected_dates <- dates_unique[(event_index - 15):(event_index + 14)]

#df <- df %>% filter(date %in% selected_dates)


################################################################################
# --- DERIVED VARIABLES ---
################################################################################

df <- df %>%
  mutate(
    # --- Treatment and group indicators ---
    postwar = if_else(date >= event_date, 1, 0),
    nbr_2 = if_else(nbr_1_or_2 == 0, 0, if_else(nbr_1 == 1, NA_real_, 1)),
    
    # --- Scaling of spreads and price measures ---
    qspread_mean = qspread_mean * 100,
    espread_mean = espread_mean * 100,
    price_impact_mean = price_impact_mean * 100,
    
    # --- Inverse price and intraday volatility measures ---
    inv_price = if_else(price > 0, 1 / price, 0),
    intra_vol_inv = if_else(volatility > 0, 1 / volatility, 0),
    intra_vol5_inv = if_else(log_volatility > 0, 1 / log_volatility, 0),
    
    # --- Log transformations ---
    log_dollar_volume = if_else(dollar_volume_sum > 0, log(dollar_volume_sum + 1), NA_real_),
    log_trades_count = if_else(trades_count > 0, log(trades_count), NA_real_),
    log_mktval = log(mktval),
    log_quotes_count = if_else(quotes_count > 0, log(quotes_count), NA_real_),
    log_volatility = if_else(volatility > 0, log(volatility), NA_real_),
    
    # --- Millions transformations ---
    dollar_volume_mln = if_else(dollar_volume_sum > 0, dollar_volume_sum / 1e6, NA_real_),
    mktval_mln        = if_else(mktval > 0, mktval / 1e6, NA_real_),
    trades_mln        = if_else(trades_count > 0, trades_count / 1e6, NA_real_),
    quotes_mln        = if_else(quotes_count > 0, quotes_count / 1e6, NA_real_),
    
    # --- Treatment intensity interactions (1st neighbours) ---
    treat_dist_intensity       = nbr_1_or_2 * treat_ukr_inv,
    treat_vol_intensity        = nbr_1_or_2 * volatility,
    treat_5m_vol_intensity     = nbr_1_or_2 * log_volatility,
    
    # --- Treatment intensity interactions (2nd neighbours) ---
    treat_dist_intensity1      = nbr_1 * treat_ukr_inv,
    treat_vol_intensity1       = nbr_1 * volatility,
    treat_5m_vol_intensity1    = nbr_1 * log_volatility,
    
    # --- Treatment intensity interactions (1st & 2nd neighbours) ---
    treat_dist_intensity2      = nbr_2 * treat_ukr_inv,
    treat_vol_intensity2       = nbr_2 * volatility,
    treat_5m_vol_intensity2    = nbr_2 * log_volatility
  )

df <- df %>% 
  group_by(ric) %>% 
  mutate( 
    treat_economic = { 
      # firm-specific log return on the event date 
      event_val <- daily_log_return[date == event_date][1]
      if (is.na(event_val)) event_val <- 0
      
      # assign 0 before event, event value for all post-event dates 
      if_else(date < event_date, 0, event_val) 
    } 
  ) %>% 
  ungroup()

df <- df %>%
  group_by(ric) %>%
  mutate(
    treat_economic2 = {
      # firm-specific log return at event date
      event_val <- daily_log_return[date == event_date][1]
      if (is.na(event_val)) event_val <- 0
      
      # assign event_val for ALL days
      event_val
    }
  ) %>%
  ungroup()

# create panel dataset
dfp <- pdata.frame(df, index = c("ric", "date"))
# matched control sample
df2 <- df[df$matched_group %in% c(0, 1) & !is.na(df$matched_group), ]
dfp2 <- pdata.frame(df2, index = c("ric", "date"))



################################################################################
# Descriptive Statistics Table
################################################################################

### Full Sample ###

# --- Define variables and labels ---
vars <- c("qspread_mean", "espread_mean", "log_dollar_volume", "log_trades_count", 
          "log_mktval", "inv_price", "log_quotes_count", "nbr_1", "nbr_2", 
          "nbr_1_or_2", "treat_ukr_inv", "treat_economic",
          "volatility")#, "log_volatility")

labels <- c("Quoted Spread (%)", "Effective Spread (%)", "Dollar Volume (log)",
            "Number of Trades (log)", "Market Value (log)",
            "Price (inverse)", "Number of Quotes (log)",
            "1st Neighbours", "2nd Neighbours", "1st-2nd Neighbours",
            "Distance (inverse)", "War LogReturn",
            "Intraday Volatility (open-to-close)")#,"Intraday Volatility (5-minute)")

# Helper: escape LaTeX special chars
escape_latex <- function(x) {
  x <- gsub("_", "\\\\_", x)
  x <- gsub("%", "\\\\%", x)
  x
}

# Function computing all descriptive statistics
summary_stats_full <- function(data, variables) {
  bind_rows(lapply(variables, function(v) {
    x <- data[[v]]
    tibble(
      Variable = v,
      #Min.    = min(x, na.rm = TRUE),
      Q1      = quantile(x, 0.25, na.rm = TRUE),
      Mean    = mean(x, na.rm = TRUE),
      Median  = median(x, na.rm = TRUE),
      Q3      = quantile(x, 0.75, na.rm = TRUE),
      #Max.    = max(x, na.rm = TRUE),
      Std.    = sd(x, na.rm = TRUE),
      #Skew.   = moments::skewness(x, na.rm = TRUE),
      #Kurt.   = moments::kurtosis(x, na.rm = TRUE)
    )
  }))
}

# Panels
panelA <- summary_stats_full(df, vars) %>% mutate(Panel = "Panel A: Full Sample")
panelB <- summary_stats_full(filter(df, date < event_date), vars) %>% mutate(Panel = "Panel B: Pre-Period")
panelC <- summary_stats_full(filter(df, date >= event_date), vars) %>% mutate(Panel = "Panel C: Post-Period")

# Apply labels
panelA$Variable <- labels
panelB$Variable <- labels
panelC$Variable <- labels

panel_list <- list(panelA, panelB, panelC)

# --- Write LaTeX ---
sink("descriptive_stats.tex")

cat("\\begin{table}[!htp]\n",
    "\\centering\n",
    "\\caption{Descriptive statistics by period for Overall Sample. This table reports summary statistics for all variables across different periods.}\n",
    "\\label{tab:descriptives}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{lcccccc}\n",
    "\\toprule\n",
    "Variables  & Q1 & Mean & Median & Q3 & St. Dev.  \\\\\n",
    "\\cmidrule{1-6}\n",
    sep = "")

for (p in panel_list) {
  panel_name <- unique(p$Panel)
  n_obs <- if (panel_name == "Panel A: Full Sample") nrow(df)
  else if (panel_name == "Panel B: Pre-Period") nrow(filter(df, date < event_date))
  else nrow(filter(df, date >= event_date))
  
  cat("\\textit{", panel_name, " (N = ", format(n_obs, big.mark = ",", scientific = FALSE), " obs.)} \\\\\n",
      "\\cmidrule{1-6}\n", sep = "")
  
  for (i in seq_len(nrow(p))) {
    row <- p[i, ]
    cat(escape_latex(row$Variable), " & ",
        #formatC(row$Min.,    format="f", digits=2, big.mark=","), " & ",
        formatC(row$Q1,      format="f", digits=2, big.mark=","), " & ",
        formatC(row$Mean,    format="f", digits=2, big.mark=","), " & ",
        formatC(row$Median,  format="f", digits=2, big.mark=","), " & ",
        formatC(row$Q3,      format="f", digits=2, big.mark=","), " & ",
        #formatC(row$Max.,    format="f", digits=2, big.mark=","), " & ",
        formatC(row$Std.,    format="f", digits=2, big.mark=","), " & ", " \\\\\n",
        #ifelse(is.na(row$Skew.), "",  formatC(row$Skew.,  format="f", digits=2, big.mark=",")), " & ",
        #ifelse(is.na(row$Kurt.), "", formatC(row$Kurt., format="f", digits=2, big.mark=",")), 
        sep = "")
  }
  cat("\\cmidrule{1-6}\n")
}

cat("\\end{tabular}\n",
    "\\end{adjustbox}\n",
    "\\end{table}\n")
sink()

message("✅ LaTeX descriptive statistics table created: descriptive_stats.tex")




################################################################################
#                      TABLE 2 — Mean Comparison Table                         
################################################################################

# Y variables (as per your specs)
y_vars <- c("qspread_mean", "espread_mean", 
            "log_dollar_volume", "log_trades_count", 
            "volatility")

y_labels <- c("Quoted Spread (\\%)", "Effective Spread (\\%)",
              "Dollar Volume (log)", "Number of Trades (log)",
              "Intraday Volatility")

fmtN <- function(x) format(x, big.mark = ",", scientific = FALSE)

# Significance stars
star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}

# t-test helper for treat vs control (same period)
diff_test <- function(x_treat, x_ctrl) {
  ttest <- try(t.test(x_treat, x_ctrl), silent = TRUE)
  if (inherits(ttest, "try-error")) return(list(diff = NA, p = NA))
  list(
    diff = mean(x_treat, na.rm = TRUE) - mean(x_ctrl, na.rm = TRUE),
    p = ttest$p.value
  )
}

# t-test helper for post vs pre within a group
diff_post_pre <- function(x_post, x_pre) {
  ttest <- try(t.test(x_post, x_pre), silent = TRUE)
  if (inherits(ttest, "try-error")) return(list(diff = NA, p = NA))
  list(
    diff = mean(x_post, na.rm = TRUE) - mean(x_pre, na.rm = TRUE),
    p = ttest$p.value
  )
}

# --------------------------------------------------------------------------
# Panels A, B, C: means + treat-control differences within each period
# --------------------------------------------------------------------------
compute_panel_ABC <- function(data, panel_label) {
  bind_rows(lapply(seq_along(y_vars), function(i) {
    v <- y_vars[i]
    
    # Treat groups
    t1   <- data[[v]][data$nbr_1 == 1]
    t2   <- data[[v]][data$nbr_2 == 1]
    t12  <- data[[v]][data$nbr_1_or_2 == 1]
    
    # Control groups
    c1   <- data[[v]][data$nbr_1 == 0]
    c2   <- data[[v]][data$nbr_2 == 0]           
    c12  <- data[[v]][data$nbr_1_or_2 == 0]
    
    # Differences + stars (treat vs control, same period)
    d1   <- diff_test(t1,  c1)
    d2   <- diff_test(t2,  c2)
    d12  <- diff_test(t12, c12)
    
    tibble(
      Variable = y_labels[i],
      Treat_1  = mean(t1,  na.rm = TRUE),
      Treat_2  = mean(t2,  na.rm = TRUE),
      Treat_12 = mean(t12, na.rm = TRUE),
      Ctrl_1   = mean(c1,  na.rm = TRUE),
      Ctrl_2   = mean(c2,  na.rm = TRUE),
      Ctrl_12  = mean(c12, na.rm = TRUE),
      Diff_1   = paste0(sprintf("%.2f", d1$diff),  star(d1$p)),
      Diff_2   = paste0(sprintf("%.2f", d2$diff),  star(d2$p)),
      Diff_12  = paste0(sprintf("%.2f", d12$diff), star(d12$p)),
      Panel    = panel_label
    )
  }))
}

# Panel A – Full sample
panelA <- compute_panel_ABC(df, "Panel A: Full Sample")

# Panel B – Pre period
panelB <- compute_panel_ABC(filter(df, date < event_date), "Panel B: Pre-Period")

# Panel C – Post period
panelC <- compute_panel_ABC(filter(df, date >= event_date), "Panel C: Post-Period")

# --------------------------------------------------------------------------
# Panel D: post - pre differences AND difference-in-differences (DiD)
# --------------------------------------------------------------------------

panelD <- bind_rows(lapply(seq_along(y_vars), function(i) {
  
  v <- y_vars[i]
  
  pre  <- df[df$date <  event_date, ]
  post <- df[df$date >= event_date, ]
  
  group_vals <- function(D, cond) D[[v]][cond]
  
  # ---- Treat pre/post ----
  t1_pre   <- group_vals(pre,  pre$nbr_1 == 1)
  t1_post  <- group_vals(post, post$nbr_1 == 1)
  t2_pre   <- group_vals(pre,  pre$nbr_2 == 1)
  t2_post  <- group_vals(post, post$nbr_2 == 1)
  t12_pre  <- group_vals(pre,  pre$nbr_1_or_2 == 1)
  t12_post <- group_vals(post, post$nbr_1_or_2 == 1)
  
  # ---- Control pre/post ----
  c1_pre   <- group_vals(pre,  pre$nbr_1 == 0)
  c1_post  <- group_vals(post, post$nbr_1 == 0)
  c2_pre   <- group_vals(pre,  pre$nbr_2 == 0)
  c2_post  <- group_vals(post, post$nbr_2 == 0)
  c12_pre  <- group_vals(pre,  pre$nbr_1_or_2 == 0)
  c12_post <- group_vals(post, post$nbr_1_or_2 == 0)
  
  # ---- Post - Pre diffs ----
  d_t1  <- diff_post_pre(t1_post,  t1_pre)
  d_t2  <- diff_post_pre(t2_post,  t2_pre)
  d_t12 <- diff_post_pre(t12_post, t12_pre)
  d_c1  <- diff_post_pre(c1_post,  c1_pre)
  d_c2  <- diff_post_pre(c2_post,  c2_pre)
  d_c12 <- diff_post_pre(c12_post, c12_pre)
  
  # ---- Difference-in-Differences ----
  did_val_1  <- d_t1$diff  - d_c1$diff
  did_val_2  <- d_t2$diff  - d_c2$diff
  did_val_12 <- d_t12$diff - d_c12$diff
  
  # Compute t-test for DiD using Welch SE
  did_test <- function(d_treat, d_ctrl, t_treat, t_ctrl) {

    treat_t <- try(t.test(t_treat$post, t_treat$pre), silent = TRUE)
    ctrl_t  <- try(t.test(t_ctrl$post,  t_ctrl$pre),  silent = TRUE)
    if (inherits(treat_t, "try-error") || inherits(ctrl_t, "try-error"))
      return(list(p = NA))
    
    se_treat <- (treat_t$stderr)
    se_ctrl  <- (ctrl_t$stderr)
    
    se_did <- sqrt(se_treat^2 + se_ctrl^2)
    
    z <- (d_treat - d_ctrl) / se_did
    p <- 2 * pnorm(-abs(z))
    list(p = p)
  }
  
  p1  <- did_test(d_t1$diff,  d_c1$diff,  list(post=t1_post,  pre=t1_pre),
                  list(post=c1_post,  pre=c1_pre))$p
  p2  <- did_test(d_t2$diff,  d_c2$diff,  list(post=t2_post,  pre=t2_pre),
                  list(post=c2_post,  pre=c2_pre))$p
  p12 <- did_test(d_t12$diff, d_c12$diff, list(post=t12_post, pre=t12_pre),
                  list(post=c12_post, pre=c12_pre))$p
  
  tibble(
    Variable = y_labels[i],
    
    # Post-pre diffs with stars (within groups)
    Treat_1  = paste0(sprintf("%.2f", d_t1$diff),  star(d_t1$p)),
    Treat_2  = paste0(sprintf("%.2f", d_t2$diff),  star(d_t2$p)),
    Treat_12 = paste0(sprintf("%.2f", d_t12$diff), star(d_t12$p)),
    Ctrl_1   = paste0(sprintf("%.2f", d_c1$diff),  star(d_c1$p)),
    Ctrl_2   = paste0(sprintf("%.2f", d_c2$diff),  star(d_c2$p)),
    Ctrl_12  = paste0(sprintf("%.2f", d_c12$diff), star(d_c12$p)),
    
    # DiD terms with stars 
    Diff_1   = paste0(sprintf("%.2f", did_val_1),  star(p1)),
    Diff_2   = paste0(sprintf("%.2f", did_val_2),  star(p2)),
    Diff_12  = paste0(sprintf("%.2f", did_val_12), star(p12)),
    
    Panel    = "Panel D: Post $-$ Pre Difference"
  )
}))

# --------------------------------------------------------------------------
# Combine panels
# --------------------------------------------------------------------------
panel_list <- list(panelA, panelB, panelC, panelD)

count_N <- function(data) {
  list(
    N_T1  = sum(data$nbr_1 == 1,          na.rm = TRUE),
    N_T2  = sum(data$nbr_2 == 1,          na.rm = TRUE),
    N_T12 = sum(data$nbr_1_or_2 == 1,     na.rm = TRUE),
    N_C1  = sum(data$nbr_1 == 0,          na.rm = TRUE),
    N_C2  = sum(data$nbr_2 == 0,          na.rm = TRUE),
    N_C12 = sum(data$nbr_1_or_2 == 0,     na.rm = TRUE)
  )
}

N_panelA <- count_N(df)
N_panelB <- count_N(filter(df, date < event_date))
N_panelC <- count_N(filter(df, date >= event_date))
N_panelD <- count_N(df)  # for DiD panel, full sample counts

N_list <- list(N_panelA, N_panelB, N_panelC, N_panelD)

################################################################################
#                               Write LaTeX Table
################################################################################

sink("mean_comparisons.tex")

cat("\\begin{table}[!htp]\n",
    "\\centering\n",
    "\\caption{Mean comparison between treatment and control groups across periods.}\n",
    "\\label{tab:mean_comparisons}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{lccccccccc}\n",
    "\\toprule\n",
    "Variable & $Nbr_1$ & $Nbr_2$ & $Nbr_{1, 2}$ & C$_1$ & C$_2$ & C$_{1, 2}$ & $Nbr_1 - $ C$_1$ & $Nbr_2 - $ C$_2$ & $Nbr_{1, 2} - $ C$_{1, 2}$ \\\\\n",
    "\\midrule\n")

for (idx in seq_along(panel_list)) {
  
  p <- panel_list[[idx]]
  panel_name <- unique(p$Panel)
  Nvals <- N_list[[idx]]
  
  cat("\\textit{", panel_name, "} \\\\\n",
      "\\cmidrule{1-10}\n", sep = "")
  
  for (i in seq_len(nrow(p))) {
    r <- p[i, ]
    
    # For Panels A–C: treat/control are numeric means, diffs are strings
    # For Panel D: treat/control are strings with stars, diffs are strings too
    val_or_blank <- function(x) {
      if (is.numeric(x)) {
        if (is.na(x)) "" else formatC(x, format = "f", digits = 2, big.mark = ",")
      } else {
        x
      }
    }
    
    cat(r$Variable, " & ",
        val_or_blank(r$Treat_1),  " & ",
        val_or_blank(r$Treat_2),  " & ",
        val_or_blank(r$Treat_12), " & ",
        val_or_blank(r$Ctrl_1),   " & ",
        val_or_blank(r$Ctrl_2),   " & ",
        val_or_blank(r$Ctrl_12),  " & ",
        r$Diff_1, " & ",
        r$Diff_2, " & ",
        r$Diff_12, " \\\\\n", sep = "")
  }
  
  # ---- N row for this panel ----
  cat("\\textit{No. Obs.} & ",
      fmtN(Nvals$N_T1),  " & ",
      fmtN(Nvals$N_T2),  " & ",
      fmtN(Nvals$N_T12), " & ",
      fmtN(Nvals$N_C1),  " & ",
      fmtN(Nvals$N_C2),  " & ",
      fmtN(Nvals$N_C12), " & ",
      fmtN(Nvals$N_T1 + Nvals$N_C1),  " & ",
      fmtN(Nvals$N_T2 + Nvals$N_C2),  " & ",
      fmtN(Nvals$N_T12 + Nvals$N_C12), " \\\\\n")
  
  cat("\\cmidrule{1-10}\n")
}

cat("\\end{tabular}\n",
    "\\end{adjustbox}\n",
    "\\end{table}\n")

sink()

message("✅ Table 2 created successfully: mean_comparisons.tex")



################################################################################
#                                Matched Sample
################################################################################

# --- Define variables and labels ---
vars <- c("qspread_mean", "espread_mean", "log_dollar_volume", "log_trades_count", 
          "log_mktval", "inv_price", "log_quotes_count", "nbr_1", "nbr_2", 
          "nbr_1_or_2", "treat_ukr_inv", "treat_economic",
          "volatility")#, "log_volatility")

labels <- c("Quoted Spread (%)", "Effective Spread (%)", "Dollar Volume (log)",
            "Number of Trades (log)", "Market Value (log)",
            "Price (inverse)", "Number of Quotes (log)",
            "1st Neighbours", "2nd Neighbours", "1st-2nd Neighbours",
            "Distance (inverse)", "War LogReturn",
            "Intraday Volatility (open-to-close)")#,"Intraday Volatility (5-minute)")

# Helper: escape LaTeX special chars
escape_latex <- function(x) {
  x <- gsub("_", "\\\\_", x)
  x <- gsub("%", "\\\\%", x)
  x
}

# Function computing all descriptive statistics
summary_stats_psm_full <- function(data, variables) {
  bind_rows(lapply(variables, function(v) {
    x <- data[[v]]
    tibble(
      Variable = v,
      #Min.    = min(x, na.rm = TRUE),
      Q1      = quantile(x, 0.25, na.rm = TRUE),
      Mean    = mean(x, na.rm = TRUE),
      Median  = median(x, na.rm = TRUE),
      Q3      = quantile(x, 0.75, na.rm = TRUE),
      #Max.    = max(x, na.rm = TRUE),
      Std.    = sd(x, na.rm = TRUE),
      #Skew.   = moments::skewness(x, na.rm = TRUE),
      #Kurt.   = moments::kurtosis(x, na.rm = TRUE)
    )
  }))
}

# Panels
panelA <- summary_stats_psm_full(df2, vars) %>% mutate(Panel = "Panel A: Full Sample")
panelB <- summary_stats_psm_full(filter(df2, date < event_date), vars) %>% mutate(Panel = "Panel B: Pre-Period")
panelC <- summary_stats_psm_full(filter(df2, date >= event_date), vars) %>% mutate(Panel = "Panel C: Post-Period")

# Apply labels
panelA$Variable <- labels
panelB$Variable <- labels
panelC$Variable <- labels

panel_list <- list(panelA, panelB, panelC)

# --- Write LaTeX ---
sink("descriptive_stats_psm.tex")

cat("\\begin{table}[!htp]\n",
    "\\centering\n",
    "\\caption{Descriptive statistics by period for Matched Sample. This table reports summary statistics for all variables across different periods.}\n",
    "\\label{tab:descriptives_psm}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{lcccccc}\n",
    "\\toprule\n",
    "Variables  & Q1 & Mean & Median & Q3 & St. Dev.  \\\\\n",
    "\\cmidrule{1-6}\n",
    sep = "")

for (p in panel_list) {
  panel_name <- unique(p$Panel)
  n_obs <- if (panel_name == "Panel A: Full Sample") nrow(df2)
  else if (panel_name == "Panel B: Pre-Period") nrow(filter(df2, date < event_date))
  else nrow(filter(df2, date >= event_date))
  
  cat("\\textit{", panel_name, " (N = ", format(n_obs, big.mark = ",", scientific = FALSE), " obs.)} \\\\\n",
      "\\cmidrule{1-6}\n", sep = "")
  
  for (i in seq_len(nrow(p))) {
    row <- p[i, ]
    cat(escape_latex(row$Variable), " & ",
        #formatC(row$Min.,    format="f", digits=2, big.mark=","), " & ",
        formatC(row$Q1,      format="f", digits=2, big.mark=","), " & ",
        formatC(row$Mean,    format="f", digits=2, big.mark=","), " & ",
        formatC(row$Median,  format="f", digits=2, big.mark=","), " & ",
        formatC(row$Q3,      format="f", digits=2, big.mark=","), " & ",
        #formatC(row$Max.,    format="f", digits=2, big.mark=","), " & ",
        formatC(row$Std.,    format="f", digits=2, big.mark=","), " & ", " \\\\\n",
        #ifelse(is.na(row$Skew.), "",  formatC(row$Skew.,  format="f", digits=2, big.mark=",")), " & ",
        #ifelse(is.na(row$Kurt.), "", formatC(row$Kurt., format="f", digits=2, big.mark=",")), 
        sep = "")
  }
  cat("\\cmidrule{1-6}\n")
}

cat("\\end{tabular}\n",
    "\\end{adjustbox}\n",
    "\\end{table}\n")
sink()

message("✅ LaTeX descriptive statistics table created: descriptive_stats_psm.tex")




################################################################################
#                      TABLE 2 — Mean Comparison Table                         
################################################################################

# Y variables (as per your specs)
y_vars <- c("qspread_mean", "espread_mean", 
            "log_dollar_volume", "log_trades_count", 
            "volatility")

y_labels <- c("Quoted Spread (\\%)", "Effective Spread (\\%)",
              "Dollar Volume (log)", "Number of Trades (log)",
              "Intraday Volatility")

fmtN <- function(x) format(x, big.mark = ",", scientific = FALSE)

# Significance stars
star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}

# t-test helper for treat vs control (same period)
diff_test <- function(x_treat, x_ctrl) {
  ttest <- try(t.test(x_treat, x_ctrl), silent = TRUE)
  if (inherits(ttest, "try-error")) return(list(diff = NA, p = NA))
  list(
    diff = mean(x_treat, na.rm = TRUE) - mean(x_ctrl, na.rm = TRUE),
    p = ttest$p.value
  )
}

# t-test helper for post vs pre within a group
diff_post_pre <- function(x_post, x_pre) {
  ttest <- try(t.test(x_post, x_pre), silent = TRUE)
  if (inherits(ttest, "try-error")) return(list(diff = NA, p = NA))
  list(
    diff = mean(x_post, na.rm = TRUE) - mean(x_pre, na.rm = TRUE),
    p = ttest$p.value
  )
}

# --------------------------------------------------------------------------
# Panels A, B, C: means + treat-control differences within each period
# --------------------------------------------------------------------------
compute_panel_ABC <- function(data, panel_label) {
  bind_rows(lapply(seq_along(y_vars), function(i) {
    v <- y_vars[i]
    
    # Treat groups
    t1   <- data[[v]][data$nbr_1 == 1]
    t2   <- data[[v]][data$nbr_2 == 1]
    t12  <- data[[v]][data$nbr_1_or_2 == 1]
    
    # Control groups
    c1   <- data[[v]][data$nbr_1 == 0]
    c2   <- data[[v]][data$nbr_2 == 0]           
    c12  <- data[[v]][data$nbr_1_or_2 == 0]
    
    # Differences + stars (treat vs control, same period)
    d1   <- diff_test(t1,  c1)
    d2   <- diff_test(t2,  c2)
    d12  <- diff_test(t12, c12)
    
    tibble(
      Variable = y_labels[i],
      Treat_1  = mean(t1,  na.rm = TRUE),
      Treat_2  = mean(t2,  na.rm = TRUE),
      Treat_12 = mean(t12, na.rm = TRUE),
      Ctrl_1   = mean(c1,  na.rm = TRUE),
      Ctrl_2   = mean(c2,  na.rm = TRUE),
      Ctrl_12  = mean(c12, na.rm = TRUE),
      Diff_1   = paste0(sprintf("%.2f", d1$diff),  star(d1$p)),
      Diff_2   = paste0(sprintf("%.2f", d2$diff),  star(d2$p)),
      Diff_12  = paste0(sprintf("%.2f", d12$diff), star(d12$p)),
      Panel    = panel_label
    )
  }))
}

# Panel A – Full sample
panelA <- compute_panel_ABC(df2, "Panel A: Full Sample")

# Panel B – Pre period
panelB <- compute_panel_ABC(filter(df2, date < event_date), "Panel B: Pre-Period")

# Panel C – Post period
panelC <- compute_panel_ABC(filter(df2, date >= event_date), "Panel C: Post-Period")

# --------------------------------------------------------------------------
# Panel D: post - pre differences AND difference-in-differences (DiD)
# --------------------------------------------------------------------------

panelD <- bind_rows(lapply(seq_along(y_vars), function(i) {
  
  v <- y_vars[i]
  
  pre  <- df2[df2$date <  event_date, ]
  post <- df2[df2$date >= event_date, ]
  
  group_vals <- function(D, cond) D[[v]][cond]
  
  # ---- Treat pre/post ----
  t1_pre   <- group_vals(pre,  pre$nbr_1 == 1)
  t1_post  <- group_vals(post, post$nbr_1 == 1)
  t2_pre   <- group_vals(pre,  pre$nbr_2 == 1)
  t2_post  <- group_vals(post, post$nbr_2 == 1)
  t12_pre  <- group_vals(pre,  pre$nbr_1_or_2 == 1)
  t12_post <- group_vals(post, post$nbr_1_or_2 == 1)
  
  # ---- Control pre/post ----
  c1_pre   <- group_vals(pre,  pre$nbr_1 == 0)
  c1_post  <- group_vals(post, post$nbr_1 == 0)
  c2_pre   <- group_vals(pre,  pre$nbr_2 == 0)
  c2_post  <- group_vals(post, post$nbr_2 == 0)
  c12_pre  <- group_vals(pre,  pre$nbr_1_or_2 == 0)
  c12_post <- group_vals(post, post$nbr_1_or_2 == 0)
  
  # ---- Post - Pre diffs ----
  d_t1  <- diff_post_pre(t1_post,  t1_pre)
  d_t2  <- diff_post_pre(t2_post,  t2_pre)
  d_t12 <- diff_post_pre(t12_post, t12_pre)
  d_c1  <- diff_post_pre(c1_post,  c1_pre)
  d_c2  <- diff_post_pre(c2_post,  c2_pre)
  d_c12 <- diff_post_pre(c12_post, c12_pre)
  
  # ---- Difference-in-Differences ----
  did_val_1  <- d_t1$diff  - d_c1$diff
  did_val_2  <- d_t2$diff  - d_c2$diff
  did_val_12 <- d_t12$diff - d_c12$diff
  
  # Compute t-test for DiD using Welch SE
  did_test <- function(d_treat, d_ctrl, t_treat, t_ctrl) {
    
    treat_t <- try(t.test(t_treat$post, t_treat$pre), silent = TRUE)
    ctrl_t  <- try(t.test(t_ctrl$post,  t_ctrl$pre),  silent = TRUE)
    if (inherits(treat_t, "try-error") || inherits(ctrl_t, "try-error"))
      return(list(p = NA))
    
    se_treat <- (treat_t$stderr)
    se_ctrl  <- (ctrl_t$stderr)
    
    se_did <- sqrt(se_treat^2 + se_ctrl^2)
    
    z <- (d_treat - d_ctrl) / se_did
    p <- 2 * pnorm(-abs(z))
    list(p = p)
  }
  
  p1  <- did_test(d_t1$diff,  d_c1$diff,  list(post=t1_post,  pre=t1_pre),
                  list(post=c1_post,  pre=c1_pre))$p
  p2  <- did_test(d_t2$diff,  d_c2$diff,  list(post=t2_post,  pre=t2_pre),
                  list(post=c2_post,  pre=c2_pre))$p
  p12 <- did_test(d_t12$diff, d_c12$diff, list(post=t12_post, pre=t12_pre),
                  list(post=c12_post, pre=c12_pre))$p
  
  tibble(
    Variable = y_labels[i],
    
    # Post-pre diffs with stars (within groups)
    Treat_1  = paste0(sprintf("%.2f", d_t1$diff),  star(d_t1$p)),
    Treat_2  = paste0(sprintf("%.2f", d_t2$diff),  star(d_t2$p)),
    Treat_12 = paste0(sprintf("%.2f", d_t12$diff), star(d_t12$p)),
    Ctrl_1   = paste0(sprintf("%.2f", d_c1$diff),  star(d_c1$p)),
    Ctrl_2   = paste0(sprintf("%.2f", d_c2$diff),  star(d_c2$p)),
    Ctrl_12  = paste0(sprintf("%.2f", d_c12$diff), star(d_c12$p)),
    
    # DiD terms with stars 
    Diff_1   = paste0(sprintf("%.2f", did_val_1),  star(p1)),
    Diff_2   = paste0(sprintf("%.2f", did_val_2),  star(p2)),
    Diff_12  = paste0(sprintf("%.2f", did_val_12), star(p12)),
    
    Panel    = "Panel D: Post $-$ Pre Difference"
  )
}))

# --------------------------------------------------------------------------
# Combine panels
# --------------------------------------------------------------------------
panel_list <- list(panelA, panelB, panelC, panelD)

count_N <- function(data) {
  list(
    N_T1  = sum(data$nbr_1 == 1,          na.rm = TRUE),
    N_T2  = sum(data$nbr_2 == 1,          na.rm = TRUE),
    N_T12 = sum(data$nbr_1_or_2 == 1,     na.rm = TRUE),
    N_C1  = sum(data$nbr_1 == 0,          na.rm = TRUE),
    N_C2  = sum(data$nbr_2 == 0,          na.rm = TRUE),
    N_C12 = sum(data$nbr_1_or_2 == 0,     na.rm = TRUE)
  )
}

N_panelA <- count_N(df2)
N_panelB <- count_N(filter(df2, date < event_date))
N_panelC <- count_N(filter(df2, date >= event_date))
N_panelD <- count_N(df2)  # for DiD panel, full sample counts

N_list <- list(N_panelA, N_panelB, N_panelC, N_panelD)

################################################################################
#                               Write LaTeX Table
################################################################################

sink("mean_comparisons_psm.tex")

cat("\\begin{table}[!htp]\n",
    "\\centering\n",
    "\\caption{Mean comparison between treatment and control groups across periods.}\n",
    "\\label{tab:mean_comparisons_psm}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{lccccccccc}\n",
    "\\toprule\n",
    "Variable & $Nbr_1$ & $Nbr_2$ & $Nbr_{1, 2}$ & C$_1$ & C$_2$ & C$_{1, 2}$ & $Nbr_1 - $ C$_1$ & $Nbr_2 - $ C$_2$ & $Nbr_{1, 2} - $ C$_{1, 2}$ \\\\\n",
    "\\midrule\n")

for (idx in seq_along(panel_list)) {
  
  p <- panel_list[[idx]]
  panel_name <- unique(p$Panel)
  Nvals <- N_list[[idx]]
  
  cat("\\textit{", panel_name, "} \\\\\n",
      "\\cmidrule{1-10}\n", sep = "")
  
  for (i in seq_len(nrow(p))) {
    r <- p[i, ]
    
    # For Panels A–C: treat/control are numeric means, diffs are strings
    # For Panel D: treat/control are strings with stars, diffs are strings too
    val_or_blank <- function(x) {
      if (is.numeric(x)) {
        if (is.na(x)) "" else formatC(x, format = "f", digits = 2, big.mark = ",")
      } else {
        x
      }
    }
    
    cat(r$Variable, " & ",
        val_or_blank(r$Treat_1),  " & ",
        val_or_blank(r$Treat_2),  " & ",
        val_or_blank(r$Treat_12), " & ",
        val_or_blank(r$Ctrl_1),   " & ",
        val_or_blank(r$Ctrl_2),   " & ",
        val_or_blank(r$Ctrl_12),  " & ",
        r$Diff_1, " & ",
        r$Diff_2, " & ",
        r$Diff_12, " \\\\\n", sep = "")
  }
  
  # ---- N row for this panel ----
  cat("\\textit{No. Obs.} & ",
      fmtN(Nvals$N_T1),  " & ",
      fmtN(Nvals$N_T2),  " & ",
      fmtN(Nvals$N_T12), " & ",
      fmtN(Nvals$N_C1),  " & ",
      fmtN(Nvals$N_C2),  " & ",
      fmtN(Nvals$N_C12), " & ",
      fmtN(Nvals$N_T1 + Nvals$N_C1),  " & ",
      fmtN(Nvals$N_T2 + Nvals$N_C2),  " & ",
      fmtN(Nvals$N_T12 + Nvals$N_C12), " \\\\\n")
  
  cat("\\cmidrule{1-10}\n")
}

cat("\\end{tabular}\n",
    "\\end{adjustbox}\n",
    "\\end{table}\n")

sink()

message("✅ Table 2 created successfully: mean_comparisons_psm.tex")




################################################################################
# Parallel Pre-Trend Tests
################################################################################

# create subset to test parallel trends
df_pre <- df %>% filter(date < event_date)
df_pre <- df_pre %>%
  group_by(ric) %>%
  mutate(trend = as.numeric(date - min(date))) %>%
  ungroup()

df_prep <- pdata.frame(df_pre, index = c("ric", "date"))


### relative quoted spread ###
pretrend_qspread_model01 <- plm(qspread_mean ~ nbr_1 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_qspread_model01)
coeftest(pretrend_qspread_model01, vcov. = vcovDC(pretrend_qspread_model01, type = "HC0"))

pretrend_qspread_model02 <- plm(qspread_mean ~ nbr_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_qspread_model02)
coeftest(pretrend_qspread_model02, vcov. = vcovDC(pretrend_qspread_model02, type = "HC0"))

pretrend_qspread_model03 <- plm(qspread_mean ~ nbr_1_or_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_qspread_model03)
coeftest(pretrend_qspread_model03, vcov. = vcovDC(pretrend_qspread_model03, type = "HC0"))

pretrend_qspread_model04 <- plm(qspread_mean ~ log(treat_ukr_inv) * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_qspread_model04)
coeftest(pretrend_qspread_model04, vcov. = vcovDC(pretrend_qspread_model04, type = "HC0"))

pretrend_qspread_model05 <- plm(qspread_mean ~ volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_qspread_model05)
coeftest(pretrend_qspread_model05, vcov. = vcovDC(pretrend_qspread_model05, type = "HC0"))

pretrend_qspread_model06 <- plm(qspread_mean ~ log_volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_qspread_model06)
coeftest(pretrend_qspread_model06, vcov. = vcovDC(pretrend_qspread_model06, type = "HC0"))

### effective spread ###

pretrend_espread_model01 <- plm(espread_mean ~ nbr_1 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_espread_model01)
coeftest(pretrend_espread_model01, vcov. = vcovDC(pretrend_espread_model01, type = "HC0"))

pretrend_espread_model02 <- plm(espread_mean ~ nbr_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_espread_model02)
coeftest(pretrend_espread_model02, vcov. = vcovDC(pretrend_espread_model02, type = "HC0"))

pretrend_espread_model03 <- plm(espread_mean ~ nbr_1_or_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_espread_model03)
coeftest(pretrend_espread_model03, vcov. = vcovDC(pretrend_espread_model03, type = "HC0"))

pretrend_espread_model04 <- plm(espread_mean ~ log(treat_ukr_inv) * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_espread_model04)
coeftest(pretrend_espread_model04, vcov. = vcovDC(pretrend_espread_model04, type = "HC0"))

pretrend_espread_model05 <- plm(espread_mean ~ volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_espread_model05)
coeftest(pretrend_espread_model05, vcov. = vcovDC(pretrend_espread_model05, type = "HC0"))

pretrend_espread_model06 <- plm(espread_mean ~ log_volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_espread_model06)
coeftest(pretrend_espread_model06, vcov. = vcovDC(pretrend_espread_model06, type = "HC0"))

### dollar volumes ###

pretrend_dvolume_model01 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_dvolume_model01)
coeftest(pretrend_dvolume_model01, vcov. = vcovDC(pretrend_dvolume_model01, type = "HC0"))

pretrend_dvolume_model02 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_dvolume_model02)
coeftest(pretrend_dvolume_model02, vcov. = vcovDC(pretrend_dvolume_model02, type = "HC0"))

pretrend_dvolume_model03 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_dvolume_model03)
coeftest(pretrend_dvolume_model03, vcov. = vcovDC(pretrend_dvolume_model03, type = "HC0"))

pretrend_dvolume_model04 <- plm(log(dollar_volume_sum+1) ~ log(treat_ukr_inv) * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_dvolume_model04)
coeftest(pretrend_dvolume_model04, vcov. = vcovDC(pretrend_dvolume_model04, type = "HC0"))

pretrend_dvolume_model05 <- plm(log(dollar_volume_sum+1) ~ volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_dvolume_model05)
coeftest(pretrend_dvolume_model05, vcov. = vcovDC(pretrend_dvolume_model05, type = "HC0"))

pretrend_dvolume_model06 <- plm(log(dollar_volume_sum+1) ~ log_volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep, model = "within", effect = "twoways")
summary(pretrend_dvolume_model06)
coeftest(pretrend_dvolume_model06, vcov. = vcovDC(pretrend_dvolume_model06, type = "HC0"))

### number of trades ###

pretrend_trades_model01 <- plm(log(trades_count) ~ nbr_1 * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep, model = "within", effect = "twoways")
summary(pretrend_trades_model01)
coeftest(pretrend_trades_model01, vcov. = vcovDC(pretrend_trades_model01, type = "HC0"))

pretrend_trades_model02 <- plm(log(trades_count) ~ nbr_2 * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep, model = "within", effect = "twoways")
summary(pretrend_trades_model02)
coeftest(pretrend_trades_model02, vcov. = vcovDC(pretrend_trades_model02, type = "HC0"))

pretrend_trades_model03 <- plm(log(trades_count) ~ nbr_1_or_2 * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep, model = "within", effect = "twoways")
summary(pretrend_trades_model03)
coeftest(pretrend_trades_model03, vcov. = vcovDC(pretrend_trades_model03, type = "HC0"))

pretrend_trades_model04 <- plm(log(trades_count) ~ log(treat_ukr_inv) * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep, model = "within", effect = "twoways")
summary(pretrend_trades_model04)
coeftest(pretrend_trades_model04, vcov. = vcovDC(pretrend_trades_model04, type = "HC0"))

pretrend_trades_model05 <- plm(log(trades_count) ~ volatility * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep, model = "within", effect = "twoways")
summary(pretrend_trades_model05)
coeftest(pretrend_trades_model05, vcov. = vcovDC(pretrend_trades_model05, type = "HC0"))

pretrend_trades_model06 <- plm(log(trades_count) ~ log_volatility * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep, model = "within", effect = "twoways")
summary(pretrend_trades_model06)
coeftest(pretrend_trades_model06, vcov. = vcovDC(pretrend_trades_model06, type = "HC0"))


# Formatting helpers
fmt <- function(x) ifelse(is.na(x), "", formatC(x, format="f", digits=3))
fmt_se <- function(x) ifelse(is.na(x), "", formatC(x, format="f", digits=2))
star <- function(p) ifelse(is.na(p), "",
                           ifelse(p<0.01,"***",
                                  ifelse(p<0.05,"**",
                                         ifelse(p<0.10,"*",""))))
pct <- function(x) ifelse(is.na(x),"",paste0(formatC(100*x,format="f",digits=2),"\\%"))

# Extract coefficient safely
get_info <- function(model, var){
  ct <- tryCatch(coeftest(model, vcov.=vcovDC(model,type="HC0")), error=function(e) NULL)
  if (is.null(ct)) return(list(coef=NA,se=NA,p=NA))
  rn <- rownames(ct)
  # Match variable or interaction flexibly
  match <- rn[grepl(var, rn, fixed=TRUE)]
  if (length(match)==0) match <- rn[grepl(var, rn, ignore.case=TRUE)]
  if (length(match)==0) return(list(coef=NA,se=NA,p=NA))
  list(coef=ct[match[1],1], se=ct[match[1],2], p=ct[match[1],4])
}

get_r2 <- function(m){
  s <- summary(m)
  if (!is.null(s$r.squared)){
    if ("rsq" %in% names(s$r.squared)) s$r.squared["rsq"]
    else s$r.squared
  } else NA
}
get_adj <- function(m){
  s <- summary(m)
  if (!is.null(s$r.squared)){
    if ("adjrsq" %in% names(s$r.squared)) s$r.squared["adjrsq"]
    else NA
  } else NA
}
get_n <- function(m) nobs(m)

# Model groups
panelA_L <- list(pretrend_qspread_model01,pretrend_qspread_model02,pretrend_qspread_model03,
                 pretrend_qspread_model04,pretrend_qspread_model05,pretrend_qspread_model06)
panelA_R <- list(pretrend_espread_model01,pretrend_espread_model02,pretrend_espread_model03,
                 pretrend_espread_model04,pretrend_espread_model05,pretrend_espread_model06)
panelB_L <- list(pretrend_dvolume_model01,pretrend_dvolume_model02,pretrend_dvolume_model03,
                 pretrend_dvolume_model04,pretrend_dvolume_model05,pretrend_dvolume_model06)
panelB_R <- list(pretrend_trades_model01,pretrend_trades_model02,pretrend_trades_model03,
                 pretrend_trades_model04,pretrend_trades_model05,pretrend_trades_model06)

treat_terms <- c("nbr_1:trend","nbr_2:trend","nbr_1_or_2:trend",
                 "log(treat_ukr_inv):trend","volatility:trend","log_volatility")
treat_labels<- c("$Neighbour_1 \\times Trend$","$Neighbour_2 \\times Trend$","$Neighbour_{1-2} \\times Trend$",
                 "$Distance \\times Trend$","$Volatility_{OC} \\times Trend$","$Volatility_{5m} \\times Trend$")
controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels<-c("Market Value (log)","Price (inverse)","No. Quotes (log)")

# Write LaTeX table
sink("parallel_pretrend.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Overall Sample: Parallel Pre-Trend Tests. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\label{tab:pretrend}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lrccccccrcccccc}\n",
    "\\toprule\n",
    "\\multicolumn{15}{l}{\\textit{Panel A: Liquidity}} \\\\ \\toprule\n",
    " & &\\multicolumn{6}{c}{Quoted Spread (\\%)} & &\\multicolumn{6}{c}{Effective Spread (\\%)} \\\\\n",
    "\\cmidrule(lr){3-8}\\cmidrule(lr){10-15}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6) & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")

# Panel A
for (i in seq_along(treat_terms)){
  left <- lapply(panelA_L, get_info, treat_terms[i])
  right<- lapply(panelA_R, get_info, treat_terms[i])
  cat(treat_labels[i]," & &",
      paste(sapply(left,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " \\\\\n")
}
for (j in seq_along(controls)){
  left <- lapply(panelA_L, get_info, controls[j])
  right<- lapply(panelA_R, get_info, controls[j])
  cat(ctrl_labels[j]," & &",
      paste(sapply(left,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " \\\\\n")
}
r2L<-sapply(panelA_L,get_r2); adjL<-pmax(0, sapply(panelA_L, get_adj), na.rm = FALSE); nL<-sapply(panelA_L,get_n)
r2R<-sapply(panelA_R,get_r2); adjR<-pmax(0, sapply(panelA_R, get_adj), na.rm = FALSE); nR<-sapply(panelA_R,get_n)
cat("\\midrule\n",
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2L),collapse=" & ")," & &",paste(pct(r2R),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjL),collapse=" & ")," & &",paste(pct(adjR),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(formatC(nL, format="f", digits=0, big.mark=","),collapse=" & ")," & &",paste(formatC(nR, format="f", digits=0, big.mark=","),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "\\multicolumn{15}{l}{\\textit{Panel B: Trading Activity}} \\\\ \\toprule\n",
    " & &\\multicolumn{6}{c}{Dollar Volume (log)} & &\\multicolumn{6}{c}{Number of Trades (log)} \\\\\n",
    "\\cmidrule(lr){3-8}\\cmidrule(lr){10-15}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6) & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")

# Panel B
for (i in seq_along(treat_terms)){
  left <- lapply(panelB_L, get_info, treat_terms[i])
  right<- lapply(panelB_R, get_info, treat_terms[i])
  cat(treat_labels[i]," & &",
      paste(sapply(left,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " \\\\\n")
}
for (j in seq_along(controls)){
  left <- lapply(panelB_L, get_info, controls[j])
  right<- lapply(panelB_R, get_info, controls[j])
  cat(ctrl_labels[j]," & &",
      paste(sapply(left,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " \\\\\n")
}
r2L<-sapply(panelB_L,get_r2); adjL<-pmax(0, sapply(panelB_L, get_adj), na.rm = FALSE); nL<-sapply(panelB_L,get_n)
r2R<-sapply(panelB_R,get_r2); adjR<-pmax(0, sapply(panelB_R, get_adj), na.rm = FALSE); nR<-sapply(panelB_R,get_n)
cat("\\midrule\n",
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2L),collapse=" & ")," & &",paste(pct(r2R),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjL),collapse=" & ")," & &",paste(pct(adjR),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(formatC(nL, format="f", digits=0, big.mark=","),collapse=" & ")," & &",paste(formatC(nR, format="f", digits=0, big.mark=","),collapse=" & ")," \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to parallel_pretrend.tex")


################################ Matched Sample ################################

# create subset to test parallel trends
df_pre2 <- df2 %>% filter(date < event_date)
df_pre2 <- df_pre2 %>%
  group_by(ric) %>%
  mutate(trend = as.numeric(date - min(date))) %>%
  ungroup()

df_prep2 <- pdata.frame(df_pre2, index = c("ric", "date"))


### relative quoted spread ###
pretrend_qspread_model01 <- plm(qspread_mean ~ nbr_1 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_qspread_model01)
coeftest(pretrend_qspread_model01, vcov. = vcovDC(pretrend_qspread_model01, type = "HC0"))

pretrend_qspread_model02 <- plm(qspread_mean ~ nbr_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_qspread_model02)
coeftest(pretrend_qspread_model02, vcov. = vcovDC(pretrend_qspread_model02, type = "HC0"))

pretrend_qspread_model03 <- plm(qspread_mean ~ nbr_1_or_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_qspread_model03)
coeftest(pretrend_qspread_model03, vcov. = vcovDC(pretrend_qspread_model03, type = "HC0"))

pretrend_qspread_model04 <- plm(qspread_mean ~ log(treat_ukr_inv) * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_qspread_model04)
coeftest(pretrend_qspread_model04, vcov. = vcovDC(pretrend_qspread_model04, type = "HC0"))

pretrend_qspread_model05 <- plm(qspread_mean ~ volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_qspread_model05)
coeftest(pretrend_qspread_model05, vcov. = vcovDC(pretrend_qspread_model05, type = "HC0"))

pretrend_qspread_model06 <- plm(qspread_mean ~ log_volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_qspread_model06)
coeftest(pretrend_qspread_model06, vcov. = vcovDC(pretrend_qspread_model06, type = "HC0"))

### effective spread ###

pretrend_espread_model01 <- plm(espread_mean ~ nbr_1 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_espread_model01)
coeftest(pretrend_espread_model01, vcov. = vcovDC(pretrend_espread_model01, type = "HC0"))

pretrend_espread_model02 <- plm(espread_mean ~ nbr_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_espread_model02)
coeftest(pretrend_espread_model02, vcov. = vcovDC(pretrend_espread_model02, type = "HC0"))

pretrend_espread_model03 <- plm(espread_mean ~ nbr_1_or_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_espread_model03)
coeftest(pretrend_espread_model03, vcov. = vcovDC(pretrend_espread_model03, type = "HC0"))

pretrend_espread_model04 <- plm(espread_mean ~ log(treat_ukr_inv) * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_espread_model04)
coeftest(pretrend_espread_model04, vcov. = vcovDC(pretrend_espread_model04, type = "HC0"))

pretrend_espread_model05 <- plm(espread_mean ~ volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_espread_model05)
coeftest(pretrend_espread_model05, vcov. = vcovDC(pretrend_espread_model05, type = "HC0"))

pretrend_espread_model06 <- plm(espread_mean ~ log_volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_espread_model06)
coeftest(pretrend_espread_model06, vcov. = vcovDC(pretrend_espread_model06, type = "HC0"))

### dollar volumes ###

pretrend_dvolume_model01 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_dvolume_model01)
coeftest(pretrend_dvolume_model01, vcov. = vcovDC(pretrend_dvolume_model01, type = "HC0"))

pretrend_dvolume_model02 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_dvolume_model02)
coeftest(pretrend_dvolume_model02, vcov. = vcovDC(pretrend_dvolume_model02, type = "HC0"))

pretrend_dvolume_model03 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_dvolume_model03)
coeftest(pretrend_dvolume_model03, vcov. = vcovDC(pretrend_dvolume_model03, type = "HC0"))

pretrend_dvolume_model04 <- plm(log(dollar_volume_sum+1) ~ log(treat_ukr_inv) * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_dvolume_model04)
coeftest(pretrend_dvolume_model04, vcov. = vcovDC(pretrend_dvolume_model04, type = "HC0"))

pretrend_dvolume_model05 <- plm(log(dollar_volume_sum+1) ~ volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_dvolume_model05)
coeftest(pretrend_dvolume_model05, vcov. = vcovDC(pretrend_dvolume_model05, type = "HC0"))

pretrend_dvolume_model06 <- plm(log(dollar_volume_sum+1) ~ log_volatility * trend + log(mktval) + inv_price + log(quotes_count),
                                data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_dvolume_model06)
coeftest(pretrend_dvolume_model06, vcov. = vcovDC(pretrend_dvolume_model06, type = "HC0"))

### number of trades ###

pretrend_trades_model01 <- plm(log(trades_count) ~ nbr_1 * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_trades_model01)
coeftest(pretrend_trades_model01, vcov. = vcovDC(pretrend_trades_model01, type = "HC0"))

pretrend_trades_model02 <- plm(log(trades_count) ~ nbr_2 * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_trades_model02)
coeftest(pretrend_trades_model02, vcov. = vcovDC(pretrend_trades_model02, type = "HC0"))

pretrend_trades_model03 <- plm(log(trades_count) ~ nbr_1_or_2 * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_trades_model03)
coeftest(pretrend_trades_model03, vcov. = vcovDC(pretrend_trades_model03, type = "HC0"))

pretrend_trades_model04 <- plm(log(trades_count) ~ log(treat_ukr_inv) * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_trades_model04)
coeftest(pretrend_trades_model04, vcov. = vcovDC(pretrend_trades_model04, type = "HC0"))

pretrend_trades_model05 <- plm(log(trades_count) ~ volatility * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_trades_model05)
coeftest(pretrend_trades_model05, vcov. = vcovDC(pretrend_trades_model05, type = "HC0"))

pretrend_trades_model06 <- plm(log(trades_count) ~ log_volatility * trend + log(mktval) + inv_price + log(quotes_count),
                               data = df_prep2, model = "within", effect = "twoways")
summary(pretrend_trades_model06)
coeftest(pretrend_trades_model06, vcov. = vcovDC(pretrend_trades_model06, type = "HC0"))


# Model groups
panelA_L2 <- list(pretrend_qspread_model01,pretrend_qspread_model02,pretrend_qspread_model03,
                 pretrend_qspread_model04,pretrend_qspread_model05,pretrend_qspread_model06)
panelA_R2 <- list(pretrend_espread_model01,pretrend_espread_model02,pretrend_espread_model03,
                 pretrend_espread_model04,pretrend_espread_model05,pretrend_espread_model06)
panelB_L2 <- list(pretrend_dvolume_model01,pretrend_dvolume_model02,pretrend_dvolume_model03,
                 pretrend_dvolume_model04,pretrend_dvolume_model05,pretrend_dvolume_model06)
panelB_R2 <- list(pretrend_trades_model01,pretrend_trades_model02,pretrend_trades_model03,
                 pretrend_trades_model04,pretrend_trades_model05,pretrend_trades_model06)

treat_terms <- c("nbr_1:trend","nbr_2:trend","nbr_1_or_2:trend",
                 "log(treat_ukr_inv):trend","volatility:trend","log_volatility")
treat_labels<- c("$Neighbour_1 \\times Trend$","$Neighbour_2 \\times Trend$","$Neighbour_{1-2} \\times Trend$",
                 "$Distance \\times Trend$","$Volatility_{OC} \\times Trend$","$Volatility_{5m} \\times Trend$")
controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels<-c("Market Value (log)","Price (inverse)","No. Quotes (log)")

# Write LaTeX table
sink("parallel_pretrend_psm.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Matched Sample: Parallel Pre-Trend Tests. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\label{tab:pretrend_psm}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lrccccccrcccccc}\n",
    "\\toprule\n",
    "\\multicolumn{15}{l}{\\textit{Panel A: Liquidity}} \\\\ \\toprule\n",
    " & &\\multicolumn{6}{c}{Quoted Spread (\\%)} & &\\multicolumn{6}{c}{Effective Spread (\\%)} \\\\\n",
    "\\cmidrule(lr){3-8}\\cmidrule(lr){10-15}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6) & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")

# Panel A
for (i in seq_along(treat_terms)){
  left <- lapply(panelA_L2, get_info, treat_terms[i])
  right<- lapply(panelA_R2, get_info, treat_terms[i])
  cat(treat_labels[i]," & &",
      paste(sapply(left,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " \\\\\n")
}
for (j in seq_along(controls)){
  left <- lapply(panelA_L2, get_info, controls[j])
  right<- lapply(panelA_R2, get_info, controls[j])
  cat(ctrl_labels[j]," & &",
      paste(sapply(left,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " \\\\\n")
}
r2L<-sapply(panelA_L2,get_r2); adjL<-pmax(0, sapply(panelA_L2, get_adj), na.rm = FALSE); nL<-sapply(panelA_L2,get_n)
r2R<-sapply(panelA_R2,get_r2); adjR<-pmax(0, sapply(panelA_R2, get_adj), na.rm = FALSE); nR<-sapply(panelA_R2,get_n)
cat("\\midrule\n",
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2L),collapse=" & ")," & &",paste(pct(r2R),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjL),collapse=" & ")," & &",paste(pct(adjR),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(formatC(nL, format="f", digits=0, big.mark=","),collapse=" & ")," & &",paste(formatC(nR, format="f", digits=0, big.mark=","),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "\\multicolumn{15}{l}{\\textit{Panel B: Trading Activity}} \\\\ \\toprule\n",
    " & &\\multicolumn{6}{c}{Dollar Volume (log)} & &\\multicolumn{6}{c}{Number of Trades (log)} \\\\\n",
    "\\cmidrule(lr){3-8}\\cmidrule(lr){10-15}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6) & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")

# Panel B
for (i in seq_along(treat_terms)){
  left <- lapply(panelB_L2, get_info, treat_terms[i])
  right<- lapply(panelB_R2, get_info, treat_terms[i])
  cat(treat_labels[i]," & &",
      paste(sapply(left,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " \\\\\n")
}
for (j in seq_along(controls)){
  left <- lapply(panelB_L2, get_info, controls[j])
  right<- lapply(panelB_R2, get_info, controls[j])
  cat(ctrl_labels[j]," & &",
      paste(sapply(left,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " & &",
      paste(sapply(right,function(x) ifelse(is.na(x$se),"",paste0("(",fmt_se(x$se),")",star(x$p)))), collapse=" & "),
      " \\\\\n")
}
r2L<-sapply(panelB_L2,get_r2); adjL<-pmax(0, sapply(panelB_L2, get_adj), na.rm = FALSE); nL<-sapply(panelB_L2,get_n)
r2R<-sapply(panelB_R2,get_r2); adjR<-pmax(0, sapply(panelB_R2, get_adj), na.rm = FALSE); nR<-sapply(panelB_R2,get_n)
cat("\\midrule\n",
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2L),collapse=" & ")," & &",paste(pct(r2R),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjL),collapse=" & ")," & &",paste(pct(adjR),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(formatC(nL, format="f", digits=0, big.mark=","),collapse=" & ")," & &",paste(formatC(nR, format="f", digits=0, big.mark=","),collapse=" & ")," \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to parallel_pretrend_psm.tex")



################################################################################
# --- DESCRIPTIVE TABLES (MARKET, COUNTRY, INDUSTRY) → LATEX -------------------
################################################################################

library(dplyr)
library(readr)
library(xtable)

# Load dataset
df <- read_parquet(file.path(DATA_ROOT, "daily_main.parquet"))

# Keep unique firm-level entries
df_firm <- df %>% distinct(ric, market, ctriso3, indm, .keep_all = TRUE)

# --- 1. Firms by Market
market_tab <- df_firm %>%
  group_by(market) %>%
  summarise(
    Firms = n_distinct(ric),
    Countries = n_distinct(ctriso3),
    Industries = n_distinct(indm)
  ) %>%
  mutate(Share = 100 * Firms / sum(Firms, na.rm = TRUE)) %>%
  arrange(desc(Firms))

# --- 2. Firms by Country
country_tab <- df_firm %>%
  group_by(ctriso3) %>%
  summarise(
    Firms = n_distinct(ric),
    Markets = n_distinct(market),
    Industries = n_distinct(indm)
  ) %>%
  mutate(Share = 100 * Firms / sum(Firms, na.rm = TRUE)) %>%
  arrange(desc(Firms))

# --- 3. Firms by Industry
industry_tab <- df_firm %>%
  group_by(indm) %>%
  summarise(
    Firms = n_distinct(ric),
    Markets = n_distinct(market),
    Countries = n_distinct(ctriso3)
  ) %>%
  mutate(Share = 100 * Firms / sum(Firms, na.rm = TRUE)) %>%
  arrange(desc(Firms))

# --- Function to export as LaTeX table (booktabs style)
export_latex <- function(df, caption, label, file) {
  latex_code <- print(
    xtable(df, caption = caption, label = label, digits = c(0, 0, 0, 0, 0, 2)),
    include.rownames = FALSE,
    caption.placement = "top",
    booktabs = TRUE,
    sanitize.text.function = identity,
    file = file
  )
  message("✅ Saved LaTeX table: ", file)
}

# --- Export all three
export_latex(
  market_tab,
  caption = "Number of firms by market. Table reports the number of unique firms, countries, and industries per market.",
  label = "tab:market_firms",
  file = "table_markets.tex"
)

export_latex(
  country_tab,
  caption = "Number of firms by country. Table reports the number of unique firms, markets, and industries per country.",
  label = "tab:country_firms",
  file = "table_countries.tex"
)

export_latex(
  industry_tab,
  caption = "Number of firms by industry. Table reports the number of unique firms, markets, and countries per industry.",
  label = "tab:industry_firms",
  file = "table_industries.tex"
)


