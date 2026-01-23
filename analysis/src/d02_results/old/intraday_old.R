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
library(lubridate)
library(data.table)

################################################################################
# --- IMPORT DATA ---
################################################################################

df <- fread("intraday_balanced_psm.csv")
df$date <- as.Date(df$date)
df$datetime <- as.POSIXct(df$datetime, tz = "UTC")
event_time <- as.POSIXct("2022-02-24 03:59:00", tz = "UTC")
event_date <- as.Date("2022-02-24")

df <- df[order(datetime)]

################################################################################
# --- DERIVED VARIABLES ---
################################################################################

df <- df %>%
  mutate(
    # treatment dummy
    postwar = ifelse(datetime >= event_time, 1, 0),
    nearby  = ifelse(psm_group %in% c(1), 1, 0),
    
    # spreads in % units
    qspread_mean      = qspread_mean * 100,
    espread_mean      = espread_mean * 100,
    priceimpact_mean  = priceimpact_mean * 100,
    
    # inverse price
    inv_price  = if_else(price > 0, 1/price, 0),
    inv_price2 = if_else(price_mean > 0, 1/price_mean, 0),
    
    # intraday vol inverse measures
    intra_vol_inv  = if_else(intraday_vol_mean > 0, 1/intraday_vol_mean, 0),
    intra_vol5_inv = if_else(intraday_5m_vol_mean > 0, 1/intraday_5m_vol_mean, 0),
    
    # log transforms
    log_dollar_volume = if_else(dollar_volume_sum > 0, log(dollar_volume_sum + 1), NA_real_),
    log_trades_count  = if_else(trades_count > 0, log(trades_count + 1), NA_real_),
    log_intraday_vol  = if_else(intraday_vol_mean > 0, log(intraday_vol_mean + 1), NA_real_),
    
    log_mktval        = log(mktval),
    log_quotes_count  = if_else(quotes_count > 0, log(quotes_count + 1), NA_real_),
    
    # millions
    dollar_volume_mln = if_else(dollar_volume_sum > 0, dollar_volume_sum/1e6, NA_real_),
    mktval_mln        = if_else(mktval > 0, mktval/1e6, NA_real_),
    trades_mln        = if_else(trades_count > 0, trades_count/1e6, NA_real_),
    quotes_mln        = if_else(quotes_count > 0, quotes_count/1e6, NA_real_)
  )

################################################################################
# --- DEFINE OPENING & CLOSING WINDOWS (FIRST/LAST 60 MIN) ---
################################################################################

DT <- as.data.table(df)
DT[, date_utc := as.Date(datetime)]

# first and last timestamps per ric × date
bounds <- DT[, .(first_dt = min(datetime),
                 last_dt  = max(datetime)),
             by = .(ric, date_utc)]

DT <- merge(DT, bounds, by = c("ric","date_utc"), all.x = TRUE)

# flag open / close hours (first/last 60 min)
DT[, is_open  := datetime >= first_dt & datetime <= first_dt + minutes(60)]
DT[, is_close := datetime >= last_dt  - minutes(60) & datetime <= last_dt]

################################################################################
# --- COMPUTE OPENING & CLOSING AVERAGES (PER ric × date) ---
################################################################################

vars_plain  <- c("qspread_mean", "espread_mean")
vars_log1p  <- c("dollar_volume_sum", "trades_count", "intraday_vol_mean")

all_vars <- c(vars_plain, vars_log1p)

open_avg_list <- list()
close_avg_list <- list()

for (v in all_vars) {
  
  # Opening hour averages
  open_tmp <- DT[is_open == TRUE,
                 .(mean_val =
                     if (v %in% vars_log1p)
                       log(get(v) + 1) |> mean(na.rm=TRUE)
                   else
                     mean(get(v), na.rm=TRUE)),
                 by = .(ric, date_utc, nearby, treat_ukr, postwar)]
  
  open_tmp[, var := v]
  open_avg_list[[v]] <- open_tmp
  
  # Closing hour averages
  close_tmp <- DT[is_close == TRUE,
                  .(mean_val =
                      if (v %in% vars_log1p)
                        log(get(v) + 1) |> mean(na.rm=TRUE)
                    else
                      mean(get(v), na.rm=TRUE)),
                  by = .(ric, date_utc, nearby, treat_ukr, postwar)]
  
  close_tmp[, var := v]
  close_avg_list[[v]] <- close_tmp
}

# bind them
open_avg  <- rbindlist(open_avg_list)
close_avg <- rbindlist(close_avg_list)

# reshape wide: one row per ric × date_utc
open_avg_wide <- dcast(open_avg,
                       ric + date_utc + nearby + treat_ukr + postwar ~ var,
                       value.var = "mean_val")

close_avg_wide <- dcast(close_avg,
                        ric + date_utc + nearby + treat_ukr + postwar ~ var,
                        value.var = "mean_val")





model_op01 <- plm(qspread_mean ~ postwar, 
                  data = pdata.frame(open_avg_wide, index = c("ric", "date_utc")), model = "within", effect = "individual")
summary(model_op01)
coeftest(model_op01, vcov. = vcovHC(model_op01, type = "HC0"))

model_op02 <- plm(qspread_mean ~ nearby * postwar,
                  data = pdata.frame(open_avg_wide, index = c("ric", "date_utc")), model = "within", effect = "twoway")
summary(model_op02)
coeftest(model_op02, vcov. = vcovDC(model_op02, type = "HC0"))

model_op03 <- plm(espread_mean ~ treat_ukr * postwar,
                  data = pdata.frame(open_avg_wide, index = c("ric", "date_utc")), model = "within", effect = "twoway")
summary(model_op03)
coeftest(model_op03, vcov. = vcovDC(model_op03, type = "HC0"))

###
###

model_cl01 <- plm(qspread_mean ~ postwar, 
                  data = pdata.frame(close_avg_wide, index = c("ric", "date_utc")), model = "within", effect = "individual")
summary(model_cl01)
coeftest(model_cl01, vcov. = vcovHC(model_cl01, type = "HC0"))

model_cl02 <- plm(qspread_mean ~ nearby * postwar,
                  data = pdata.frame(close_avg_wide, index = c("ric", "date_utc")), model = "within", effect = "twoway")
summary(model_cl02)
coeftest(model_cl02, vcov. = vcovDC(model_cl02, type = "HC0"))

model_cl03 <- plm(espread_mean ~ treat_ukr * postwar,
                  data = pdata.frame(close_avg_wide, index = c("ric", "date_utc")), model = "within", effect = "twoway")
summary(model_cl03)
coeftest(model_cl03, vcov. = vcovDC(model_cl03, type = "HC0"))

###



################################################################################
# --- LATEX TABLE: ONE PANEL, 5 VARIABLES × 2 MODELS = 10 TOTAL ---------------
################################################################################

fmt  <- function(x) ifelse(is.na(x), "", formatC(x, format="f", digits=3))
fmt_se <- function(x) ifelse(is.na(x), "", formatC(x, format="f", digits=2))
star <- function(p) ifelse(is.na(p), "",
                           ifelse(p<0.01,"***",
                                  ifelse(p<0.05,"**",
                                         ifelse(p<0.10,"*",""))))
pct <- function(x) ifelse(is.na(x),"",paste0(formatC(100*x,format="f",digits=2),"\\%"))
fmt_num <- function(x) ifelse(is.na(x),"",formatC(x, big.mark=",", digits=0, format="f"))

get_info <- function(model, var){
  ct <- tryCatch(coeftest(model, vcov.=vcovHC(model,type="HC0")), error=function(e) NULL)
  if (is.null(ct)) return(list(coef=NA,se=NA,p=NA))
  rn <- rownames(ct)
  m <- rn[grepl(var,rn,fixed=TRUE)]
  if (length(m)==0) m <- rn[grepl(var,rn,ignore.case=TRUE)]
  if (length(m)==0) return(list(coef=NA,se=NA,p=NA))
  list(coef=ct[m[1],1],se=ct[m[1],2],p=ct[m[1],4])
}
get_r2 <- function(m){ s<-summary(m); ifelse(!is.null(s$r.squared),
                                             ifelse("rsq"%in%names(s$r.squared),s$r.squared["rsq"],s$r.squared),NA)}
get_adj <- function(m){ s<-summary(m); ifelse(!is.null(s$r.squared),
                                              ifelse("adjrsq"%in%names(s$r.squared),s$r.squared["adjrsq"],NA),NA)}
get_n <- function(m) nobs(m)

# Panels: 5 dependent variables, 2 specs each
panel <- list(
  list(model01, model02),  # Quoted Spread
  list(model03, model04),  # Effective Spread
  list(model05, model06),  # Dollar Volume
  list(model07, model08),  # Number of Trades
  list(model09, model10)   # Intraday Volatility
)

dep_labels <- c("Quoted Spread (\\%)","Effective Spread (\\%)",
                "Dollar Volume (log)","Number of Trades (log)",
                "Intraday Volatility (log)")

treat_terms <- c("postwar","nbr_1_or_2:postwar")
treat_labels <- c("$War$","$Neighbour_{1-2} \\times War$")

# Begin LaTeX table
sink("main_table_onepanel.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Intraday Unbalanced Matched Sample: Main Specifications. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lccrccrccrccrcc}\n",
    "\\toprule\n",
    " & \\multicolumn{2}{c}{",dep_labels[1],"} & \\multicolumn{2}{c}{",dep_labels[2],"} & ",
    "\\multicolumn{2}{c}{",dep_labels[3],"} & \\multicolumn{2}{c}{",dep_labels[4],"} & ",
    "\\multicolumn{2}{c}{",dep_labels[5],"} \\\\\n",
    "\\cmidrule{2-3}\cmidrule{5-6}\cmidrule{8-9}\cmidrule{11-12}\cmidrule{14-15}\n",
    " & (1) & (2) && (3) & (4) && (5) & (6) && (7) & (8) && (9) & (10) \\\\\n\\midrule\n")

# Loop through variables
for (i in seq_along(treat_terms)){
  cat(treat_labels[i]," & ")
  for (v in panel){
    info <- lapply(v, get_info, treat_terms[i])
    cat(paste(sapply(info,function(x) fmt(x$coef)),collapse=" & ")," & ")
  }
  cat(" \\\\\n & ")
  for (v in panel){
    info <- lapply(v, get_info, treat_terms[i])
    cat(paste(sapply(info,function(x){
      if (is.na(x$se) | is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
    }),collapse=" & ")," & ")
  }
  cat(" \\\\\n")
}

# Summary statistics
r2  <- unlist(lapply(panel, function(v) sapply(v,get_r2)))
adj <- pmax(0,unlist(lapply(panel,function(v)sapply(v,get_adj))))
n   <- unlist(lapply(panel,function(v)sapply(v,get_n)))

cat("\\midrule\n",
    "Firm FEs & ", paste(rep("Yes & Yes",5),collapse=" & "), " \\\\\n",
    "Day FEs & ", paste(rep("No & Yes",5),collapse=" & "), " \\\\\n",
    "$R^{2}$ & ", paste(paste0(pct(r2)),collapse=" & "), " \\\\\n",
    "No. Obs. & ", paste(paste0(fmt_num(n)),collapse=" & "), " \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to main_table_onepanel.tex")





################################################################################
# FULL DATA
################################################################################

model01 <- plm(qspread_mean ~ postwar,
               data = dfp, model = "within", effect = "individual")
summary(model01)
coeftest(model01, vcov. = vcovHC(model01, type = "HC0"))

model02 <- plm(qspread_mean ~ nbr_1_or_2 * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model02)
coeftest(model02, vcov. = vcovDC(model02, type = "HC0"))

model03 <- plm(qspread_mean ~ treat_ukr * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model03)
coeftest(model03, vcov. = vcovDC(model03, type = "HC0"))

###

model04 <- plm(espread_mean ~ postwar,
               data = dfp, model = "within", effect = "individual")
summary(model04)
coeftest(model04, vcov. = vcovHC(model04, type = "HC0"))

model05 <- plm(espread_mean ~ nbr_1_or_2 * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model05)
coeftest(model05, vcov. = vcovDC(model05, type = "HC0"))

model06 <- plm(espread_mean ~ treat_ukr * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model06)
coeftest(model06, vcov. = vcovDC(model06, type = "HC0"))

###

model07 <- plm(log(dollar_volume_sum+1) ~ postwar,
               data = dfp, model = "within", effect = "individual")
summary(model07)
coeftest(model07, vcov. = vcovHC(model07, type = "HC0"))

model08 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model08)
coeftest(model08, vcov. = vcovDC(model08, type = "HC0"))

model09 <- plm(log(dollar_volume_sum+1) ~ treat_ukr * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model09)
coeftest(model09, vcov. = vcovDC(model09, type = "HC0"))

###

model10 <- plm(log(trades_count+1) ~ postwar,
               data = dfp, model = "within", effect = "individual")
summary(model10)
coeftest(model10, vcov. = vcovHC(model10, type = "HC0"))

model11 <- plm(log(trades_count+1) ~ nbr_1_or_2 * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model11)
coeftest(model11, vcov. = vcovDC(model11, type = "HC0"))

model12 <- plm(log(trades_count+1) ~ treat_ukr * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model12)
coeftest(model12, vcov. = vcovDC(model12, type = "HC0"))

###

model13 <- plm(log(intraday_vol_mean+1) ~ postwar,
               data = dfp, model = "within", effect = "individual")
summary(model13)
coeftest(model13, vcov. = vcovHC(model13, type = "HC0"))

model14 <- plm(log(intraday_vol_mean+1) ~ nbr_1_or_2 * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model14)
coeftest(model14, vcov. = vcovDC(model14, type = "HC0"))

model15 <- plm(log(intraday_vol_mean+1) ~ treat_ukr * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model15)
coeftest(model15, vcov. = vcovDC(model15, type = "HC0"))

###

model16 <- plm(log(intraday_5m_vol_mean+1) ~ postwar,
               data = dfp, model = "within", effect = "individual")
summary(model16)
coeftest(model16, vcov. = vcovHC(model16, type = "HC0"))

model17 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_1_or_2 * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model17)
coeftest(model17, vcov. = vcovDC(model17, type = "HC0"))

model18 <- plm(log(intraday_5m_vol_mean+1) ~ treat_ukr * postwar,
               data = dfp, model = "within", effect = "twoway")
summary(model18)
coeftest(model18, vcov. = vcovDC(model18, type = "HC0"))


################################################################################
# --- LATEX TABLE: BASELINE MODELS (3 SPECS × 6 VARS = 18 TOTAL) ---
################################################################################

fmt  <- function(x) ifelse(is.na(x), "", formatC(x, format="f", digits=3))
fmt_se <- function(x) ifelse(is.na(x), "", formatC(x, format="f", digits=2))
star <- function(p) ifelse(is.na(p), "",
                           ifelse(p<0.01,"***",
                                  ifelse(p<0.05,"**",
                                         ifelse(p<0.10,"*",""))))
pct <- function(x) ifelse(is.na(x),"",paste0(formatC(100*x,format="f",digits=2),"\\%"))
fmt_num <- function(x) ifelse(is.na(x),"",formatC(x, big.mark=",", digits=0, format="f"))

get_info <- function(model, var){
  ct <- tryCatch(coeftest(model, vcov.=vcovHC(model,type="HC0")), error=function(e) NULL)
  if (is.null(ct)) return(list(coef=NA,se=NA,p=NA))
  rn <- rownames(ct)
  m <- rn[grepl(var,rn,fixed=TRUE)]
  if (length(m)==0) m <- rn[grepl(var,rn,ignore.case=TRUE)]
  if (length(m)==0) return(list(coef=NA,se=NA,p=NA))
  list(coef=ct[m[1],1],se=ct[m[1],2],p=ct[m[1],4])
}
get_r2 <- function(m){ s<-summary(m); ifelse(!is.null(s$r.squared),
                                             ifelse("rsq"%in%names(s$r.squared),s$r.squared["rsq"],s$r.squared),NA)}
get_adj <- function(m){ s<-summary(m); ifelse(!is.null(s$r.squared),
                                              ifelse("adjrsq"%in%names(s$r.squared),s$r.squared["adjrsq"],NA),NA)}
get_n <- function(m) nobs(m)

# Panels
panelA_left  <- list(model01,model02,model03)
panelA_right <- list(model04,model05,model06)

panelB_left  <- list(model07,model08,model09)
panelB_right <- list(model10,model11,model12)

panelC_left  <- list(model13,model14,model15)
panelC_right <- list(model16,model17,model18)

# (Rename appropriately if using your actual variable names.)

# Variable terms and labels
treat_terms <- c("postwar","nbr_1_or_2:postwar","treat_ukr:postwar")
treat_labels<- c("$War$","$Nearby_{1-2} \\times War$","$Distance \\times War$")

# Begin table
sink("intraday.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Intraday Sample. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lrccccccrcccccc}\n",
    "\\toprule\n",
    "\\multicolumn{15}{l}{\\textit{Panel A: Liquidity Measures}} \\\\ \\toprule\n",
    " & &\\multicolumn{3}{c}{Quoted Spread (\\%)} & & &\\multicolumn{3}{c}{Effective Spread (\\%)} \\\\\n",
    "\\cmidrule(lr){3-5}\\cmidrule(lr){9-11}\n",
    " & &(1)&(2)&(3) & & &(1)&(2)&(3) \\\\\n\\midrule\n")

# Panel A
for (i in seq_along(treat_terms)){
  left <- lapply(panelA_left, get_info, treat_terms[i])
  right<- lapply(panelA_right, get_info, treat_terms[i])
  cat(treat_labels[i]," & &",
      paste(sapply(left,function(x) fmt(x$coef)),collapse=" & "),
      " & & &",
      paste(sapply(right,function(x) fmt(x$coef)),collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " & & &",
      paste(sapply(right,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " \\\\\n")
}
r2L<-sapply(panelA_left,get_r2); adjL<-pmax(0,sapply(panelA_left,get_adj)); nL<-sapply(panelA_left,get_n)
r2R<-sapply(panelA_right,get_r2); adjR<-pmax(0,sapply(panelA_right,get_adj)); nR<-sapply(panelA_right,get_n)
cat("\\midrule\n",
    "Firm FEs & &Yes &Yes &Yes & & &Yes &Yes &Yes \\\\\n",
    "Day FEs & &No &Yes &Yes & & &No &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2L),collapse=" & ")," & & &",paste(pct(r2R),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nL),collapse=" & ")," & & &",paste(fmt_num(nR),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "\\multicolumn{15}{l}{\\textit{Panel B: Trading Activity Measures}} \\\\ \\toprule\n",
    " & &\\multicolumn{3}{c}{Dollar Volume (log)} & & &\\multicolumn{3}{c}{Number of Trades (log)} \\\\\n",
    "\\cmidrule(lr){3-5}\\cmidrule(lr){9-11}\n",
    " & &(1)&(2)&(3) & & &(1)&(2)&(3) \\\\\n\\midrule\n")

# Panel B
for (i in seq_along(treat_terms)){
  left <- lapply(panelB_left, get_info, treat_terms[i])
  right<- lapply(panelB_right, get_info, treat_terms[i])
  cat(treat_labels[i]," & &",
      paste(sapply(left,function(x) fmt(x$coef)),collapse=" & "),
      " & & &",
      paste(sapply(right,function(x) fmt(x$coef)),collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " & & &",
      paste(sapply(right,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " \\\\\n")
}
r2L<-sapply(panelB_left,get_r2); adjL<-pmax(0,sapply(panelB_left,get_adj)); nL<-sapply(panelB_left,get_n)
r2R<-sapply(panelB_right,get_r2); adjR<-pmax(0,sapply(panelB_right,get_adj)); nR<-sapply(panelB_right,get_n)
cat("\\midrule\n",
    "Firm FEs & &Yes &Yes &Yes & & &Yes &Yes &Yes \\\\\n",
    "Day FEs & &No &Yes &Yes & & &No &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2L),collapse=" & ")," & & &",paste(pct(r2R),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nL),collapse=" & ")," & & &",paste(fmt_num(nR),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "\\multicolumn{15}{l}{\\textit{Panel C: Risk Measures}} \\\\ \\toprule\n",
    " & &\\multicolumn{3}{c}{Intraday Volatility (open-to-close)} & & &\\multicolumn{3}{c}{Intraday Volatility (5-minute)} \\\\\n",
    "\\cmidrule(lr){3-5}\\cmidrule(lr){9-11}\n",
    " & &(1)&(2)&(3) & & &(1)&(2)&(3) \\\\\n\\midrule\n")

# Panel C
for (i in seq_along(treat_terms)){
  left <- lapply(panelC_left, get_info, treat_terms[i])
  right<- lapply(panelC_right, get_info, treat_terms[i])
  cat(treat_labels[i]," & &",
      paste(sapply(left,function(x) fmt(x$coef)),collapse=" & "),
      " & & &",
      paste(sapply(right,function(x) fmt(x$coef)),collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(left,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " & & &",
      paste(sapply(right,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " \\\\\n")
}
r2L<-sapply(panelC_left,get_r2); adjL<-pmax(0,sapply(panelC_left,get_adj)); nL<-sapply(panelC_left,get_n)
r2R<-sapply(panelC_right,get_r2); adjR<-pmax(0,sapply(panelC_right,get_adj)); nR<-sapply(panelC_right,get_n)
cat("\\midrule\n",
    "Firm FEs & &Yes &Yes &Yes & & &Yes &Yes &Yes \\\\\n",
    "Day FEs & &No &Yes &Yes & & &No &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2L),collapse=" & ")," & & &",paste(pct(r2R),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nL),collapse=" & ")," & & &",paste(fmt_num(nR),collapse=" & ")," \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to intraday.tex")












