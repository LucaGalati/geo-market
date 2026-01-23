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


################################################################################
# --- IMPORT DATA ---
################################################################################

DATA_ROOT <- "~/Desktop/geo-market/analysis/data"
DAILY  <- file.path(DATA_ROOT, "daily_balanced_psm.csv") 
df <- read.csv(DAILY)
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
    priceimpact_mean = priceimpact_mean * 100,
    
    # --- Inverse price and intraday volatility measures ---
    inv_price = if_else(price > 0, 1 / price, 0),
    inv_price2 = if_else(price_mean > 0, 1 / price_mean, 0),
    intra_vol_inv = if_else(intraday_vol_mean > 0, 1 / intraday_vol_mean, 0),
    intra_vol5_inv = if_else(intraday_5m_vol_mean > 0, 1 / intraday_5m_vol_mean, 0),
    
    # --- Log transformations ---
    log_dollar_volume = if_else(dollar_volume_sum > 0, log(dollar_volume_sum + 1), NA_real_),
    log_trades_count = if_else(trades_count > 0, log(trades_count), NA_real_),
    log_mktval = log(mktval),
    log_quotes_count = if_else(quotes_count > 0, log(quotes_count), NA_real_),
    
    # --- Millions transformations ---
    dollar_volume_mln = if_else(dollar_volume_sum > 0, dollar_volume_sum / 1e6, NA_real_),
    mktval_mln        = if_else(mktval > 0, mktval / 1e6, NA_real_),
    trades_mln        = if_else(trades_count > 0, trades_count / 1e6, NA_real_),
    quotes_mln        = if_else(quotes_count > 0, quotes_count / 1e6, NA_real_),
    
    # --- Treatment intensity interactions (1st neighbours) ---
    treat_dist_intensity       = nbr_1_or_2 * treat_ukr_inv,
    treat_vol_intensity        = nbr_1_or_2 * intraday_vol_mean,
    treat_5m_vol_intensity     = nbr_1_or_2 * intraday_5m_vol_mean,
    
    # --- Treatment intensity interactions (2nd neighbours) ---
    treat_dist_intensity1      = nbr_1 * treat_ukr_inv,
    treat_vol_intensity1       = nbr_1 * intraday_vol_mean,
    treat_5m_vol_intensity1    = nbr_1 * intraday_5m_vol_mean,
    
    # --- Treatment intensity interactions (1st & 2nd neighbours) ---
    treat_dist_intensity2      = nbr_2 * treat_ukr_inv,
    treat_vol_intensity2       = nbr_2 * intraday_vol_mean,
    treat_5m_vol_intensity2    = nbr_2 * intraday_5m_vol_mean
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
df2 <- df[df$psm_group %in% c(0, 1) & !is.na(df$psm_group), ]
dfp2 <- pdata.frame(df2, index = c("ric", "date"))


######################### Create Functions for Tables ##########################

fmt  <- function(x) ifelse(is.na(x), "", formatC(x, format="f", digits=3))
fmt_se <- function(x) ifelse(is.na(x), "", formatC(x, format="f", digits=2))
star <- function(p) ifelse(is.na(p), "",
                           ifelse(p<0.01,"***",
                                  ifelse(p<0.05,"**",
                                         ifelse(p<0.10,"*",""))))
pct <- function(x) ifelse(is.na(x),"",paste0(formatC(100*x,format="f",digits=2),"\\%"))
fmt_num <- function(x) ifelse(is.na(x),"",formatC(x, big.mark=",", digits=0, format="f"))

get_info <- function(model, var){
  ct <- tryCatch(coeftest(model, vcov.=vcovDC(model,type="HC0")), error=function(e) NULL)
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


################################################################################
# Risk Models - Overall Sample
################################################################################


####################### Intraday Volatility (Open-Close) #######################

model_vol01 <- plm(log(intraday_vol_mean+1) ~ postwar, 
                   data = dfp, model = "within", effect = "individual")
summary(model_vol01)
coeftest(model_vol01, vcov. = vcovHC(model_vol01, type = "HC0"))

model_vol02 <- plm(log(intraday_vol_mean+1) ~ postwar + log(mktval) + inv_price  + log(quotes_count), 
                   data = dfp, model = "within", effect = "individual")
summary(model_vol02)
coeftest(model_vol02, vcov. = vcovHC(model_vol02, type = "HC0"))

###

model_vol03 <- plm(log(intraday_vol_mean+1) ~ nbr_1 * postwar, 
                   data = dfp, model = "within", effect = "twoways")
summary(model_vol03)
coeftest(model_vol03, vcov. = vcovDC(model_vol03, type = "HC0"))

model_vol04 <- plm(log(intraday_vol_mean+1) ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                   data = dfp, model = "within", effect = "twoways")
summary(model_vol04)
coeftest(model_vol04, vcov. = vcovDC(model_vol04, type = "HC0"))

###

model_vol05 <- plm(log(intraday_vol_mean+1) ~ nbr_2 * postwar, 
                   data = dfp, model = "within", effect = "twoways")
summary(model_vol05)
coeftest(model_vol05, vcov. = vcovDC(model_vol05, type = "HC0"))

model_vol06 <- plm(log(intraday_vol_mean+1) ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                   data = dfp, model = "within", effect = "twoways")
summary(model_vol06)
coeftest(model_vol06, vcov. = vcovDC(model_vol06, type = "HC0"))

###

model_vol07 <- plm(log(intraday_vol_mean+1) ~ nbr_1_or_2 * postwar, 
                   data = dfp, model = "within", effect = "twoways")
summary(model_vol07)
coeftest(model_vol07, vcov. = vcovDC(model_vol07, type = "HC0"))

model_vol08 <- plm(log(intraday_vol_mean+1) ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                   data = dfp, model = "within", effect = "twoways")
summary(model_vol08)
coeftest(model_vol08, vcov. = vcovDC(model_vol08, type = "HC0"))

###

model_vol09 <- plm(log(intraday_vol_mean+1) ~ log(treat_ukr_inv) * postwar, 
                   data = dfp, model = "within", effect = "twoways")
summary(model_vol09)
coeftest(model_vol09, vcov. = vcovDC(model_vol09, type = "HC0"))

model_vol10 <- plm(log(intraday_vol_mean+1) ~ log(treat_ukr_inv) * postwar + log(mktval) + inv_price + log(quotes_count), 
                   data = dfp, model = "within", effect = "twoways")
summary(model_vol10)
coeftest(model_vol10, vcov. = vcovDC(model_vol10, type = "HC0"))


######################## Intraday Volatility (5-Minute) ########################

model_5m_vol01 <- plm(log(intraday_5m_vol_mean+1) ~ postwar, 
                      data = dfp, model = "within", effect = "individual")
summary(model_5m_vol01)
coeftest(model_5m_vol01, vcov. = vcovHC(model_5m_vol01, type = "HC0"))

model_5m_vol02 <- plm(log(intraday_5m_vol_mean+1) ~ postwar + log(mktval) + inv_price  + log(quotes_count), 
                      data = dfp, model = "within", effect = "individual")
summary(model_5m_vol02)
coeftest(model_5m_vol02, vcov. = vcovHC(model_5m_vol02, type = "HC0"))

###

model_5m_vol03 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_1 * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_5m_vol03)
coeftest(model_5m_vol03, vcov. = vcovDC(model_5m_vol03, type = "HC0"))

model_5m_vol04 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_5m_vol04)
coeftest(model_5m_vol04, vcov. = vcovDC(model_5m_vol04, type = "HC0"))

###

model_5m_vol05 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_2 * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_5m_vol05)
coeftest(model_5m_vol05, vcov. = vcovDC(model_5m_vol05, type = "HC0"))

model_5m_vol06 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_5m_vol06)
coeftest(model_5m_vol06, vcov. = vcovDC(model_5m_vol06, type = "HC0"))

###

model_5m_vol07 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_1_or_2 * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_5m_vol07)
coeftest(model_5m_vol07, vcov. = vcovDC(model_5m_vol07, type = "HC0"))

model_5m_vol08 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_5m_vol08)
coeftest(model_5m_vol08, vcov. = vcovDC(model_5m_vol08, type = "HC0"))

###

model_5m_vol09 <- plm(log(intraday_5m_vol_mean+1) ~ log(treat_ukr_inv) * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_5m_vol09)
coeftest(model_5m_vol09, vcov. = vcovDC(model_5m_vol09, type = "HC0"))

model_5m_vol10 <- plm(log(intraday_5m_vol_mean+1) ~ log(treat_ukr_inv) * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_5m_vol10)
coeftest(model_5m_vol10, vcov. = vcovDC(model_5m_vol10, type = "HC0"))



# Model panels
panelA <- list(model_vol01,model_vol02,model_vol03,model_vol04,model_vol05,
               model_vol06,model_vol07,model_vol08,model_vol09,model_vol10)
panelB <- list(model_5m_vol01,model_5m_vol02,model_5m_vol03,model_5m_vol04,
               model_5m_vol05,model_5m_vol06,model_5m_vol07,model_5m_vol08,
               model_5m_vol09,model_5m_vol10)

# Terms and labels
treat_terms <- c("postwar","nbr_1:postwar","nbr_2:postwar",
                 "nbr_1_or_2:postwar","log(treat_ukr_inv):postwar")
treat_labels<- c("$War$","$Neighbour_1 \\times War$",
                 "$Neighbour_2 \\times War$",
                 "$Neighbour_{1-2} \\times War$",
                 "$Distance \\times War$")
controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels<-c("Market Value (log)","Price (inverse)","No. Quotes (log)")

# Begin table
sink("risk.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Overall Sample: Risk Models. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lrcccccccccc}\n",
    "\\toprule\n",
    "%\\multicolumn{12}{l}{\\textit{Panel A: Overall Sample}} \\\\ \\toprule\n",
    "&&\\multicolumn{10}{c}{Intraday Volatility (open-to-close)} \\\\ \\cmidrule{3-12}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10) \\\\\n\\midrule\n")

# ------------------------ Panel A ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelA, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) {
        if (treat_terms[i]=="postwar" && j>2) "" 
        else if (treat_terms[i]!="postwar" && j<=2) "" 
        else fmt(info[[j]]$coef)
      }), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j) {
        if (treat_terms[i]=="postwar" && j>2) ""
        else if (treat_terms[i]!="postwar" && j<=2) ""
        else if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) "" 
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }), collapse=" & "),
      " \\\\\n")
}

# Controls
for (j in seq_along(controls)){
  info <- lapply(panelA, get_info, controls[j])
  cat(ctrl_labels[j]," & &",
      paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(info,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" 
        else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " \\\\\n")
}

r2A<-sapply(panelA,get_r2); adjA<-pmax(0, sapply(panelA, get_adj), na.rm = FALSE); nA<-sapply(panelA,get_n)
cat("\\midrule\n",
    "Firm FEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "Day FEs & &No &No &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2A),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjA),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nA),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "%\\multicolumn{12}{l}{\\textit{Panel B: Matched Sample}} \\\\ \\toprule\n",
    "&&\\multicolumn{10}{c}{Intraday Volatility (5-minute)} \\\\ \\cmidrule{3-12}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10) \\\\\n\\midrule\n")

# ------------------------ Panel B ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelB, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) {
        if (treat_terms[i]=="postwar" && j>2) "" 
        else if (treat_terms[i]!="postwar" && j<=2) "" 
        else fmt(info[[j]]$coef)
      }), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j) {
        if (treat_terms[i]=="postwar" && j>2) ""
        else if (treat_terms[i]!="postwar" && j<=2) ""
        else if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) "" 
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }), collapse=" & "),
      " \\\\\n")
}

# Controls Panel B
for (j in seq_along(controls)){
  info <- lapply(panelB, get_info, controls[j])
  cat(ctrl_labels[j]," & &",
      paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(info,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" 
        else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " \\\\\n")
}

r2B<-sapply(panelB,get_r2); adjB<-pmax(0, sapply(panelB, get_adj), na.rm = FALSE); nB<-sapply(panelB,get_n)
cat("\\midrule\n",
    "Firm FEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "Day FEs & &No &No &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2B),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjB),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nB),collapse=" & ")," \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to risk_models.tex")



################################################################################
# Risk Models - Matched Sample
################################################################################

####################### Intraday Volatility (Open-Close) #######################

model_vol01 <- plm(log(intraday_vol_mean+1) ~ postwar, 
                   data = dfp2, model = "within", effect = "individual")
summary(model_vol01)
coeftest(model_vol01, vcov. = vcovHC(model_vol01, type = "HC0"))

model_vol02 <- plm(log(intraday_vol_mean+1) ~ postwar + log(mktval) + inv_price  + log(quotes_count), 
                   data = dfp2, model = "within", effect = "individual")
summary(model_vol02)
coeftest(model_vol02, vcov. = vcovHC(model_vol02, type = "HC0"))

###

model_vol03 <- plm(log(intraday_vol_mean+1) ~ nbr_1 * postwar, 
                   data = dfp2, model = "within", effect = "twoways")
summary(model_vol03)
coeftest(model_vol03, vcov. = vcovDC(model_vol03, type = "HC0"))

model_vol04 <- plm(log(intraday_vol_mean+1) ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                   data = dfp2, model = "within", effect = "twoways")
summary(model_vol04)
coeftest(model_vol04, vcov. = vcovDC(model_vol04, type = "HC0"))

###

model_vol05 <- plm(log(intraday_vol_mean+1) ~ nbr_2 * postwar, 
                   data = dfp2, model = "within", effect = "twoways")
summary(model_vol05)
coeftest(model_vol05, vcov. = vcovDC(model_vol05, type = "HC0"))

model_vol06 <- plm(log(intraday_vol_mean+1) ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                   data = dfp2, model = "within", effect = "twoways")
summary(model_vol06)
coeftest(model_vol06, vcov. = vcovDC(model_vol06, type = "HC0"))

###

model_vol07 <- plm(log(intraday_vol_mean+1) ~ nbr_1_or_2 * postwar, 
                   data = dfp2, model = "within", effect = "twoways")
summary(model_vol07)
coeftest(model_vol07, vcov. = vcovDC(model_vol07, type = "HC0"))

model_vol08 <- plm(log(intraday_vol_mean+1) ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                   data = dfp2, model = "within", effect = "twoways")
summary(model_vol08)
coeftest(model_vol08, vcov. = vcovDC(model_vol08, type = "HC0"))

###

model_vol09 <- plm(log(intraday_vol_mean+1) ~ log(treat_ukr_inv) * postwar, 
                   data = dfp2, model = "within", effect = "twoways")
summary(model_vol09)
coeftest(model_vol09, vcov. = vcovDC(model_vol09, type = "HC0"))

model_vol10 <- plm(log(intraday_vol_mean+1) ~ log(treat_ukr_inv) * postwar + log(mktval) + inv_price + log(quotes_count), 
                   data = dfp2, model = "within", effect = "twoways")
summary(model_vol10)
coeftest(model_vol10, vcov. = vcovDC(model_vol10, type = "HC0"))


######################## Intraday Volatility (5-Minute) ########################

model_5m_vol01 <- plm(log(intraday_5m_vol_mean+1) ~ postwar, 
                      data = dfp2, model = "within", effect = "individual")
summary(model_5m_vol01)
coeftest(model_5m_vol01, vcov. = vcovHC(model_5m_vol01, type = "HC0"))

model_5m_vol02 <- plm(log(intraday_5m_vol_mean+1) ~ postwar + log(mktval) + inv_price  + log(quotes_count), 
                      data = dfp2, model = "within", effect = "individual")
summary(model_5m_vol02)
coeftest(model_5m_vol02, vcov. = vcovHC(model_5m_vol02, type = "HC0"))

###

model_5m_vol03 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_1 * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_5m_vol03)
coeftest(model_5m_vol03, vcov. = vcovDC(model_5m_vol03, type = "HC0"))

model_5m_vol04 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_5m_vol04)
coeftest(model_5m_vol04, vcov. = vcovDC(model_5m_vol04, type = "HC0"))

###

model_5m_vol05 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_2 * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_5m_vol05)
coeftest(model_5m_vol05, vcov. = vcovDC(model_5m_vol05, type = "HC0"))

model_5m_vol06 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_5m_vol06)
coeftest(model_5m_vol06, vcov. = vcovDC(model_5m_vol06, type = "HC0"))

###

model_5m_vol07 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_1_or_2 * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_5m_vol07)
coeftest(model_5m_vol07, vcov. = vcovDC(model_5m_vol07, type = "HC0"))

model_5m_vol08 <- plm(log(intraday_5m_vol_mean+1) ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_5m_vol08)
coeftest(model_5m_vol08, vcov. = vcovDC(model_5m_vol08, type = "HC0"))

###

model_5m_vol09 <- plm(log(intraday_5m_vol_mean+1) ~ log(treat_ukr_inv) * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_5m_vol09)
coeftest(model_5m_vol09, vcov. = vcovDC(model_5m_vol09, type = "HC0"))

model_5m_vol10 <- plm(log(intraday_5m_vol_mean+1) ~ log(treat_ukr_inv) * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_5m_vol10)
coeftest(model_5m_vol10, vcov. = vcovDC(model_5m_vol10, type = "HC0"))


# Model panels
panelA <- list(model_vol01,model_vol02,model_vol03,model_vol04,model_vol05,
               model_vol06,model_vol07,model_vol08,model_vol09,model_vol10)
panelB <- list(model_5m_vol01,model_5m_vol02,model_5m_vol03,model_5m_vol04,
               model_5m_vol05,model_5m_vol06,model_5m_vol07,model_5m_vol08,
               model_5m_vol09,model_5m_vol10)

# Terms and labels
treat_terms <- c("postwar","nbr_1:postwar","nbr_2:postwar",
                 "nbr_1_or_2:postwar","log(treat_ukr_inv):postwar")
treat_labels<- c("$War$","$Neighbour_1 \\times War$",
                 "$Neighbour_2 \\times War$",
                 "$Neighbour_{1-2} \\times War$",
                 "$Distance \\times War$")
controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels<-c("Market Value (log)","Price (inverse)","No. Quotes (log)")

# Begin table
sink("risk_psm.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Matched Sample: Risk Models. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lrcccccccccc}\n",
    "\\toprule\n",
    "%\\multicolumn{12}{l}{\\textit{Panel A: Overall Sample}} \\\\ \\toprule\n",
    "&&\\multicolumn{10}{c}{Intraday Volatility (open-to-close)} \\\\ \\cmidrule{3-12}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10) \\\\\n\\midrule\n")

# ------------------------ Panel A ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelA, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) {
        if (treat_terms[i]=="postwar" && j>2) "" 
        else if (treat_terms[i]!="postwar" && j<=2) "" 
        else fmt(info[[j]]$coef)
      }), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j) {
        if (treat_terms[i]=="postwar" && j>2) ""
        else if (treat_terms[i]!="postwar" && j<=2) ""
        else if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) "" 
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }), collapse=" & "),
      " \\\\\n")
}

# Controls
for (j in seq_along(controls)){
  info <- lapply(panelA, get_info, controls[j])
  cat(ctrl_labels[j]," & &",
      paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(info,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" 
        else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " \\\\\n")
}

r2A<-sapply(panelA,get_r2); adjA<-pmax(0, sapply(panelA, get_adj), na.rm = FALSE); nA<-sapply(panelA,get_n)
cat("\\midrule\n",
    "Firm FEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "Day FEs & &No &No &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2A),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjA),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nA),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "%\\multicolumn{12}{l}{\\textit{Panel B: Matched Sample}} \\\\ \\toprule\n",
    "&&\\multicolumn{10}{c}{Intraday Volatility (5-minute)} \\\\ \\cmidrule{3-12}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10) \\\\\n\\midrule\n")

# ------------------------ Panel B ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelB, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) {
        if (treat_terms[i]=="postwar" && j>2) "" 
        else if (treat_terms[i]!="postwar" && j<=2) "" 
        else fmt(info[[j]]$coef)
      }), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j) {
        if (treat_terms[i]=="postwar" && j>2) ""
        else if (treat_terms[i]!="postwar" && j<=2) ""
        else if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) "" 
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }), collapse=" & "),
      " \\\\\n")
}

# Controls Panel B
for (j in seq_along(controls)){
  info <- lapply(panelB, get_info, controls[j])
  cat(ctrl_labels[j]," & &",
      paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(info,function(x){
        if (is.na(x$se) | is.na(x$coef)) "" 
        else paste0("(",fmt_se(x$se),")",star(x$p))
      }),collapse=" & "),
      " \\\\\n")
}

r2B<-sapply(panelB,get_r2); adjB<-pmax(0, sapply(panelB, get_adj), na.rm = FALSE); nB<-sapply(panelB,get_n)
cat("\\midrule\n",
    "Firm FEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "Day FEs & &No &No &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2B),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjB),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nB),collapse=" & ")," \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to risk_psm.tex")





################################################################################
# Baseline Models - Overall Sample
################################################################################


############################ Relative Quoted Spread ############################

model_qspread01 <- plm(qspread_mean ~ nbr_1 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread01)
coeftest(model_qspread01, vcov. = vcovDC(model_qspread01, type = "HC0"))

model_qspread02 <- plm(qspread_mean ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_qspread02)
coeftest(model_qspread02, vcov. = vcovDC(model_qspread02, type = "HC0"))

model_qspread03 <- plm(qspread_mean ~ nbr_2 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread03)
coeftest(model_qspread03, vcov. = vcovDC(model_qspread03, type = "HC0"))

model_qspread04 <- plm(qspread_mean ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_qspread04)
coeftest(model_qspread04, vcov. = vcovDC(model_qspread04, type = "HC0"))

model_qspread05 <- plm(qspread_mean ~ nbr_1_or_2 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread05)
coeftest(model_qspread05, vcov. = vcovDC(model_qspread05, type = "HC0"))

model_qspread06 <- plm(qspread_mean ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_qspread06)
coeftest(model_qspread06, vcov. = vcovDC(model_qspread06, type = "HC0"))

model_qspread07 <- plm(qspread_mean ~ treat_ukr_inv * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread07)
coeftest(model_qspread07, vcov. = vcovDC(model_qspread07, type = "HC0"))

model_qspread08 <- plm(qspread_mean ~ treat_ukr_inv * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_qspread08)
coeftest(model_qspread08, vcov. = vcovDC(model_qspread08, type = "HC0"))

model_qspread09 <- plm(qspread_mean ~ intraday_vol_mean * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread09)
coeftest(model_qspread09, vcov. = vcovDC(model_qspread09, type = "HC0"))

model_qspread10 <- plm(qspread_mean ~ intraday_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_qspread10)
coeftest(model_qspread10, vcov. = vcovDC(model_qspread10, type = "HC0"))

model_qspread11 <- plm(qspread_mean ~ treat_economic, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread11)
coeftest(model_qspread11, vcov. = vcovDC(model_qspread11, type = "HC0"))

model_qspread18 <- plm(qspread_mean ~ treat_economic + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread18)
coeftest(model_qspread18, vcov. = vcovDC(model_qspread18, type = "HC0"))

model_qspread12 <- plm(qspread_mean ~ nbr_1 * postwar + treat_dist_intensity1 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread12)
coeftest(model_qspread12, vcov. = vcovDC(model_qspread12, type = "HC0"))

model_qspread13 <- plm(qspread_mean ~ nbr_1 * postwar + treat_dist_intensity1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread13)
coeftest(model_qspread13, vcov. = vcovDC(model_qspread13, type = "HC0"))

model_qspread14 <- plm(qspread_mean ~ nbr_2 * postwar + treat_dist_intensity2 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread14)
coeftest(model_qspread14, vcov. = vcovDC(model_qspread14, type = "HC0"))

model_qspread15 <- plm(qspread_mean ~ nbr_2 * postwar + treat_dist_intensity2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread15)
coeftest(model_qspread15, vcov. = vcovDC(model_qspread15, type = "HC0"))

model_qspread16 <- plm(qspread_mean ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread16)
coeftest(model_qspread16, vcov. = vcovDC(model_qspread16, type = "HC0"))

model_qspread17 <- plm(qspread_mean ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_qspread17)
coeftest(model_qspread17, vcov. = vcovDC(model_qspread17, type = "HC0"))


############################### Effective Spread ############################### 

model_espread01 <- plm(espread_mean ~ nbr_1 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread01)
coeftest(model_espread01, vcov. = vcovDC(model_espread01, type = "HC0"))

model_espread02 <- plm(espread_mean ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread02)
coeftest(model_espread02, vcov. = vcovDC(model_espread02, type = "HC0"))

model_espread03 <- plm(espread_mean ~ nbr_2 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread03)
coeftest(model_espread03, vcov. = vcovDC(model_espread03, type = "HC0"))

model_espread04 <- plm(espread_mean ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread04)
coeftest(model_espread04, vcov. = vcovDC(model_espread04, type = "HC0"))

model_espread05 <- plm(espread_mean ~ nbr_1_or_2 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread05)
coeftest(model_espread05, vcov. = vcovDC(model_espread05, type = "HC0"))

model_espread06 <- plm(espread_mean ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread06)
coeftest(model_espread06, vcov. = vcovDC(model_espread06, type = "HC0"))

model_espread07 <- plm(espread_mean ~ treat_ukr_inv * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread07)
coeftest(model_espread07, vcov. = vcovDC(model_espread07, type = "HC0"))

model_espread08 <- plm(espread_mean ~ treat_ukr_inv * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread08)
coeftest(model_espread08, vcov. = vcovDC(model_espread08, type = "HC0"))

model_espread09 <- plm(espread_mean ~ intraday_vol_mean * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread09)
coeftest(model_espread09, vcov. = vcovDC(model_espread09, type = "HC0"))

model_espread10 <- plm(espread_mean ~ intraday_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread10)
coeftest(model_espread10, vcov. = vcovDC(model_espread10, type = "HC0"))

model_espread11 <- plm(espread_mean ~ treat_economic, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread11)
coeftest(model_espread11, vcov. = vcovDC(model_espread11, type = "HC0"))

model_espread18 <- plm(espread_mean ~ treat_economic + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread18)
coeftest(model_espread18, vcov. = vcovDC(model_espread18, type = "HC0"))

model_espread12 <- plm(espread_mean ~ nbr_1 * postwar + treat_dist_intensity1 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread12)
coeftest(model_espread12, vcov. = vcovDC(model_espread12, type = "HC0"))

model_espread13 <- plm(espread_mean ~ nbr_1 * postwar + treat_dist_intensity1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread13)
coeftest(model_espread13, vcov. = vcovDC(model_espread13, type = "HC0"))

model_espread14 <- plm(espread_mean ~ nbr_2 * postwar + treat_dist_intensity2 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread14)
coeftest(model_espread14, vcov. = vcovDC(model_espread14, type = "HC0"))

model_espread15 <- plm(espread_mean ~ nbr_2 * postwar + treat_dist_intensity2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread15)
coeftest(model_espread15, vcov. = vcovDC(model_espread15, type = "HC0"))

model_espread16 <- plm(espread_mean ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread16)
coeftest(model_espread16, vcov. = vcovDC(model_espread16, type = "HC0"))

model_espread17 <- plm(espread_mean ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_espread17)
coeftest(model_espread17, vcov. = vcovDC(model_espread17, type = "HC0"))


################################ Dollar Volumes ################################   

model_dvolume01 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume01)
coeftest(model_dvolume01, vcov. = vcovDC(model_dvolume01, type = "HC0"))

model_dvolume02 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume02)
coeftest(model_dvolume02, vcov. = vcovDC(model_dvolume02, type = "HC0"))

model_dvolume03 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume03)
coeftest(model_dvolume03, vcov. = vcovDC(model_dvolume03, type = "HC0"))

model_dvolume04 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume04)
coeftest(model_dvolume04, vcov. = vcovDC(model_dvolume04, type = "HC0"))

model_dvolume05 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume05)
coeftest(model_dvolume05, vcov. = vcovDC(model_dvolume05, type = "HC0"))

model_dvolume06 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume06)
coeftest(model_dvolume06, vcov. = vcovDC(model_dvolume06, type = "HC0"))

model_dvolume07 <- plm(log(dollar_volume_sum+1) ~ treat_ukr_inv * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume07)
coeftest(model_dvolume07, vcov. = vcovDC(model_dvolume07, type = "HC0"))

model_dvolume08 <- plm(log(dollar_volume_sum+1) ~ treat_ukr_inv * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume08)
coeftest(model_dvolume08, vcov. = vcovDC(model_dvolume08, type = "HC0"))

model_dvolume09 <- plm(log(dollar_volume_sum+1) ~ intraday_vol_mean * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume09)
coeftest(model_dvolume09, vcov. = vcovDC(model_dvolume09, type = "HC0"))

model_dvolume10 <- plm(log(dollar_volume_sum+1) ~ intraday_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume10)
coeftest(model_dvolume10, vcov. = vcovDC(model_dvolume10, type = "HC0"))

model_dvolume11 <- plm(log(dollar_volume_sum+1) ~ treat_economic, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume11)
coeftest(model_dvolume11, vcov. = vcovDC(model_dvolume11, type = "HC0"))

model_dvolume18 <- plm(log(dollar_volume_sum+1) ~ treat_economic + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume18)
coeftest(model_dvolume18, vcov. = vcovDC(model_dvolume18, type = "HC0"))

model_dvolume12 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * postwar + treat_dist_intensity1 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume12)
coeftest(model_dvolume12, vcov. = vcovDC(model_dvolume12, type = "HC0"))

model_dvolume13 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * postwar + treat_dist_intensity1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume13)
coeftest(model_dvolume13, vcov. = vcovDC(model_dvolume13, type = "HC0"))

model_dvolume14 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * postwar + treat_dist_intensity2 * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume14)
coeftest(model_dvolume14, vcov. = vcovDC(model_dvolume14, type = "HC0"))

model_dvolume15 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * postwar + treat_dist_intensity2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume15)
coeftest(model_dvolume15, vcov. = vcovDC(model_dvolume15, type = "HC0"))

model_dvolume16 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume16)
coeftest(model_dvolume16, vcov. = vcovDC(model_dvolume16, type = "HC0"))

model_dvolume17 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_dvolume17)
coeftest(model_dvolume17, vcov. = vcovDC(model_dvolume17, type = "HC0"))


############################### Number of Trades ###############################

model_trades01 <- plm(log(trades_count+1) ~ nbr_1 * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades01)
coeftest(model_trades01, vcov. = vcovDC(model_trades01, type = "HC0"))

model_trades02 <- plm(log(trades_count+1) ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades02)
coeftest(model_trades02, vcov. = vcovDC(model_trades02, type = "HC0"))

model_trades03 <- plm(log(trades_count+1) ~ nbr_2 * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades03)
coeftest(model_trades03, vcov. = vcovDC(model_trades03, type = "HC0"))

model_trades04 <- plm(log(trades_count+1) ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades04)
coeftest(model_trades04, vcov. = vcovDC(model_trades04, type = "HC0"))

model_trades05 <- plm(log(trades_count+1) ~ nbr_1_or_2 * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades05)
coeftest(model_trades05, vcov. = vcovDC(model_trades05, type = "HC0"))

model_trades06 <- plm(log(trades_count+1) ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades06)
coeftest(model_trades06, vcov. = vcovDC(model_trades06, type = "HC0"))

model_trades07 <- plm(log(trades_count+1) ~ treat_ukr_inv * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades07)
coeftest(model_trades07, vcov. = vcovDC(model_trades07, type = "HC0"))

model_trades08 <- plm(log(trades_count+1) ~ treat_ukr_inv * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades08)
coeftest(model_trades08, vcov. = vcovDC(model_trades08, type = "HC0"))

model_trades09 <- plm(log(trades_count+1) ~ intraday_vol_mean * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades09)
coeftest(model_trades09, vcov. = vcovDC(model_trades09, type = "HC0"))

model_trades10 <- plm(log(trades_count+1) ~ intraday_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades10)
coeftest(model_trades10, vcov. = vcovDC(model_trades10, type = "HC0"))

model_trades11 <- plm(log(trades_count+1) ~ treat_economic, 
                       data = dfp, model = "within", effect = "twoways")
summary(model_trades11)
coeftest(model_trades11, vcov. = vcovDC(model_trades11, type = "HC0"))

model_trades18 <- plm(log(trades_count+1) ~ treat_economic + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp, model = "within", effect = "twoways")
summary(model_trades18)
coeftest(model_trades18, vcov. = vcovDC(model_trades18, type = "HC0"))

model_trades12 <- plm(log(trades_count+1) ~ nbr_1 * postwar + treat_dist_intensity1 * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades12)
coeftest(model_trades12, vcov. = vcovDC(model_trades12, type = "HC0"))

model_trades13 <- plm(log(trades_count+1) ~ nbr_1 * postwar + treat_dist_intensity1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades13)
coeftest(model_trades13, vcov. = vcovDC(model_trades13, type = "HC0"))

model_trades14 <- plm(log(trades_count+1) ~ nbr_2 * postwar + treat_dist_intensity2 * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades14)
coeftest(model_trades14, vcov. = vcovDC(model_trades14, type = "HC0"))

model_trades15 <- plm(log(trades_count+1) ~ nbr_2 * postwar + treat_dist_intensity2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades15)
coeftest(model_trades15, vcov. = vcovDC(model_trades15, type = "HC0"))

model_trades16 <- plm(log(trades_count+1) ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar, 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades16)
coeftest(model_trades16, vcov. = vcovDC(model_trades16, type = "HC0"))

model_trades17 <- plm(log(trades_count+1) ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp, model = "within", effect = "twoways")
summary(model_trades17)
coeftest(model_trades17, vcov. = vcovDC(model_trades17, type = "HC0"))


################################################################################
# --- LATEX TABLES ---
################################################################################


#============================================================
#========== 0. Helper Objects (panels & labels) =========
#============================================================
  
### Panels for MODELS 1–6
panel_qspread_1_6 <- list(model_qspread01,model_qspread02,model_qspread03,
                          model_qspread04,model_qspread05,model_qspread06)

panel_espread_1_6 <- list(model_espread01,model_espread02,model_espread03,
                          model_espread04,model_espread05,model_espread06)

panel_dvol_1_6    <- list(model_dvolume01,model_dvolume02,model_dvolume03,
                          model_dvolume04,model_dvolume05,model_dvolume06)

panel_trades_1_6  <- list(model_trades01,model_trades02,model_trades03,
                          model_trades04,model_trades05,model_trades06)


### Panels for MODELS 12–17 (Distance × War interactions)
panel_qspread_12_17 <- list(model_qspread12,model_qspread13,model_qspread14,
                            model_qspread15,model_qspread16,model_qspread17)

panel_espread_12_17 <- list(model_espread12,model_espread13,model_espread14,
                            model_espread15,model_espread16,model_espread17)

panel_dvol_12_17    <- list(model_dvolume12,model_dvolume13,model_dvolume14,
                            model_dvolume15,model_dvolume16,model_dvolume17)

panel_trades_12_17  <- list(model_trades12,model_trades13,model_trades14,
                            model_trades15,model_trades16,model_trades17)


### Panels for MODELS 11–18 (LogReturn × War)
panel_return <- list(
  qspread = list(model_qspread11, model_qspread18),
  espread = list(model_espread11, model_espread18),
  dvolume = list(model_dvolume11, model_dvolume18),
  trades  = list(model_trades11, model_trades18)
)

### Panels for MODELS 7–8 (Distance × War)
panel_distance <- list(
  qspread = list(model_qspread07, model_qspread08),
  espread = list(model_espread07, model_espread08),
  dvolume = list(model_dvolume07, model_dvolume08),
  trades  = list(model_trades07, model_trades08)
)

### Panels for MODELS 9–10 (Volatility × War)
panel_volatility <- list(
  qspread = list(model_qspread09, model_qspread10),
  espread = list(model_espread09, model_espread10),
  dvolume = list(model_dvolume09, model_dvolume10),
  trades  = list(model_trades09, model_trades10)
)


### Labels
treat_terms_main <- c("nbr_1:postwar","nbr_2:postwar","nbr_1_or_2:postwar")
treat_labels_main <- c("$Neighbour_1 \\times War$",
                       "$Neighbour_2 \\times War$",
                       "$Neighbour_{1-2} \\times War$")

treat_terms_dist <- c(
  "nbr_1:postwar",
  "postwar:treat_dist_intensity1",
  "nbr_2:postwar",
  "postwar:treat_dist_intensity2",
  "nbr_1_or_2:postwar",
  "postwar:treat_dist_intensity"
)

treat_labels_dist <- c(
  "$Neighbour_1 \\times War$",
  "$Neighbour_1 \\times Distance \\times War$",
  "$Neighbour_2 \\times War$",
  "$Neighbour_2 \\times Distance \\times War$",
  "$Neighbour_{1-2} \\times War$",
  "$Neighbour_{1-2} \\times Distance \\times War$"
)

controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels <- c("Market Value (log)","Price (inverse)","No. Quotes (log)")


#============================================================
#====== 1. MAIN TABLES: MODELS 1–6 (All y variables) =====
#============================================================

make_table_1_6 <- function(filename, panelA, panelB, panelA_title, panelB_title){
  
  sink(filename)
  cat("\\begin{table}\\centering\n",
      "\\caption{Difference-in-differences regression results for Overall Sample: Neighbour Dummy. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
      "\\begin{adjustbox}{max width=\\textwidth}\n",
      "\\begin{tabular}{lrcccccc}\n",
      "\\toprule\n",
      "&&\\multicolumn{6}{c}{", panelA_title, "} \\\\ \\cmidrule{3-8}\n",
      " & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")
  
  ## PANEL A — Main Treatment Effects
  for (i in seq_along(treat_terms_main)){
    info <- lapply(panelA, get_info, treat_terms_main[i])
    cat(treat_labels_main[i], " & &",
        paste(sapply(info, function(x) fmt(x$coef)), collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info, function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }), collapse=" & "), "\\\\\n")
  }
  
  ## Controls
  for (j in seq_along(controls)){
    info <- lapply(panelA, get_info, controls[j])
    cat(ctrl_labels[j]," & &",
        paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info,function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Bottom stats PANEL A
  r2A <- sapply(panelA,get_r2); adjA <- pmax(0,sapply(panelA,get_adj), na.rm = FALSE); nA <- sapply(panelA,get_n)
  cat("\\midrule\n",
      "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
      "$R^{2}$ & &", paste(pct(r2A),collapse=" & "), " \\\\\n",
      "Adj.-$R^{2}$ & &", paste(pct(adjA),collapse=" & "), " \\\\\n",
      "No. Obs. & &", paste(fmt_num(nA),collapse=" & "), " \\\\\n",
      "\\midrule\n",
      "&&\\multicolumn{6}{c}{", panelB_title, "} \\\\ \\cmidrule{3-8}\n",
      " & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")
  
  ## PANEL B — Same logic
  for (i in seq_along(treat_terms_main)){
    info <- lapply(panelB, get_info, treat_terms_main[i])
    cat(treat_labels_main[i], " & &",
        paste(sapply(info, function(x) fmt(x$coef)), collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info, function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }), collapse=" & "), "\\\\\n")
  }
  
  ## Controls PANEL B
  for (j in seq_along(controls)){
    info <- lapply(panelB, get_info, controls[j])
    cat(ctrl_labels[j]," & &",
        paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info,function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Bottom stats PANEL B
  r2B <- sapply(panelB,get_r2); adjB <- pmax(0,sapply(panelB,get_adj), na.rm = FALSE); nB <- sapply(panelB,get_n)
  cat("\\midrule\n",
      "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
      "$R^{2}$ & &", paste(pct(r2B),collapse=" & "), " \\\\\n",
      "Adj.-$R^{2}$ & &", paste(pct(adjB),collapse=" & "), " \\\\\n",
      "No. Obs. & &", paste(fmt_num(nB),collapse=" & "), " \\\\\n",
      "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
  sink()
}


# Generate tables for models 1-6

make_table_1_6("liquidity_neighbour.tex",
               panelA = panel_qspread_1_6,
               panelB = panel_espread_1_6,
               panelA_title = "Quoted Spread (\\%)",
               panelB_title = "Effective Spread (\\%)")

make_table_1_6("trading_activity_neighbour.tex",
               panelA = panel_dvol_1_6,
               panelB = panel_trades_1_6,
               panelA_title = "Dollar Volume (log)",
               panelB_title = "Number of Trades (log)")


#============================================================
#  === 2. DISTANCE × WAR TABLES (MODELS 12–17) — same format
#============================================================

make_table_12_17 <- function(filename, panelA, panelB, panelA_title, panelB_title){
  
  sink(filename)
  cat("\\begin{table}\\centering\n",
      "\\caption{Difference-in-differences regression results for Overall Sample: Neighbour Distance Intensity. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
      "\\begin{adjustbox}{max width=\\textwidth}\n",
      "\\begin{tabular}{lrcccccc}\n",
      "\\toprule\n",
      "&&\\multicolumn{6}{c}{", panelA_title, "} \\\\ \\cmidrule{3-8}\n",
      " & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")
  
  ## PANEL A
  for (i in seq_along(treat_terms_dist)){
    info <- lapply(panelA, get_info, treat_terms_dist[i])
    cat(treat_labels_dist[i], " & &",
        paste(sapply(info, function(x) fmt(x$coef)), collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info, function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }), collapse=" & "), "\\\\\n")
  }
  
  ## Controls PANEL A
  for (j in seq_along(controls)){
    info <- lapply(panelA, get_info, controls[j])
    cat(ctrl_labels[j]," & &",
        paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info,function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Bottom stats PANEL A
  r2A <- sapply(panelA,get_r2); adjA <- pmax(0,sapply(panelA,get_adj), na.rm = FALSE); nA <- sapply(panelA,get_n)
  cat("\\midrule\n",
      "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
      "$R^{2}$ & &", paste(pct(r2A),collapse=" & "), " \\\\\n",
      "Adj.-$R^{2}$ & &", paste(pct(adjA),collapse=" & "), " \\\\\n",
      "No. Obs. & &", paste(fmt_num(nA),collapse=" & "), " \\\\\n",
      "\\midrule\n",
      "&&\\multicolumn{6}{c}{", panelB_title, "} \\\\ \\cmidrule{3-8}\n",
      " & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")
  
  ## PANEL B
  for (i in seq_along(treat_terms_dist)){
    info <- lapply(panelB, get_info, treat_terms_dist[i])
    cat(treat_labels_dist[i], " & &",
        paste(sapply(info, function(x) fmt(x$coef)), collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info, function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Controls PANEL B
  for (j in seq_along(controls)){
    info <- lapply(panelB, get_info, controls[j])
    cat(ctrl_labels[j]," & &",
        paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info,function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Bottom stats PANEL B
  r2B <- sapply(panelB,get_r2); adjB <- pmax(0,sapply(panelB,get_adj), na.rm = FALSE); nB <- sapply(panelB,get_n)
  cat("\\midrule\n",
      "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
      "$R^{2}$ & &", paste(pct(r2B),collapse=" & "), " \\\\\n",
      "Adj.-$R^{2}$ & &", paste(pct(adjB),collapse=" & "), " \\\\\n",
      "No. Obs. & &", paste(fmt_num(nB),collapse=" & "), " \\\\\n",
      "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
  sink()
}

make_table_12_17("liquidity_intensity.tex",
                 panelA = panel_qspread_12_17,
                 panelB = panel_espread_12_17,
                 panelA_title = "Quoted Spread (\\%)",
                 panelB_title = "Effective Spread (\\%)")

make_table_12_17("trading_activity_intensity.tex",
                 panelA = panel_dvol_12_17,
                 panelB = panel_trades_12_17,
                 panelA_title = "Dollar Volume (log)",
                 panelB_title = "Number of Trades (log)")


#============================================================
#====== 3. DISTANCE AND VOLATILITY MODELS (Models 7–8) ======
#============================================================

make_2model_multiy_table <- function(filename, 
                                     panels,           # list(qspread=list(m1,m2), espread=list(m1,m2), ...)
                                     treat_term,       # e.g. "treat_ukr_inv:postwar"
                                     treat_label,      # LaTeX label
                                     table_caption){
  
  sink(filename)
  
  cat("\\begin{table}\\centering\n",
      "\\caption{", table_caption, "}\n",
      "\\begin{adjustbox}{max width=\\textwidth}\n",
      "\\begin{tabular}{lccccccccccc}\n",
      "\\toprule\n",
      " & \\multicolumn{2}{c}{QSpread (\\%)} & & \\multicolumn{2}{c}{ESpread (\\%)} & &",
      " \\multicolumn{2}{c}{DollarVol (log)} & & \\multicolumn{2}{c}{No. Trades (log)} \\\\ \\cmidrule{2-3}\\cmidrule{5-6}\\cmidrule{8-9}\\cmidrule{11-12} \n",
      " & (1) & (2) & & (3) & (4) & & (5) & (6) & & (7) & (8) \\\\\n",
      "\\midrule\n")
  
  ## --------------- TREATMENT ROW ----------------
  info_q  <- lapply(panels$qspread, get_info,  treat_term)
  info_e  <- lapply(panels$espread, get_info,  treat_term)
  info_d  <- lapply(panels$dvolume, get_info,  treat_term)
  info_t  <- lapply(panels$trades,  get_info,  treat_term)
  
  cat(treat_label, " & ",
      fmt(info_q[[1]]$coef), " & ", fmt(info_q[[2]]$coef), " & & ",
      fmt(info_e[[1]]$coef), " & ", fmt(info_e[[2]]$coef), " & & ",
      fmt(info_d[[1]]$coef), " & ", fmt(info_d[[2]]$coef), " & & ",
      fmt(info_t[[1]]$coef), " & ", fmt(info_t[[2]]$coef), "\\\\\n")
  
  cat(" & ",
      paste0("(",fmt_se(info_q[[1]]$se),")",star(info_q[[1]]$p)), " & ",
      paste0("(",fmt_se(info_q[[2]]$se),")",star(info_q[[2]]$p)), " & & ",
      paste0("(",fmt_se(info_e[[1]]$se),")",star(info_e[[1]]$p)), " & ",
      paste0("(",fmt_se(info_e[[2]]$se),")",star(info_e[[2]]$p)), " & & ",
      paste0("(",fmt_se(info_d[[1]]$se),")",star(info_d[[1]]$p)), " & ",
      paste0("(",fmt_se(info_d[[2]]$se),")",star(info_d[[2]]$p)), " & & ",
      paste0("(",fmt_se(info_t[[1]]$se),")",star(info_t[[1]]$p)), " & ",
      paste0("(",fmt_se(info_t[[2]]$se),")",star(info_t[[2]]$p)), "\\\\\n")
  
  ## --------------- CONTROLS ----------------
  for (j in seq_along(controls)) {
    ctrl <- controls[j]
    clab <- ctrl_labels[j]
    
    info_q <- lapply(panels$qspread, get_info, ctrl)
    info_e <- lapply(panels$espread, get_info, ctrl)
    info_d <- lapply(panels$dvolume, get_info, ctrl)
    info_t <- lapply(panels$trades,  get_info, ctrl)
    
    cat(clab, " & ",
        ifelse(is.na(info_q[[1]]$coef),"",fmt(info_q[[1]]$coef)), " & ",
        ifelse(is.na(info_q[[2]]$coef),"",fmt(info_q[[2]]$coef)), " & & ",
        ifelse(is.na(info_e[[1]]$coef),"",fmt(info_e[[1]]$coef)), " & ",
        ifelse(is.na(info_e[[2]]$coef),"",fmt(info_e[[2]]$coef)), " & & ",
        ifelse(is.na(info_d[[1]]$coef),"",fmt(info_d[[1]]$coef)), " & ",
        ifelse(is.na(info_d[[2]]$coef),"",fmt(info_d[[2]]$coef)), " & & ",
        ifelse(is.na(info_t[[1]]$coef),"",fmt(info_t[[1]]$coef)), " & ",
        ifelse(is.na(info_t[[2]]$coef),"",fmt(info_t[[2]]$coef)),
        "\\\\\n")
    
    cat(" & ",
        ifelse(is.na(info_q[[1]]$coef),"",paste0("(",fmt_se(info_q[[1]]$se),")",star(info_q[[1]]$p))), " & ",
        ifelse(is.na(info_q[[2]]$coef),"",paste0("(",fmt_se(info_q[[2]]$se),")",star(info_q[[2]]$p))), " & & ",
        ifelse(is.na(info_e[[1]]$coef),"",paste0("(",fmt_se(info_e[[1]]$se),")",star(info_e[[1]]$p))), " & ",
        ifelse(is.na(info_e[[2]]$coef),"",paste0("(",fmt_se(info_e[[2]]$se),")",star(info_e[[2]]$p))), " & & ",
        ifelse(is.na(info_d[[1]]$coef),"",paste0("(",fmt_se(info_d[[1]]$se),")",star(info_d[[1]]$p))), " & ",
        ifelse(is.na(info_d[[2]]$coef),"",paste0("(",fmt_se(info_d[[2]]$se),")",star(info_d[[2]]$p))), " & & ",
        ifelse(is.na(info_t[[1]]$coef),"",paste0("(",fmt_se(info_t[[1]]$se),")",star(info_t[[1]]$p))), " & ",
        ifelse(is.na(info_t[[2]]$coef),"",paste0("(",fmt_se(info_t[[2]]$se),")",star(info_t[[2]]$p))),
        "\\\\\n")
  }
  
  ## --------------- R2, Adj R2, N ----------------
  
  # R2 by outcome
  r2_q <- sapply(panels$qspread, get_r2)
  r2_e <- sapply(panels$espread, get_r2)
  r2_d <- sapply(panels$dvolume, get_r2)
  r2_t <- sapply(panels$trades,  get_r2)
  
  # Adj-R2 by outcome (truncated at zero)
  adj_q <- pmax(0, sapply(panels$qspread, get_adj))
  adj_e <- pmax(0, sapply(panels$espread, get_adj))
  adj_d <- pmax(0, sapply(panels$dvolume, get_adj))
  adj_t <- pmax(0, sapply(panels$trades,  get_adj))
  
  # No. of observations
  n_q <- sapply(panels$qspread, get_n)
  n_e <- sapply(panels$espread, get_n)
  n_d <- sapply(panels$dvolume, get_n)
  n_t <- sapply(panels$trades,  get_n)
  
  # pretty formatting for commas in N
  fmt_comma <- function(x) format(x, big.mark = ",", scientific = FALSE)
  
  cat("\\midrule\n",
      "TWFEs & Yes & Yes & & Yes & Yes & & Yes & Yes & & Yes & Yes \\\\\n",
      "$R^{2}$ & ",
      pct(r2_q[1]), " & ", pct(r2_q[2]), " & & ",
      pct(r2_e[1]), " & ", pct(r2_e[2]), " & & ",
      pct(r2_d[1]), " & ", pct(r2_d[2]), " & & ",
      pct(r2_t[1]), " & ", pct(r2_t[2]), " \\\\\n",
      "Adj-$R^{2}$ & ",
      pct(adj_q[1]), " & ", pct(adj_q[2]), " & & ",
      pct(adj_e[1]), " & ", pct(adj_e[2]), " & & ",
      pct(adj_d[1]), " & ", pct(adj_d[2]), " & & ",
      pct(adj_t[1]), " & ", pct(adj_t[2]), " \\\\\n",
      "No. Obs. & ",
      fmt_comma(n_q[1]), " & ", fmt_comma(n_q[2]), " & & ",
      fmt_comma(n_e[1]), " & ", fmt_comma(n_e[2]), " & & ",
      fmt_comma(n_d[1]), " & ", fmt_comma(n_d[2]), " & & ",
      fmt_comma(n_t[1]), " & ", fmt_comma(n_t[2]), " \\\\\n",
      "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
  
  sink()
}


make_2model_multiy_table(
  filename = "return_models.tex",
  panels = panel_return,
  treat_term = "treat_economic",
  treat_label = "$War Log Return$",
  table_caption = "Difference-in-differences regression results for Overall Sample: Economic Risk. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance."
)

make_2model_multiy_table(
  filename = "distance_models.tex",
  panels = panel_distance,
  treat_term = "treat_ukr_inv:postwar",
  treat_label = "$Distance \\times War$",
  table_caption = "Difference-in-differences regression results for Overall Sample: Spatial Distance. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance."
)

make_2model_multiy_table(
  filename = "volatility_models.tex",
  panels = panel_volatility,
  treat_term = "intraday_vol_mean:postwar",
  treat_label = "$Volatility_{OC} \\times War$",
  table_caption = "Difference-in-differences regression results for Overall Sample: Volatility. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance."
)



################################################################################
# Baseline Models - Matched Sample
################################################################################


############################ Relative Quoted Spread ############################

model_qspread01 <- plm(qspread_mean ~ nbr_1 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread01)
coeftest(model_qspread01, vcov. = vcovDC(model_qspread01, type = "HC0"))

model_qspread02 <- plm(qspread_mean ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread02)
coeftest(model_qspread02, vcov. = vcovDC(model_qspread02, type = "HC0"))

model_qspread03 <- plm(qspread_mean ~ nbr_2 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread03)
coeftest(model_qspread03, vcov. = vcovDC(model_qspread03, type = "HC0"))

model_qspread04 <- plm(qspread_mean ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread04)
coeftest(model_qspread04, vcov. = vcovDC(model_qspread04, type = "HC0"))

model_qspread05 <- plm(qspread_mean ~ nbr_1_or_2 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread05)
coeftest(model_qspread05, vcov. = vcovDC(model_qspread05, type = "HC0"))

model_qspread06 <- plm(qspread_mean ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread06)
coeftest(model_qspread06, vcov. = vcovDC(model_qspread06, type = "HC0"))

model_qspread07 <- plm(qspread_mean ~ treat_ukr_inv * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread07)
coeftest(model_qspread07, vcov. = vcovDC(model_qspread07, type = "HC0"))

model_qspread08 <- plm(qspread_mean ~ treat_ukr_inv * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread08)
coeftest(model_qspread08, vcov. = vcovDC(model_qspread08, type = "HC0"))

model_qspread09 <- plm(qspread_mean ~ intraday_vol_mean * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread09)
coeftest(model_qspread09, vcov. = vcovDC(model_qspread09, type = "HC0"))

model_qspread10 <- plm(qspread_mean ~ intraday_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread10)
coeftest(model_qspread10, vcov. = vcovDC(model_qspread10, type = "HC0"))

model_qspread11 <- plm(qspread_mean ~ treat_economic, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread11)
coeftest(model_qspread11, vcov. = vcovDC(model_qspread11, type = "HC0"))

model_qspread18 <- plm(qspread_mean ~ treat_economic + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread18)
coeftest(model_qspread18, vcov. = vcovDC(model_qspread18, type = "HC0"))

model_qspread12 <- plm(qspread_mean ~ nbr_1 * postwar + treat_dist_intensity1 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread12)
coeftest(model_qspread12, vcov. = vcovDC(model_qspread12, type = "HC0"))

model_qspread13 <- plm(qspread_mean ~ nbr_1 * postwar + treat_dist_intensity1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread13)
coeftest(model_qspread13, vcov. = vcovDC(model_qspread13, type = "HC0"))

model_qspread14 <- plm(qspread_mean ~ nbr_2 * postwar + treat_dist_intensity2 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread14)
coeftest(model_qspread14, vcov. = vcovDC(model_qspread14, type = "HC0"))

model_qspread15 <- plm(qspread_mean ~ nbr_2 * postwar + treat_dist_intensity2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread15)
coeftest(model_qspread15, vcov. = vcovDC(model_qspread15, type = "HC0"))

model_qspread16 <- plm(qspread_mean ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread16)
coeftest(model_qspread16, vcov. = vcovDC(model_qspread16, type = "HC0"))

model_qspread17 <- plm(qspread_mean ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread17)
coeftest(model_qspread17, vcov. = vcovDC(model_qspread17, type = "HC0"))


############################### Effective Spread ############################### 

model_espread01 <- plm(espread_mean ~ nbr_1 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread01)
coeftest(model_espread01, vcov. = vcovDC(model_espread01, type = "HC0"))

model_espread02 <- plm(espread_mean ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread02)
coeftest(model_espread02, vcov. = vcovDC(model_espread02, type = "HC0"))

model_espread03 <- plm(espread_mean ~ nbr_2 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread03)
coeftest(model_espread03, vcov. = vcovDC(model_espread03, type = "HC0"))

model_espread04 <- plm(espread_mean ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread04)
coeftest(model_espread04, vcov. = vcovDC(model_espread04, type = "HC0"))

model_espread05 <- plm(espread_mean ~ nbr_1_or_2 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread05)
coeftest(model_espread05, vcov. = vcovDC(model_espread05, type = "HC0"))

model_espread06 <- plm(espread_mean ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread06)
coeftest(model_espread06, vcov. = vcovDC(model_espread06, type = "HC0"))

model_espread07 <- plm(espread_mean ~ treat_ukr_inv * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread07)
coeftest(model_espread07, vcov. = vcovDC(model_espread07, type = "HC0"))

model_espread08 <- plm(espread_mean ~ treat_ukr_inv * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread08)
coeftest(model_espread08, vcov. = vcovDC(model_espread08, type = "HC0"))

model_espread09 <- plm(espread_mean ~ intraday_vol_mean * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread09)
coeftest(model_espread09, vcov. = vcovDC(model_espread09, type = "HC0"))

model_espread10 <- plm(espread_mean ~ intraday_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread10)
coeftest(model_espread10, vcov. = vcovDC(model_espread10, type = "HC0"))

model_espread11 <- plm(espread_mean ~ treat_economic, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread11)
coeftest(model_espread11, vcov. = vcovDC(model_espread11, type = "HC0"))

model_espread18 <- plm(espread_mean ~ treat_economic + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread18)
coeftest(model_espread18, vcov. = vcovDC(model_espread18, type = "HC0"))

model_espread12 <- plm(espread_mean ~ nbr_1 * postwar + treat_dist_intensity1 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread12)
coeftest(model_espread12, vcov. = vcovDC(model_espread12, type = "HC0"))

model_espread13 <- plm(espread_mean ~ nbr_1 * postwar + treat_dist_intensity1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread13)
coeftest(model_espread13, vcov. = vcovDC(model_espread13, type = "HC0"))

model_espread14 <- plm(espread_mean ~ nbr_2 * postwar + treat_dist_intensity2 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread14)
coeftest(model_espread14, vcov. = vcovDC(model_espread14, type = "HC0"))

model_espread15 <- plm(espread_mean ~ nbr_2 * postwar + treat_dist_intensity2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread15)
coeftest(model_espread15, vcov. = vcovDC(model_espread15, type = "HC0"))

model_espread16 <- plm(espread_mean ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread16)
coeftest(model_espread16, vcov. = vcovDC(model_espread16, type = "HC0"))

model_espread17 <- plm(espread_mean ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread17)
coeftest(model_espread17, vcov. = vcovDC(model_espread17, type = "HC0"))


################################ Dollar Volumes ################################   

model_dvolume01 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume01)
coeftest(model_dvolume01, vcov. = vcovDC(model_dvolume01, type = "HC0"))

model_dvolume02 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume02)
coeftest(model_dvolume02, vcov. = vcovDC(model_dvolume02, type = "HC0"))

model_dvolume03 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume03)
coeftest(model_dvolume03, vcov. = vcovDC(model_dvolume03, type = "HC0"))

model_dvolume04 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume04)
coeftest(model_dvolume04, vcov. = vcovDC(model_dvolume04, type = "HC0"))

model_dvolume05 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume05)
coeftest(model_dvolume05, vcov. = vcovDC(model_dvolume05, type = "HC0"))

model_dvolume06 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume06)
coeftest(model_dvolume06, vcov. = vcovDC(model_dvolume06, type = "HC0"))

model_dvolume07 <- plm(log(dollar_volume_sum+1) ~ treat_ukr_inv * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume07)
coeftest(model_dvolume07, vcov. = vcovDC(model_dvolume07, type = "HC0"))

model_dvolume08 <- plm(log(dollar_volume_sum+1) ~ treat_ukr_inv * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume08)
coeftest(model_dvolume08, vcov. = vcovDC(model_dvolume08, type = "HC0"))

model_dvolume09 <- plm(log(dollar_volume_sum+1) ~ intraday_vol_mean * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume09)
coeftest(model_dvolume09, vcov. = vcovDC(model_dvolume09, type = "HC0"))

model_dvolume10 <- plm(log(dollar_volume_sum+1) ~ intraday_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume10)
coeftest(model_dvolume10, vcov. = vcovDC(model_dvolume10, type = "HC0"))

model_dvolume11 <- plm(log(dollar_volume_sum+1) ~ treat_economic, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume11)
coeftest(model_dvolume11, vcov. = vcovDC(model_dvolume11, type = "HC0"))

model_dvolume18 <- plm(log(dollar_volume_sum+1) ~ treat_economic + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume18)
coeftest(model_dvolume18, vcov. = vcovDC(model_dvolume18, type = "HC0"))

model_dvolume12 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * postwar + treat_dist_intensity1 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume12)
coeftest(model_dvolume12, vcov. = vcovDC(model_dvolume12, type = "HC0"))

model_dvolume13 <- plm(log(dollar_volume_sum+1) ~ nbr_1 * postwar + treat_dist_intensity1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume13)
coeftest(model_dvolume13, vcov. = vcovDC(model_dvolume13, type = "HC0"))

model_dvolume14 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * postwar + treat_dist_intensity2 * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume14)
coeftest(model_dvolume14, vcov. = vcovDC(model_dvolume14, type = "HC0"))

model_dvolume15 <- plm(log(dollar_volume_sum+1) ~ nbr_2 * postwar + treat_dist_intensity2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume15)
coeftest(model_dvolume15, vcov. = vcovDC(model_dvolume15, type = "HC0"))

model_dvolume16 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume16)
coeftest(model_dvolume16, vcov. = vcovDC(model_dvolume16, type = "HC0"))

model_dvolume17 <- plm(log(dollar_volume_sum+1) ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume17)
coeftest(model_dvolume17, vcov. = vcovDC(model_dvolume17, type = "HC0"))


############################### Number of Trades ###############################

model_trades01 <- plm(log(trades_count+1) ~ nbr_1 * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades01)
coeftest(model_trades01, vcov. = vcovDC(model_trades01, type = "HC0"))

model_trades02 <- plm(log(trades_count+1) ~ nbr_1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades02)
coeftest(model_trades02, vcov. = vcovDC(model_trades02, type = "HC0"))

model_trades03 <- plm(log(trades_count+1) ~ nbr_2 * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades03)
coeftest(model_trades03, vcov. = vcovDC(model_trades03, type = "HC0"))

model_trades04 <- plm(log(trades_count+1) ~ nbr_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades04)
coeftest(model_trades04, vcov. = vcovDC(model_trades04, type = "HC0"))

model_trades05 <- plm(log(trades_count+1) ~ nbr_1_or_2 * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades05)
coeftest(model_trades05, vcov. = vcovDC(model_trades05, type = "HC0"))

model_trades06 <- plm(log(trades_count+1) ~ nbr_1_or_2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades06)
coeftest(model_trades06, vcov. = vcovDC(model_trades06, type = "HC0"))

model_trades07 <- plm(log(trades_count+1) ~ treat_ukr_inv * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades07)
coeftest(model_trades07, vcov. = vcovDC(model_trades07, type = "HC0"))

model_trades08 <- plm(log(trades_count+1) ~ treat_ukr_inv * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades08)
coeftest(model_trades08, vcov. = vcovDC(model_trades08, type = "HC0"))

model_trades09 <- plm(log(trades_count+1) ~ intraday_vol_mean * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades09)
coeftest(model_trades09, vcov. = vcovDC(model_trades09, type = "HC0"))

model_trades10 <- plm(log(trades_count+1) ~ intraday_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades10)
coeftest(model_trades10, vcov. = vcovDC(model_trades10, type = "HC0"))

model_trades11 <- plm(log(trades_count+1) ~ treat_economic, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades11)
coeftest(model_trades11, vcov. = vcovDC(model_trades11, type = "HC0"))

model_trades18 <- plm(log(trades_count+1) ~ treat_economic + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades18)
coeftest(model_trades18, vcov. = vcovDC(model_trades18, type = "HC0"))

model_trades12 <- plm(log(trades_count+1) ~ nbr_1 * postwar + treat_dist_intensity1 * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades12)
coeftest(model_trades12, vcov. = vcovDC(model_trades12, type = "HC0"))

model_trades13 <- plm(log(trades_count+1) ~ nbr_1 * postwar + treat_dist_intensity1 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades13)
coeftest(model_trades13, vcov. = vcovDC(model_trades13, type = "HC0"))

model_trades14 <- plm(log(trades_count+1) ~ nbr_2 * postwar + treat_dist_intensity2 * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades14)
coeftest(model_trades14, vcov. = vcovDC(model_trades14, type = "HC0"))

model_trades15 <- plm(log(trades_count+1) ~ nbr_2 * postwar + treat_dist_intensity2 * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades15)
coeftest(model_trades15, vcov. = vcovDC(model_trades15, type = "HC0"))

model_trades16 <- plm(log(trades_count+1) ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades16)
coeftest(model_trades16, vcov. = vcovDC(model_trades16, type = "HC0"))

model_trades17 <- plm(log(trades_count+1) ~ nbr_1_or_2 * postwar + treat_dist_intensity * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades17)
coeftest(model_trades17, vcov. = vcovDC(model_trades17, type = "HC0"))


################################################################################
# --- LATEX TABLES ---
################################################################################


#============================================================
#========== 0. Helper Objects (panels & labels) =========
#============================================================

### Panels for MODELS 1–6
panel_qspread_1_6 <- list(model_qspread01,model_qspread02,model_qspread03,
                          model_qspread04,model_qspread05,model_qspread06)

panel_espread_1_6 <- list(model_espread01,model_espread02,model_espread03,
                          model_espread04,model_espread05,model_espread06)

panel_dvol_1_6    <- list(model_dvolume01,model_dvolume02,model_dvolume03,
                          model_dvolume04,model_dvolume05,model_dvolume06)

panel_trades_1_6  <- list(model_trades01,model_trades02,model_trades03,
                          model_trades04,model_trades05,model_trades06)


### Panels for MODELS 12–17 (Distance × War interactions)
panel_qspread_12_17 <- list(model_qspread12,model_qspread13,model_qspread14,
                            model_qspread15,model_qspread16,model_qspread17)

panel_espread_12_17 <- list(model_espread12,model_espread13,model_espread14,
                            model_espread15,model_espread16,model_espread17)

panel_dvol_12_17    <- list(model_dvolume12,model_dvolume13,model_dvolume14,
                            model_dvolume15,model_dvolume16,model_dvolume17)

panel_trades_12_17  <- list(model_trades12,model_trades13,model_trades14,
                            model_trades15,model_trades16,model_trades17)


### Panels for MODELS 11–18 (LogReturn × War)
panel_return <- list(
  qspread = list(model_qspread11, model_qspread18),
  espread = list(model_espread11, model_espread18),
  dvolume = list(model_dvolume11, model_dvolume18),
  trades  = list(model_trades11, model_trades18)
)

### Panels for MODELS 7–8 (Distance × War)
panel_distance <- list(
  qspread = list(model_qspread07, model_qspread08),
  espread = list(model_espread07, model_espread08),
  dvolume = list(model_dvolume07, model_dvolume08),
  trades  = list(model_trades07, model_trades08)
)

### Panels for MODELS 9–10 (Volatility × War)
panel_volatility <- list(
  qspread = list(model_qspread09, model_qspread10),
  espread = list(model_espread09, model_espread10),
  dvolume = list(model_dvolume09, model_dvolume10),
  trades  = list(model_trades09, model_trades10)
)


### Labels
treat_terms_main <- c("nbr_1:postwar","nbr_2:postwar","nbr_1_or_2:postwar")
treat_labels_main <- c("$Neighbour_1 \\times War$",
                       "$Neighbour_2 \\times War$",
                       "$Neighbour_{1-2} \\times War$")

treat_terms_dist <- c("treat_dist_intensity1:postwar",
                      "treat_dist_intensity2:postwar",
                      "treat_dist_intensity:postwar")

treat_labels_dist <- c("$Neighbour_1 \\times Distance \\times War$",
                       "$Neighbour_2 \\times Distance \\times War$",
                       "$Neighbour_{1-2} \\times Distance \\times War$")

controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels <- c("Market Value (log)","Price (inverse)","No. Quotes (log)")


#============================================================
#====== 1. MAIN TABLES: MODELS 1–6 (All y variables) =====
#============================================================

make_table_1_6 <- function(filename, panelA, panelB, panelA_title, panelB_title){
  
  sink(filename)
  cat("\\begin{table}\\centering\n",
      "\\caption{Difference-in-differences regression results for Matched Sample: Neighbour Dummy. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
      "\\begin{adjustbox}{max width=\\textwidth}\n",
      "\\begin{tabular}{lrcccccc}\n",
      "\\toprule\n",
      "&&\\multicolumn{6}{c}{", panelA_title, "} \\\\ \\cmidrule{3-8}\n",
      " & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")
  
  ## PANEL A — Main Treatment Effects
  for (i in seq_along(treat_terms_main)){
    info <- lapply(panelA, get_info, treat_terms_main[i])
    cat(treat_labels_main[i], " & &",
        paste(sapply(info, function(x) fmt(x$coef)), collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info, function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }), collapse=" & "), "\\\\\n")
  }
  
  ## Controls
  for (j in seq_along(controls)){
    info <- lapply(panelA, get_info, controls[j])
    cat(ctrl_labels[j]," & &",
        paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info,function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Bottom stats PANEL A
  r2A <- sapply(panelA,get_r2); adjA <- pmax(0,sapply(panelA,get_adj), na.rm = FALSE); nA <- sapply(panelA,get_n)
  cat("\\midrule\n",
      "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
      "$R^{2}$ & &", paste(pct(r2A),collapse=" & "), " \\\\\n",
      "Adj.-$R^{2}$ & &", paste(pct(adjA),collapse=" & "), " \\\\\n",
      "No. Obs. & &", paste(fmt_num(nA),collapse=" & "), " \\\\\n",
      "\\midrule\n",
      "&&\\multicolumn{6}{c}{", panelB_title, "} \\\\ \\cmidrule{3-8}\n",
      " & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")
  
  ## PANEL B — Same logic
  for (i in seq_along(treat_terms_main)){
    info <- lapply(panelB, get_info, treat_terms_main[i])
    cat(treat_labels_main[i], " & &",
        paste(sapply(info, function(x) fmt(x$coef)), collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info, function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }), collapse=" & "), "\\\\\n")
  }
  
  ## Controls PANEL B
  for (j in seq_along(controls)){
    info <- lapply(panelB, get_info, controls[j])
    cat(ctrl_labels[j]," & &",
        paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info,function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Bottom stats PANEL B
  r2B <- sapply(panelB,get_r2); adjB <- pmax(0,sapply(panelB,get_adj), na.rm = FALSE); nB <- sapply(panelB,get_n)
  cat("\\midrule\n",
      "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
      "$R^{2}$ & &", paste(pct(r2B),collapse=" & "), " \\\\\n",
      "Adj.-$R^{2}$ & &", paste(pct(adjB),collapse=" & "), " \\\\\n",
      "No. Obs. & &", paste(fmt_num(nB),collapse=" & "), " \\\\\n",
      "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
  sink()
}


# Generate tables for models 1-6

make_table_1_6("liquidity_neighbour_psm.tex",
               panelA = panel_qspread_1_6,
               panelB = panel_espread_1_6,
               panelA_title = "Quoted Spread (\\%)",
               panelB_title = "Effective Spread (\\%)")

make_table_1_6("trading_activity_neighbour_psm.tex",
               panelA = panel_dvol_1_6,
               panelB = panel_trades_1_6,
               panelA_title = "Dollar Volume (log)",
               panelB_title = "Number of Trades (log)")


#============================================================
#  === 2. DISTANCE × WAR TABLES (MODELS 12–17) — same format
#============================================================

make_table_12_17 <- function(filename, panelA, panelB, panelA_title, panelB_title){
  
  sink(filename)
  cat("\\begin{table}\\centering\n",
      "\\caption{Difference-in-differences regression results for Matched Sample: Neighbour Distance Intensity. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
      "\\begin{adjustbox}{max width=\\textwidth}\n",
      "\\begin{tabular}{lrcccccc}\n",
      "\\toprule\n",
      "&&\\multicolumn{6}{c}{", panelA_title, "} \\\\ \\cmidrule{3-8}\n",
      " & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")
  
  ## PANEL A
  for (i in seq_along(treat_terms_dist)){
    info <- lapply(panelA, get_info, treat_terms_dist[i])
    cat(treat_labels_dist[i], " & &",
        paste(sapply(info, function(x) fmt(x$coef)), collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info, function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }), collapse=" & "), "\\\\\n")
  }
  
  ## Controls PANEL A
  for (j in seq_along(controls)){
    info <- lapply(panelA, get_info, controls[j])
    cat(ctrl_labels[j]," & &",
        paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info,function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Bottom stats PANEL A
  r2A <- sapply(panelA,get_r2); adjA <- pmax(0,sapply(panelA,get_adj), na.rm = FALSE); nA <- sapply(panelA,get_n)
  cat("\\midrule\n",
      "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
      "$R^{2}$ & &", paste(pct(r2A),collapse=" & "), " \\\\\n",
      "Adj.-$R^{2}$ & &", paste(pct(adjA),collapse=" & "), " \\\\\n",
      "No. Obs. & &", paste(fmt_num(nA),collapse=" & "), " \\\\\n",
      "\\midrule\n",
      "&&\\multicolumn{6}{c}{", panelB_title, "} \\\\ \\cmidrule{3-8}\n",
      " & &(1)&(2)&(3)&(4)&(5)&(6) \\\\\n\\midrule\n")
  
  ## PANEL B
  for (i in seq_along(treat_terms_dist)){
    info <- lapply(panelB, get_info, treat_terms_dist[i])
    cat(treat_labels_dist[i], " & &",
        paste(sapply(info, function(x) fmt(x$coef)), collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info, function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Controls PANEL B
  for (j in seq_along(controls)){
    info <- lapply(panelB, get_info, controls[j])
    cat(ctrl_labels[j]," & &",
        paste(sapply(info,function(x) ifelse(is.na(x$coef),"",fmt(x$coef))),collapse=" & "), "\\\\\n")
    cat(" & &",
        paste(sapply(info,function(x){
          if (is.na(x$coef)) "" else paste0("(",fmt_se(x$se),")",star(x$p))
        }),collapse=" & "), "\\\\\n")
  }
  
  ## Bottom stats PANEL B
  r2B <- sapply(panelB,get_r2); adjB <- pmax(0,sapply(panelB,get_adj), na.rm = FALSE); nB <- sapply(panelB,get_n)
  cat("\\midrule\n",
      "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
      "$R^{2}$ & &", paste(pct(r2B),collapse=" & "), " \\\\\n",
      "Adj.-$R^{2}$ & &", paste(pct(adjB),collapse=" & "), " \\\\\n",
      "No. Obs. & &", paste(fmt_num(nB),collapse=" & "), " \\\\\n",
      "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
  sink()
}

make_table_12_17("liquidity_intensity_psm.tex",
                 panelA = panel_qspread_12_17,
                 panelB = panel_espread_12_17,
                 panelA_title = "Quoted Spread (\\%)",
                 panelB_title = "Effective Spread (\\%)")

make_table_12_17("trading_activity_intensity_psm.tex",
                 panelA = panel_dvol_12_17,
                 panelB = panel_trades_12_17,
                 panelA_title = "Dollar Volume (log)",
                 panelB_title = "Number of Trades (log)")


#============================================================
#====== 3. DISTANCE AND VOLATILITY MODELS (Models 7–8) ======
#============================================================

make_2model_multiy_table <- function(filename, 
                                     panels,           # list(qspread=list(m1,m2), espread=list(m1,m2), ...)
                                     treat_term,       # e.g. "treat_ukr_inv:postwar"
                                     treat_label,      # LaTeX label
                                     table_caption){
  
  sink(filename)
  
  cat("\\begin{table}\\centering\n",
      "\\caption{", table_caption, "}\n",
      "\\begin{adjustbox}{max width=\\textwidth}\n",
      "\\begin{tabular}{lccccccccccc}\n",
      "\\toprule\n",
      " & \\multicolumn{2}{c}{QSpread (\\%)} & & \\multicolumn{2}{c}{ESpread (\\%)} & &",
      " \\multicolumn{2}{c}{DollarVol (log)} & & \\multicolumn{2}{c}{No. Trades (log)} \\\\ \\cmidrule{2-3}\\cmidrule{5-6}\\cmidrule{8-9}\\cmidrule{11-12} \n",
      " & (1) & (2) & & (3) & (4) & & (5) & (6) & & (7) & (8) \\\\\n",
      "\\midrule\n")
  
  ## --------------- TREATMENT ROW ----------------
  info_q  <- lapply(panels$qspread, get_info,  treat_term)
  info_e  <- lapply(panels$espread, get_info,  treat_term)
  info_d  <- lapply(panels$dvolume, get_info,  treat_term)
  info_t  <- lapply(panels$trades,  get_info,  treat_term)
  
  cat(treat_label, " & ",
      fmt(info_q[[1]]$coef), " & ", fmt(info_q[[2]]$coef), " & & ",
      fmt(info_e[[1]]$coef), " & ", fmt(info_e[[2]]$coef), " & & ",
      fmt(info_d[[1]]$coef), " & ", fmt(info_d[[2]]$coef), " & & ",
      fmt(info_t[[1]]$coef), " & ", fmt(info_t[[2]]$coef), "\\\\\n")
  
  cat(" & ",
      paste0("(",fmt_se(info_q[[1]]$se),")",star(info_q[[1]]$p)), " & ",
      paste0("(",fmt_se(info_q[[2]]$se),")",star(info_q[[2]]$p)), " & & ",
      paste0("(",fmt_se(info_e[[1]]$se),")",star(info_e[[1]]$p)), " & ",
      paste0("(",fmt_se(info_e[[2]]$se),")",star(info_e[[2]]$p)), " & & ",
      paste0("(",fmt_se(info_d[[1]]$se),")",star(info_d[[1]]$p)), " & ",
      paste0("(",fmt_se(info_d[[2]]$se),")",star(info_d[[2]]$p)), " & & ",
      paste0("(",fmt_se(info_t[[1]]$se),")",star(info_t[[1]]$p)), " & ",
      paste0("(",fmt_se(info_t[[2]]$se),")",star(info_t[[2]]$p)), "\\\\\n")
  
  ## --------------- CONTROLS ----------------
  for (j in seq_along(controls)) {
    ctrl <- controls[j]
    clab <- ctrl_labels[j]
    
    info_q <- lapply(panels$qspread, get_info, ctrl)
    info_e <- lapply(panels$espread, get_info, ctrl)
    info_d <- lapply(panels$dvolume, get_info, ctrl)
    info_t <- lapply(panels$trades,  get_info, ctrl)
    
    cat(clab, " & ",
        ifelse(is.na(info_q[[1]]$coef),"",fmt(info_q[[1]]$coef)), " & ",
        ifelse(is.na(info_q[[2]]$coef),"",fmt(info_q[[2]]$coef)), " & & ",
        ifelse(is.na(info_e[[1]]$coef),"",fmt(info_e[[1]]$coef)), " & ",
        ifelse(is.na(info_e[[2]]$coef),"",fmt(info_e[[2]]$coef)), " & & ",
        ifelse(is.na(info_d[[1]]$coef),"",fmt(info_d[[1]]$coef)), " & ",
        ifelse(is.na(info_d[[2]]$coef),"",fmt(info_d[[2]]$coef)), " & & ",
        ifelse(is.na(info_t[[1]]$coef),"",fmt(info_t[[1]]$coef)), " & ",
        ifelse(is.na(info_t[[2]]$coef),"",fmt(info_t[[2]]$coef)),
        "\\\\\n")
    
    cat(" & ",
        ifelse(is.na(info_q[[1]]$coef),"",paste0("(",fmt_se(info_q[[1]]$se),")",star(info_q[[1]]$p))), " & ",
        ifelse(is.na(info_q[[2]]$coef),"",paste0("(",fmt_se(info_q[[2]]$se),")",star(info_q[[2]]$p))), " & & ",
        ifelse(is.na(info_e[[1]]$coef),"",paste0("(",fmt_se(info_e[[1]]$se),")",star(info_e[[1]]$p))), " & ",
        ifelse(is.na(info_e[[2]]$coef),"",paste0("(",fmt_se(info_e[[2]]$se),")",star(info_e[[2]]$p))), " & & ",
        ifelse(is.na(info_d[[1]]$coef),"",paste0("(",fmt_se(info_d[[1]]$se),")",star(info_d[[1]]$p))), " & ",
        ifelse(is.na(info_d[[2]]$coef),"",paste0("(",fmt_se(info_d[[2]]$se),")",star(info_d[[2]]$p))), " & & ",
        ifelse(is.na(info_t[[1]]$coef),"",paste0("(",fmt_se(info_t[[1]]$se),")",star(info_t[[1]]$p))), " & ",
        ifelse(is.na(info_t[[2]]$coef),"",paste0("(",fmt_se(info_t[[2]]$se),")",star(info_t[[2]]$p))),
        "\\\\\n")
  }
  
  ## --------------- R2, Adj R2, N ----------------
  
  # R2 by outcome
  r2_q <- sapply(panels$qspread, get_r2)
  r2_e <- sapply(panels$espread, get_r2)
  r2_d <- sapply(panels$dvolume, get_r2)
  r2_t <- sapply(panels$trades,  get_r2)
  
  # Adj-R2 by outcome (truncated at zero)
  adj_q <- pmax(0, sapply(panels$qspread, get_adj))
  adj_e <- pmax(0, sapply(panels$espread, get_adj))
  adj_d <- pmax(0, sapply(panels$dvolume, get_adj))
  adj_t <- pmax(0, sapply(panels$trades,  get_adj))
  
  # No. of observations
  n_q <- sapply(panels$qspread, get_n)
  n_e <- sapply(panels$espread, get_n)
  n_d <- sapply(panels$dvolume, get_n)
  n_t <- sapply(panels$trades,  get_n)
  
  # pretty formatting for commas in N
  fmt_comma <- function(x) format(x, big.mark = ",", scientific = FALSE)
  
  cat("\\midrule\n",
      "TWFEs & Yes & Yes & & Yes & Yes & & Yes & Yes & & Yes & Yes \\\\\n",
      "$R^{2}$ & ",
      pct(r2_q[1]), " & ", pct(r2_q[2]), " & & ",
      pct(r2_e[1]), " & ", pct(r2_e[2]), " & & ",
      pct(r2_d[1]), " & ", pct(r2_d[2]), " & & ",
      pct(r2_t[1]), " & ", pct(r2_t[2]), " \\\\\n",
      "Adj-$R^{2}$ & ",
      pct(adj_q[1]), " & ", pct(adj_q[2]), " & & ",
      pct(adj_e[1]), " & ", pct(adj_e[2]), " & & ",
      pct(adj_d[1]), " & ", pct(adj_d[2]), " & & ",
      pct(adj_t[1]), " & ", pct(adj_t[2]), " \\\\\n",
      "No. Obs. & ",
      fmt_comma(n_q[1]), " & ", fmt_comma(n_q[2]), " & & ",
      fmt_comma(n_e[1]), " & ", fmt_comma(n_e[2]), " & & ",
      fmt_comma(n_d[1]), " & ", fmt_comma(n_d[2]), " & & ",
      fmt_comma(n_t[1]), " & ", fmt_comma(n_t[2]), " \\\\\n",
      "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
  
  sink()
}


make_2model_multiy_table(
  filename = "return_models_psm.tex",
  panels = panel_return,
  treat_term = "treat_economic",
  treat_label = "$War Log Return$",
  table_caption = "Difference-in-differences regression results for Matched Sample: Economic Risk. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance."
)

make_2model_multiy_table(
  filename = "distance_models_psm.tex",
  panels = panel_distance,
  treat_term = "treat_ukr_inv:postwar",
  treat_label = "$Distance \\times War$",
  table_caption = "Difference-in-differences regression results for Matched Sample: Spatial Distance. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance."
)

make_2model_multiy_table(
  filename = "volatility_models_psm.tex",
  panels = panel_volatility,
  treat_term = "intraday_vol_mean:postwar",
  treat_label = "$Volatility_{OC} \\times War$",
  table_caption = "Difference-in-differences regression results for Matched Sample: Volatility. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance."
)






