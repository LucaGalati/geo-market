


###### previous code ########

################################################################################
# --- LATEX TABLES ---
################################################################################

# Panels: Quoted Spread and Effective Spread (12 specs each)
panelA <- list(model_qspread01,model_qspread02,model_qspread03,model_qspread04,
               model_qspread05,model_qspread06,model_qspread07,model_qspread08,
               model_qspread09,model_qspread10,model_qspread11,model_qspread12)

panelB <- list(model_espread01,model_espread02,model_espread03,model_espread04,
               model_espread05,model_espread06,model_espread07,model_espread08,
               model_espread09,model_espread10,model_espread11,model_espread12)

# Terms and labels
treat_terms <- c("nbr_1:postwar","nbr_2:postwar","nbr_1_or_2:postwar",
                 "treat_ukr_inv:postwar","intraday_vol_mean:postwar",
                 "intraday_5m_vol_mean:postwar")
treat_labels<- c("$Neighbour_1 \\times War$","$Neighbour_2 \\times War$",
                 "$Neighbour_{1-2} \\times War$","$Distance \\times War$",
                 "$Volatility_{OC} \\times War$","$Volatility_{5m} \\times War$")
controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels<-c("Market Value (log)","Price (inverse)","No. Quotes (log)")

# Begin LaTeX table
sink("liquidity.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Overall Sample: Liquidity Measures. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lrcccccccccccc}\n",
    "\\toprule\n",
    "&&\\multicolumn{12}{c}{Quoted Spread (\\%)} \\\\ \\cmidrule{3-14}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10)&(11)&(12) \\\\\n\\midrule\n")

# ------------------------ Panel A ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelA, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) fmt(info[[j]]$coef)), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j){
        if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) ""
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }),collapse=" & "),
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
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2A),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjA),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nA),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "&&\\multicolumn{12}{c}{Effective Spread (\\%)} \\\\ \\cmidrule{3-14}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10)&(11)&(12) \\\\\n\\midrule\n")

# ------------------------ Panel B ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelB, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) fmt(info[[j]]$coef)), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j){
        if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) ""
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }),collapse=" & "),
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
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2B),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjB),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nB),collapse=" & ")," \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to liquidity.tex")


# Panels: Dollar Volume and Numebr of Trades (12 specs each)
panelA <- list(model_dvolume01,model_dvolume02,model_dvolume03,model_dvolume04,
               model_dvolume05,model_dvolume06,model_dvolume07,model_dvolume08,
               model_dvolume09,model_dvolume10,model_dvolume11,model_dvolume12)

panelB <- list(model_trades01,model_trades02,model_trades03,model_trades04,
               model_trades05,model_trades06,model_trades07,model_trades08,
               model_trades09,model_trades10,model_trades11,model_trades12)

# Terms and labels
treat_terms <- c("nbr_1:postwar","nbr_2:postwar","nbr_1_or_2:postwar",
                 "treat_ukr_inv:postwar","intraday_vol_mean:postwar",
                 "intraday_5m_vol_mean:postwar")
treat_labels<- c("$Neighbour_1 \\times War$","$Neighbour_2 \\times War$",
                 "$Neighbour_{1-2} \\times War$","$Distance \\times War$",
                 "$Volatility_{OC} \\times War$","$Volatility_{5m} \\times War$")
controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels<-c("Market Value (log)","Price (inverse)","No. Quotes (log)")

# Begin LaTeX table
sink("trading_activity.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Overall Sample: Trading Activity Measures. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lrcccccccccccc}\n",
    "\\toprule\n",
    "&&\\multicolumn{12}{c}{Dollar Volume (log)} \\\\ \\cmidrule{3-14}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10)&(11)&(12) \\\\\n\\midrule\n")

# ------------------------ Panel A ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelA, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) fmt(info[[j]]$coef)), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j){
        if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) ""
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }),collapse=" & "),
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
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2A),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjA),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nA),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "&&\\multicolumn{12}{c}{Number of Trades (log)} \\\\ \\cmidrule{3-14}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10)&(11)&(12) \\\\\n\\midrule\n")

# ------------------------ Panel B ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelB, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) fmt(info[[j]]$coef)), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j){
        if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) ""
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }),collapse=" & "),
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
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2B),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjB),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nB),collapse=" & ")," \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to trading_activity.tex")



################################################################################
# Baseline Models - Matched Sample
################################################################################


############################ Relative Quoted Spread ############################

#model_qspread00 <- plm(qspread_mean ~ postwar, 
#                      data = dfp2, model = "within", effect = "individual")
#summary(model_qspread00)
#coeftest(model_qspread00, vcov. = vcovHC(model_spread00, type = "HC0"))

#model_qspread000 <- plm(qspread_mean ~ postwar + log(mktval) + inv_price + log(quotes_count), 
#                      data = dfp2, model = "within", effect = "individual")
#summary(model_qspread000)
#coeftest(model_qspread000, vcov. = vcovHC(model_qspread000, type = "HC0"))

###

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

model_qspread11 <- plm(qspread_mean ~ intraday_5m_vol_mean * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread11)
coeftest(model_qspread11, vcov. = vcovDC(model_qspread11, type = "HC0"))

model_qspread12 <- plm(qspread_mean ~ intraday_5m_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_qspread12)
coeftest(model_qspread12, vcov. = vcovDC(model_qspread12, type = "HC0"))


############################### Effective Spread ############################### 

#model_espread00 <- plm(espread_mean ~ postwar, 
#                      data = dfp2, model = "within", effect = "individual")
#summary(model_espread00)
#coeftest(model_espread00, vcov. = vcovHC(model_espread00, type = "HC0"))

#model_espread000 <- plm(espread_mean ~ postwar + log(mktval) + inv_price + log(quotes_count), 
#                      data = dfp2, model = "within", effect = "individual")
#summary(model_espread000)
#coeftest(model_espread000, vcov. = vcovHC(model_espread000, type = "HC0"))

###

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

model_espread11 <- plm(espread_mean ~ intraday_5m_vol_mean * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread11)
coeftest(model_espread11, vcov. = vcovDC(model_espread11, type = "HC0"))

model_espread12 <- plm(espread_mean ~ intraday_5m_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_espread12)
coeftest(model_espread12, vcov. = vcovDC(model_espread12, type = "HC0"))


################################ Dollar Volumes ################################   

#model_volume00 <- plm(log(dollar_volume_sum+1) ~ postwar, 
#                      data = dfp2, model = "within", effect = "individual")
#summary(model_volume00)
#coeftest(model_volume00, vcov. = vcovHC(model_volume00, type = "HC0"))

#model_volume000 <- plm(log(dollar_volume_sum+1) ~ postwar + log(mktval) + inv_price  + log(quotes_count), 
#                      data = dfp2, model = "within", effect = "individual")
#summary(model_volume000)
#coeftest(model_volume000, vcov. = vcovHC(model_volume000, type = "HC0"))

###

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

model_dvolume11 <- plm(log(dollar_volume_sum+1) ~ intraday_5m_vol_mean * postwar, 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume11)
coeftest(model_dvolume11, vcov. = vcovDC(model_dvolume11, type = "HC0"))

model_dvolume12 <- plm(log(dollar_volume_sum+1) ~ intraday_5m_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                       data = dfp2, model = "within", effect = "twoways")
summary(model_dvolume12)
coeftest(model_dvolume12, vcov. = vcovDC(model_dvolume12, type = "HC0"))


############################### Number of Trades ###############################

#model_trades00 <- plm(log(trades_count+1) ~ postwar, 
#                      data = dfp2, model = "within", effect = "individual")
#summary(model_trades00)
#coeftest(model_trades00, vcov. = vcovHC(model_trades00, type = "HC0"))

#model_trades000 <- plm(log(trades_count+1) ~ postwar + log(mktval) + inv_price + log(quotes_count), 
#                      data = dfp2, model = "within", effect = "individual")
#summary(model_trades000)
#coeftest(model_trades000, vcov. = vcovHC(model_trades000, type = "HC0"))

###

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

model_trades11 <- plm(log(trades_count+1) ~ intraday_5m_vol_mean * postwar, 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades11)
coeftest(model_trades11, vcov. = vcovDC(model_trades11, type = "HC0"))

model_trades12 <- plm(log(trades_count+1) ~ intraday_5m_vol_mean * postwar + log(mktval) + inv_price + log(quotes_count), 
                      data = dfp2, model = "within", effect = "twoways")
summary(model_trades12)
coeftest(model_trades12, vcov. = vcovDC(model_trades12, type = "HC0"))


################################################################################
# --- LATEX TABLES ---
################################################################################

# Panels: Quoted Spread and Effective Spread (12 specs each)
panelA <- list(model_qspread01,model_qspread02,model_qspread03,model_qspread04,
               model_qspread05,model_qspread06,model_qspread07,model_qspread08,
               model_qspread09,model_qspread10,model_qspread11,model_qspread12)

panelB <- list(model_espread01,model_espread02,model_espread03,model_espread04,
               model_espread05,model_espread06,model_espread07,model_espread08,
               model_espread09,model_espread10,model_espread11,model_espread12)

# Terms and labels
treat_terms <- c("nbr_1:postwar","nbr_2:postwar","nbr_1_or_2:postwar",
                 "treat_ukr_inv:postwar","intraday_vol_mean:postwar",
                 "intraday_5m_vol_mean:postwar")
treat_labels<- c("$Neighbour_1 \\times War$","$Neighbour_2 \\times War$",
                 "$Neighbour_{1-2} \\times War$","$Distance \\times War$",
                 "$Volatility_{OC} \\times War$","$Volatility_{5m} \\times War$")
controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels<-c("Market Value (log)","Price (inverse)","No. Quotes (log)")

# Begin LaTeX table
sink("liquidity_psm.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Matched Sample: Liquidity Measures. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lrcccccccccccc}\n",
    "\\toprule\n",
    "&&\\multicolumn{12}{c}{Quoted Spread (\\%)} \\\\ \\cmidrule{3-14}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10)&(11)&(12) \\\\\n\\midrule\n")

# ------------------------ Panel A ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelA, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) fmt(info[[j]]$coef)), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j){
        if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) ""
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }),collapse=" & "),
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
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2A),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjA),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nA),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "&&\\multicolumn{12}{c}{Effective Spread (\\%)} \\\\ \\cmidrule{3-14}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10)&(11)&(12) \\\\\n\\midrule\n")

# ------------------------ Panel B ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelB, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) fmt(info[[j]]$coef)), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j){
        if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) ""
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }),collapse=" & "),
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
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2B),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjB),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nB),collapse=" & ")," \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to liquidity_psm.tex")


# Panels: Dollar Volume and Numebr of Trades (12 specs each)
panelA <- list(model_dvolume01,model_dvolume02,model_dvolume03,model_dvolume04,
               model_dvolume05,model_dvolume06,model_dvolume07,model_dvolume08,
               model_dvolume09,model_dvolume10,model_dvolume11,model_dvolume12)

panelB <- list(model_trades01,model_trades02,model_trades03,model_trades04,
               model_trades05,model_trades06,model_trades07,model_trades08,
               model_trades09,model_trades10,model_trades11,model_trades12)

# Terms and labels
treat_terms <- c("nbr_1:postwar","nbr_2:postwar","nbr_1_or_2:postwar",
                 "treat_ukr_inv:postwar","intraday_vol_mean:postwar",
                 "intraday_5m_vol_mean:postwar")
treat_labels<- c("$Neighbour_1 \\times War$","$Neighbour_2 \\times War$",
                 "$Neighbour_{1-2} \\times War$","$Distance \\times War$",
                 "$Volatility_{OC} \\times War$","$Volatility_{5m} \\times War$")
controls <- c("log(mktval)","inv_price","log(quotes_count)")
ctrl_labels<-c("Market Value (log)","Price (inverse)","No. Quotes (log)")

# Begin LaTeX table
sink("trading_activity_psm.tex")
cat("\\begin{table}\\centering\n",
    "\\caption{Difference-in-differences regression results for Matched Sample: Trading Activity Measures. Double-clustered robust SEs in parentheses. $^{***}$, $^{**}$, and $^{*}$ indicate 1\\%, 5\\%, 10\\% significance.}\n",
    "\\begin{adjustbox}{max width=\\textwidth}\n",
    "\\begin{tabular}{lrcccccccccccc}\n",
    "\\toprule\n",
    "&&\\multicolumn{12}{c}{Dollar Volume (log)} \\\\ \\cmidrule{3-14}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10)&(11)&(12) \\\\\n\\midrule\n")

# ------------------------ Panel A ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelA, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) fmt(info[[j]]$coef)), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j){
        if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) ""
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }),collapse=" & "),
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
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2A),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjA),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nA),collapse=" & ")," \\\\\n",
    "\\midrule\n",
    "&&\\multicolumn{12}{c}{Number of Trades (log)} \\\\ \\cmidrule{3-14}\n",
    " & &(1)&(2)&(3)&(4)&(5)&(6)&(7)&(8)&(9)&(10)&(11)&(12) \\\\\n\\midrule\n")

# ------------------------ Panel B ------------------------
for (i in seq_along(treat_terms)){
  info <- lapply(panelB, get_info, treat_terms[i])
  cat(treat_labels[i], " & &",
      paste(sapply(seq_along(info), function(j) fmt(info[[j]]$coef)), collapse=" & "),
      " \\\\\n")
  cat(" & &",
      paste(sapply(seq_along(info), function(j){
        if (is.na(info[[j]]$se) | is.na(info[[j]]$coef)) ""
        else paste0("(",fmt_se(info[[j]]$se),")",star(info[[j]]$p))
      }),collapse=" & "),
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
    "TWFEs & &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes &Yes \\\\\n",
    "$R^{2}$ & &",paste(pct(r2B),collapse=" & ")," \\\\\n",
    "Adj.-$R^{2}$ & &",paste(pct(adjB),collapse=" & ")," \\\\\n",
    "No. Obs. & &",paste(fmt_num(nB),collapse=" & ")," \\\\\n",
    "\\bottomrule\n\\end{tabular}\n\\end{adjustbox}\n\\end{table}\n")
sink()
message("✅ LaTeX table written to trading_activity_psm.tex")

