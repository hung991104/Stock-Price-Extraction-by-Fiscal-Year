# 3_Plot Financial Data from Annual Reports

# Load Packages
# Package names
packages <- c("quantmod", "tidyverse", "lubridate")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Set theme of plots
theme_set(theme_bw())


# Read Data Summary of specific ticker ------------------------------------
ticker = "ROKU"
ReadPath = paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker, "/data/DataSummary.csv")
DataSummary = read.csv(ReadPath, header = TRUE)

# filter out the rows with sum of values = 0 => assume that it is not useful for that company
DataSummary = DataSummary %>%
  mutate(Total = select(., starts_with("X")) %>%
           rowSums()) %>%
  #filter(Total != 0) %>%
  select(-Sheet, -Metric,-Total) %>%
  rename(Metric = Name) %>%
  pivot_longer(c(-1), names_to = "year", values_to = "value") %>%
  pivot_wider(names_from = "Metric", values_from = "value") %>%
  mutate(year = as.integer(gsub("^X", "", year))) %>%
  arrange(year) %>% 
  janitor::clean_names()


# Plot Important Metrics --------------------------------------------------
# Add columns for gross margin, operating margin
DataSummary_Margin = DataSummary %>% 
  mutate(Gross_Margin = gross_profit/revenue,
         Operating_Margin = operating_income/revenue,
         Net_Margin = net_income/revenue,
         Payback_Ratio = (-1) * (repurchase + dividends)/net_income,
         date = lubridate::ymd(paste0(year, "1231")))

# Plot multiple margins
DataSummary_Margin %>%
  select(date, ends_with("Margin")) %>%
  pivot_longer(c(-1), names_to = "margin", values_to = "value") %>%
  ggplot(aes(date, value, group = margin, color = margin)) +
  geom_line() +
  scale_y_continuous(limits = c(-0.8, 1), breaks = seq(-0.8, 1, by = 0.1), labels = scales::percent) +
  labs(y = "Margin (%)", title = "Margins by fiscal years") +
  theme(plot.title = element_text(hjust = 0.5))     # title in the middle

# Net margin is more volatile as it can be largely affected by non-operating income/expense
# Better use Net Operating Profit After Tax/ Normalized earning for forcast


# Regression on Revenue Growth --------------------------------------------
DataSummary %>%
  select(year, revenue) %>%
  ggplot() + 
  geom_point(aes(year, revenue)) +
  geom_line(aes(year, revenue), data = DataSummary) +
  scale_x_continuous(breaks = seq(2011, 2020, by = 1)) +
  labs(x = "fiscal year", title = "Revenue From 2011 to 2020") +
  theme(plot.title = element_text(hjust = 0.5))     # title in the middle

# fit log(revenue) against year
Revenue_ln_lmfit = lm(log(revenue) ~ year, data = DataSummary)
summary(Revenue_ln_lmfit)
plot(DataSummary$year, log(DataSummary$revenue))
abline(Revenue_ln_lmfit)

# Estimated Revenue growth = coefficient of year
coefficients(Revenue_ln_lmfit)
print(paste0("Estimated Revenue Growth = ", round(100 * coefficients(Revenue_ln_lmfit)[[2]], 2), "%"))
summary(Revenue_ln_lmfit)$adj.r.squared
print(paste0("Adj. R squrared = ", round(summary(Revenue_ln_lmfit)$adj.r.squared,4)))

# Plot summary of linear regression fit
par(mfrow = c(2,2))
plot(Revenue_ln_lmfit)   
dev.off()    # reset the graph setting

# Note that the growth rate in the past is not a good indicator of growth in the future!



# Beneish-M-Score - detect financial fraud/ earning manipulation ---------------------------------------------------------

Beneish_Data = DataSummary %>%
  select(year, revenue, gross_profit, receivables, total_current_assets, 
         net_ppe, d_a, amortization_for_acquired_ia, sg_a, 
         total_assets, lt_debt, total_current_liabilities, net_income, operating_cash_flow) %>%
  mutate(gross_margin = gross_profit / revenue,
         day_sales_in_receivables = receivables / revenue * 365,
         sga_margin = sg_a / revenue,
         depreciation = d_a - amortization_for_acquired_ia,
         depreciation_prop = depreciation / (net_ppe + depreciation),
         leverage_prop = (total_current_liabilities + lt_debt)/total_assets,
         cur_asset_ppe_prop = (total_current_assets + net_ppe)/total_assets)

# Compute relevant indicators
Beneish_Score = Beneish_Data %>% 
  mutate(DSRI = day_sales_in_receivables / lag(day_sales_in_receivables, 1),
         GMI = lag(gross_margin, 1)/ gross_margin,
         AQI = (1 - cur_asset_ppe_prop)/(1 - lag(cur_asset_ppe_prop, 1)),
         SGI = revenue/ lag(revenue, 1),
         DEPI = lag(depreciation_prop,1)/depreciation_prop,
         SGAI = sga_margin / lag(sga_margin, 1),
         LVGI = leverage_prop / lag(leverage_prop, 1),
         TATA = (net_income - operating_cash_flow)/total_assets) %>%
  # remove first column 
  .[-1, ] %>%
  mutate(M_Score = -4.84 + 0.92*DSRI + 0.528*GMI + 0.404*AQI + 0.892*SGI + 0.115*DEPI - 0.172*SGAI + 4.679*TATA - 0.327*LVGI)

# Plot M-Score's trend
Beneish_Score %>%
  select(year, M_Score)
y_limit = c(round(min(Beneish_Score$M_Score) - 0.3, 1), round(max(Beneish_Score$M_Score) + 0.3, 1))

Beneish_Score %>%
  ggplot() +
  geom_point(aes(year, M_Score)) +
  geom_line(aes(year, M_Score), data = Beneish_Score) +
  geom_hline(yintercept = -1.78, color = "red", size = 1.2) +
  geom_text(aes(min(year), -1.78, label = -1.78, vjust = -0.75)) + 
  geom_hline(yintercept = -2.22, color = "orange", size = 1.1, linetype = "dashed") + 
  geom_text(aes(min(year), -2.22, label = -2.22, vjust = -0.75)) + 
  scale_x_continuous(breaks = seq(min(Beneish_Score$year), max(Beneish_Score$year), by = 1)) + 
  scale_y_continuous(limits = y_limit, breaks = seq(y_limit[1], y_limit[2], by = 0.5)) +
  labs(x = "Fiscal Year", y = "M-Score", title = paste0(ticker, " Beneish M-Score Trend by Fiscal Year")) +
  theme(plot.title = element_text(hjust = 0.5))     # title in the middle

# Score < -2.22: Not manipulator
# 2.22 <= Score < -1.78: Possible manipulator
# Score >= 1.78: Likely manipulator

# Save the M-Score's Plot
SavePlotPath = paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker, "/data/", ticker,"_Beneish_M_Score.jpg")
ggsave(SavePlotPath)

# Plot individual parameter's trend
Plot_Beneish_Parameter <- function(df, x_var, y_var) {
  
  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) + 
    geom_point() + 
    geom_line() +
    labs(x = x_var, y = y_var) +
    theme_classic(base_size = 12) + 
    scale_y_continuous(limits = c(min(df[[y_var]])-0.05, max(df[[y_var]]) + 0.05)) +
    scale_x_continuous(breaks = seq(min(df[[x_var]]), max(df[[x_var]]), by = 1))
}

Plot_Beneish_Parameter(Beneish_Score, "year", "GMI")
parameters = c("DSRI", "GMI", "AQI", "SGI", "DEPI", "SGAI","LVGI", "TATA")
i = 8
Plot_Beneish_Parameter(Beneish_Score, "year", parameters[i])


