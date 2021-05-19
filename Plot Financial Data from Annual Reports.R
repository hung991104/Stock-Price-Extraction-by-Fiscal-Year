# Plot Financial Data from Annual Reports

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
ticker = "CCI"
ReadPath = paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker, "/data/DataSummary.csv")
DataSummary = read.csv(ReadPath, header = TRUE)

# filter out the rows with sum of values = 0 => assume that it is not useful for that company
DataSummary = DataSummary %>%
  mutate(Total = select(., starts_with("X")) %>%
           rowSums()) %>%
  filter(Total != 0) %>%
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
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(y = "Margin (%)", title = "Margins by fiscal years") +
  theme(plot.title = element_text(hjust = 0.5))     # title in the middle

# Net margin is more volatile as it can be largely affected by non-operating income/expense
# Better use Net Operating Profit After Tax/ Normalized earning for forcast


# Regression on Revenue Growth --------------------------------------------
DataSummary %>%
  select(year, revenue) %>%
  ggplot(aes(year, revenue)) + 
  geom_point() +
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
