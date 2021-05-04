# Transform Financial Data to Ratios 

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


# Read financial data of specific ticker ----------------------------------

ticker <- "MSFT"
ReadPath <- paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker, "/data/DataForRatio.csv")
Ratios <- read.csv(ReadPath, header = TRUE)

Ratios_wide <- Ratios %>%
  select(Metric, starts_with("X")) %>% 
  pivot_longer(c(-1), names_to = "year", values_to = "value") %>%
  pivot_wider(names_from = "Metric", values_from = "value") %>%
  mutate(year = gsub("^X", "", year)) %>%
  arrange(year)  %>% 
  janitor::clean_names() %>%
  mutate(net_debt = current_debt + lt_debt + operating_lease_liabilities - cash,
         EBITA = operating_income + amortization_for_acquired_ia)

FinData_Annual <- Ratios_wide %>%
  select(year, share_number, net_debt, net_income, EBITA) %>%
  mutate(year = as.integer(year)) 

FinData_Annual <- FinData_Annual%>%
  add_row(year = FinData_Annual[[nrow(FinData_Annual), 1]]+1) %>%
  mutate(LY_net_debt = lag(net_debt, 1)) %>%
  select(year, LY_net_debt, everything()) %>%
  .[-1, ]     # remove first column 

print(FinData_Annual[c(1, nrow(FinData_Annual)),1])



# Extract Price Records ---------------------------------------------------

start = ymd("2011-07-01")   
end = Sys.Date() - 1    # Yesterday

# include the latest year
fiscal_years = c(FinData_Annual[[1,1]]:FinData_Annual[[nrow(FinData_Annual), 1]]) 

# Classify Price Records by fiscal year
stock <- getSymbols(ticker, src = "yahoo", from = start, to = end, auto.assign = FALSE)
stock_df <- data.frame(Date = index(stock), coredata(stock)) %>%
  setNames(c("Date","Open", "High", "Low", "Close", "Volume", "Adjusted")) %>%
  select(Date, Close) %>%
  mutate(fiscal = cut(Date,
                      breaks = seq(start, end + years(1), by = "year"),
                      labels = fiscal_years)) %>%
  mutate(fiscal = as.integer(levels(fiscal))[fiscal])    # turn factor to integer


# Calculate daily enterprise value
daily_ev <- stock_df %>% 
  left_join(FinData_Annual, by = c("fiscal" = "year")) %>% 
  # fill latest year's share number with latest non NA value
  fill(share_number, .direction = "down") %>%
  mutate(ev = Close * share_number + LY_net_debt) %>%
  mutate(EV_EBITA = ev / EBITA)

# Plot EV/EBITA
daily_ev %>% 
  filter(!is.na(EV_EBITA)) %>%
  ggplot(aes(Date, EV_EBITA)) +
  geom_line()

# Get High and low of EV/EBITA each fiscal year
Annual_EV_EBITA <- daily_ev %>%
  group_by(fiscal) %>%
  summarise(Ratio_High = max(EV_EBITA), Ratio_Low = min(EV_EBITA)) %>%
  filter(!is.na(Ratio_High) & !is.na(Ratio_Low))    # no EBITA in latest year => NA => filter out

Annual_EV_EBITA %>% 
  pivot_longer(c(-1), names_to = "metric", values_to = "value") %>%
  ggplot() +
  geom_line(aes(x = fiscal, y = value, color = metric)) +
  labs(title = "EV/EBITA Trend", x = "Fiscal Year", y = "EV/EBITA") +
  scale_y_continuous(limits = c(0, 30)) + 
  scale_x_continuous(limits = c(fiscal_years[1], fiscal_years[length(fiscal_years)])) + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))     # title in the middle


# Obsolete ----------------------------------------------------------------
#Annual_ratio <- Ratios_wide %>%
#  mutate(year = ymd(paste(year, "06","30", sep = "-"))) 


# Interpolation of net debt 
Year_Net_Debt <- Ratios_wide %>%
  mutate(year = ymd(paste(year, "06","30", sep = "-")))  %>% 
  select(year, net_debt, share_number)

start = ymd("2010-07-01")
#end = ymd("2021-04-30")
end = Sys.Date() - 1

day_range <- seq(start, end, by = "day")
daily_netdebt <- approx(Year_Net_Debt$year, Year_Net_Debt$net_debt, n = length(day_range))
daily_sharenum <- approx(Year_Net_Debt$year, Year_Net_Debt$share_number, n = length(day_range))
Daily_findata <- data.frame(Date = day_range, net_debt = daily_netdebt$y, share_number = daily_sharenum$y)

