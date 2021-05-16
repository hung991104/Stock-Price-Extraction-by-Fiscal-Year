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
DataSummary %>% 
  mutate(Gross_Margin = gross_profit/revenue,
         Operating_Margin = operating_income/revenue,
         Net_Margin = net_income/revenue,
         Payback_Ratio = (-1) * (repurchase + dividends)/net_income)


