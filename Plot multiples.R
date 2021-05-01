# Read and Plot Ratios --------------------------------------------------------


if(!require("quantmod")){
  install.packages("quantmod")
  library(quantmod)
}
library(tidyverse)
library(lubridate)
theme_set(theme_bw())

ticker <- "NOC"

ReadPath <- paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker, "/data/Ratios.csv")
Ratios <- read.csv(ReadPath, header = TRUE)
Ratios 

Ratio_long <- Ratios %>% 
  janitor::clean_names() %>%
  select(metric, starts_with("x")) %>% 
  pivot_longer(c(-1), names_to = "year", values_to = "value") 

Ratio_wide <- Ratio_wide%>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(year = gsub("^x", "", year)) %>%
  arrange(year)

str(Ratio_wide) 

Annual_ratio <- Ratio_wide %>%
  #mutate(year = year(as_date(year, format = "%Y"))) %>% str()
  mutate(year = ymd(paste(year, "12","31", sep = "-")))

str(Annual_ratio)

# select and plot EV/NOPAT trend 
Annual_EV_NOPAT <- Annual_ratio %>% 
  select(year, contains("NOPAT")) # %>% janitor::clean_names()

Annual_EV_NOPAT %>% 
  pivot_longer(c(-1), names_to = "metric", values_to = "value") %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = metric)) +
  labs(title = "EV/NOPAT Trend", x = "Fiscal Year", y = "EV/NOPAT") +
  scale_y_continuous(limits = c(0, 25)) + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))     # title in the middle

# select and plot EV/FCF trend 
Annual_EV_FCF <- Annual_ratio %>% 
  select(year, contains("FCF")) # %>% janitor::clean_names()

Annual_EV_FCF %>% 
  pivot_longer(c(-1), names_to = "metric", values_to = "value") %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = metric)) +
  labs(title = "EV/FCF Trend", x = "Fiscal Year", y = "EV/FCF") +
  scale_y_continuous(limits = c(0, 40)) + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))     # title in the middle




# # Read and Plot Ratios --------------------------------------------------------
# library(readxl)      # read ratios
# ReadPath <- paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker, "/data/Ratios.xlsx")
# Ratios <- read_excel(ReadPath, sheet = "DG")
# Ratios 
# Ratio_wide <- Ratios %>% 
#   select(Metric, contains("20")) %>% 
#   pivot_longer(c(-1), names_to = "Year", values_to = "Value") 
# Ratio_long <- Ratio_wide%>%
#   pivot_wider(names_from = "Metric", values_from = "Value") %>%
#   janitor::clean_names() %>% arrange(year)
# 
# # select ev related metrics
# Ratio_wide %>% filter(grepl("EV", Metric))