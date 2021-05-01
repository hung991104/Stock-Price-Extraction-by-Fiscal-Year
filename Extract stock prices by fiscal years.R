# Extract stock prices by fiscal years

Sys.setlocale(locale = "English")

library(quantmod)
library(tidyverse)
library(lubridate)
library(openxlsx)    # save output as xlsx

FiscalPriceRange = function(ticker, start, end, startFiscal){
  
  # Package names
  packages <- c("quantmod", "tidyverse", "lubridate","openxlsx")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  
  # Print the result
  print(paste0("ticker: ", ticker))
  print(paste("Extracting Period: ", start, "to", end, sep = " "))
  print(paste("Fiscal Year:", as.character(startFiscal), "to", as.character(startFiscal + 10), sep = " "))
  
  start = ymd(start)
  end = ymd(end)
  
  # Extract Price Records
  stock <- getSymbols(ticker, src = "yahoo", from = start, to = end, auto.assign = FALSE)
  stock_df <- data.frame(Date = index(stock), coredata(stock)) %>%
    setNames(c("Date","Open", "High", "Low", "Close", "Volume", "Adjusted"))
  
  # Check whether fiscal year matches date of price records
  # print(interval(start, end) / years(1))
  years = c(startFiscal, (startFiscal + interval(start, end) / years(1)))
  
  if(abs(startFiscal - year(stock_df[1,1])) > 1){
    print(paste("StartFiscal:", startFiscal, ", First date:", stock_df[1,1] ,sep = " "))
    warning("StartFiscal (year) doesn't match the first price record.")
  }
  
  # Classify data by fiscal year
  stock_df <- stock_df %>%
    mutate(fiscal = cut(Date,
                        breaks = seq(start, end + years(1), by = "year"),
                        labels = 2011:2021))
  
  # Get High and Low of each fiscal year
  stock_df_High_Low <- stock_df %>% 
    mutate(fiscal = as.integer(levels(fiscal))[fiscal]) %>%
    select(fiscal, High, Low, Date) %>%
    group_by(fiscal) %>%
    summarise(Fiscal_High = max(High), Fiscal_Low = min(Low)) 
  
  # Change the format of data to facilitate excel work
  High_Low_final <- stock_df_High_Low %>% 
    pivot_longer(c(Fiscal_High, Fiscal_Low), names_to = "Range", values_to = "Value") %>% 
    arrange(desc(fiscal))  %>% pivot_wider(names_from = fiscal, values_from = "Value")
}

# Get price high/low of ticker
ticker = "T"
PriceHighLow <- FiscalPriceRange(ticker, "2011-01-01", "2021-04-30", startFiscal = 2011)

# save the data in "Price.xlsx"
outputdata = set_names(list(ticker = PriceHighLow), ticker)
outputfile = paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker,"/data/Price.xlsx")
write.xlsx(outputdata, file = outputfile)

