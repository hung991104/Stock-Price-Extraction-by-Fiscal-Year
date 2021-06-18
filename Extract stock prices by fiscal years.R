# Extract stock prices by fiscal years

Sys.setlocale(locale = "English")

Stock_Fiscal_Quarter = function(ticker, start, startFiscal){
  # Package names
  packages <- c("quantmod", "tidyverse", "lubridate","openxlsx")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  # convert start date to time
  if(class(start) == "character"){
    start = ymd(start)
  }
  end = Sys.Date() - 1
  
  # Check validity of input
  if(start > end){
    stop("Start date should be less than or equal to yesterday.")
  } else if(abs(startFiscal - year(ymd(start))) > 1){
    print(paste("StartFiscal:", startFiscal, ", First date:", stock_df[1,1] ,sep = " "))
    # warning("StartFiscal (year) doesn't match the first price record.")
    stop(warning("StartFiscal (year) doesn't match the first price record."))
  }
  
  # Extract Price Records
  stock <- getSymbols(ticker, src = "yahoo", from = start, to = end, auto.assign = FALSE)
  
  startyear_adjustment = interval(start, index(stock)[1]) %/% years(1)
  endyear_adjustment = interval(start, index(stock)[length(index(stock))]) %/% years(1)
  
  # Print ticker, period and corresponding fiscal years
  print(paste0("ticker: ", ticker))
  print(paste("Extracting Period: ", index(stock)[1], "to", index(stock)[length(index(stock))], sep = " "))
  print(paste("Fiscal Year:", startFiscal + startyear_adjustment, "to", startFiscal + endyear_adjustment, sep = " "))
  
  # Adjust start, end and startFiscal
  start = start + years(startyear_adjustment)
  end = index(stock)[length(index(stock))]
  startFiscal = startFiscal + startyear_adjustment
  
  stock_df <- data.frame(Date = index(stock), coredata(stock)) %>%
    setNames(c("Date","Open", "High", "Low", "Close", "Volume", "Adjusted"))
  
  # Classify data by fiscal year
  stock_df <- stock_df %>%
    mutate(fiscal = startFiscal + interval(start, Date) %/% years(1),
           quarter = interval(start, Date - years(fiscal - startFiscal)) %/% months(3) + 1) %>%
    mutate(year_quarter = paste0(fiscal, "Q", quarter))
  
  return(stock_df)
}

# Obtain price record with fiscal years and quarters
ticker = "COST"
start = "2011-01-01"
startFiscal = 2011
PriceRecord = Stock_Fiscal_Quarter(ticker, start, startFiscal)
head(PriceRecord)

# Get High and Low of each fiscal year
# Change the format of data to facilitate excel work
Fiscal_High_Low = PriceRecord %>%
  select(fiscal, High, Low, Date) %>%
  group_by(fiscal) %>%
  summarise(Fiscal_High = max(High), Fiscal_Low = min(Low)) %>% 
  pivot_longer(c(Fiscal_High, Fiscal_Low), names_to = "Range", values_to = "Value") %>% 
  arrange(desc(fiscal))  %>% pivot_wider(names_from = fiscal, values_from = "Value")

# create data folder for the ticker
tickerDir <- paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker)
dataDir <- paste0(tickerDir,"/data")
if(!dir.exists(tickerDir)) {
  dir.create(tickerDir)
  dir.create(dataDir)
  print(paste0("Create folders for ", ticker))
} else if(!dir.exists(dataDir)){
  dir.create(dataDir)
  print(paste0("Create data folder for ", ticker))
} else {
  # The folders for the ticker exist
}

# save the data as "Price.xlsx"
outputdata = set_names(list(ticker = Fiscal_High_Low), ticker)
outputfile = paste0(dataDir,"/Price.xlsx")
write.xlsx(outputdata, file = outputfile)

# Plot stock price record
PriceRecord %>% 
  select(Date, Close, fiscal, quarter) %>%
  padr::pad(interval = "day") %>%
  fill(c(Close, fiscal, quarter), .direction = "up") %>%
  ggplot() +
  geom_line(aes(x = Date, y = Close)) +
  theme_minimal() + 
  labs(title = paste0(ticker, " stock price"),
       y = "Close Price")








# Previous Work -------------------------------------------------------

FiscalPriceRange = function(ticker, start, end, startFiscal, endFiscal){
  
  # Package names
  packages <- c("quantmod", "tidyverse", "lubridate","openxlsx")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  # convert start date and end date to time
  start = ymd(start)
  end = ymd(end)
  
  # Check validity of input
  if(startFiscal > endFiscal){
    stop("startFiscal should be less than or equal to endFiscal")
  } else if(start > end){
    stop("start should be less than or equal to end")
  }
  
  # Print the result
  print(paste0("ticker: ", ticker))
  print(paste("Extracting Period: ", start, "to", end, sep = " "))
  print(paste("Fiscal Year:", as.character(startFiscal), "to", as.character(endFiscal), sep = " "))

  
  # Extract Price Records
  stock <- getSymbols(ticker, src = "yahoo", from = start, to = end, auto.assign = FALSE)
  stock_df <- data.frame(Date = index(stock), coredata(stock)) %>%
    setNames(c("Date","Open", "High", "Low", "Close", "Volume", "Adjusted"))
  
  # Print first row of price record for reference
  print("First Record:")
  print(stock_df[1,])
  
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
                        labels = startFiscal:endFiscal))
  
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
ticker = "ROKU"
yesterday = Sys.Date() - 1    # Yesterday
PriceHighLow <- FiscalPriceRange(ticker, "2017-01-01", yesterday, startFiscal = 2017, endFiscal = 2021)

# save the data in "Price.xlsx"
outputdata = set_names(list(ticker = PriceHighLow), ticker)
outputfile = paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker,"/data/Price.xlsx")
write.xlsx(outputdata, file = outputfile)
