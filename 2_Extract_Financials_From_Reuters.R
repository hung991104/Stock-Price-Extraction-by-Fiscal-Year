# 2_Extract annual financials from Reuters 

# Load Libraries ----------------------------------------------------------

# Package names
packages <- c("rvest", "tidyverse", "lubridate","openxlsx", "stringi", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Initialize --------------------------------------------------------------

Sys.setlocale("LC_TIME", "English")

# ticker and fiscal years of the company concerned
ticker = "BABA"
fiscalyears = seq(from = 2020, by = -1, length.out = 5)

# Reuters' url for financials
URL = "https://www.reuters.com/companies/BABA.N/financials/"
annual_statement_name = list(IS = "income-statement-annual", BS = "balance-sheet-annual",CF = "cash-flow-annual")
annual_URL = lapply(annual_statement_name, function(x) paste0(URL, x))

# Nodes corresponding to the table
Nodes = ".Financials-section-content-2tlo2"



# Scrap Financials --------------------------------------------------------

# Get html and extract the table 
extractTable <- function(url, nodes){
  html <- read_html(url)
  figure <- html %>% html_nodes(nodes) %>% html_table() %>% .[[1]]
  figure
}

# tidy and save the financial statements in local directory

for(i in seq_along(annual_statement_name)){
  df = extractTable(annual_URL[[i]], Nodes)
  df_name = names(annual_statement_name)[i]
  print(paste0("Importing ", df_name, "..."))
  
  # update column names with fiscal years
  colnames(df) = c("Metrics", as.character(fiscalyears), "Trend")
  
  df = df %>% 
    select(-Trend) %>%
    # replace NA pattern with zero
    mutate(across(contains("20"), function(x) gsub("--", "0.0", x))) %>%
    # remove the 000 separator in numbers
    mutate(across(contains("20"), function(x) gsub(",", "", x))) %>%
    # replace () with - 
    mutate(across(contains("20"), function(x) as.numeric(gsub("\\)", "", gsub("\\(", "-", x)))))
  
  # save in local directory
  outputdata = set_names(list(ticker = df), ticker)
  outputfile = paste0("C:/Users/User/Desktop/asset/analysis/stock analysis/", ticker,"/data/",df_name, "_Reuters.xlsx")
  openxlsx::write.xlsx(outputdata, file = outputfile)
  print(paste0(df_name, " is saved!"))
}




# Not Used ----------------------------------------------------------------

Time = URL %>% 
  read_html() %>%
  html_nodes(".tables-container .TextLabel__medium___t9PWg") %>%
  html_text()


library(XML)
tbls_xml = readHTMLTable(URL)


ReplaceZero = function(x){
  if(grepl(x, "-")){
    x = 0
  }
  x
}

for(i in seq_along(annual_statement_name)){
  df_name = names(annual_statement_name)[i]
  assign(df_name, extractTable(annual_URL[[i]], Nodes))
  
  # update colnames with fiscal years
  print(get(df_name) %>% head())
  #colnames(get(df_name)) = c("Metrics", as.character(fiscalyears), "Trend")
}


as.symbol("IS") %>% head()
head(as.symbol("IS"))

get("IS") %>% head()
