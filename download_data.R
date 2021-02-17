# Downloading data base
library(tidyverse)
library(dplyr)
library(DT)
library(zoo)
# Finance data:
library(quantmod)
options("getSymbols.warning4.0"=FALSE)
# Modeling
library(tidyquant)

#Getting symbols
symbols = data.frame(read_csv("tickers.csv"))

# Loading existing data
load("time_series.RData")

# Not in function:
"%notin%" = Negate("%in%")

# Existing companies:
existing_companies = names(time_series)[names(time_series) != "last_update"]

# New companies 
new_companies = symbols %>% filter(Company %notin% existing_companies) %>% pull(Company)

# "Error" companies
func_raw_data = function(company){
  ticker = as.character(symbols[symbols$Company == company,]$Ticker)
  getSymbols(ticker, src="yahoo")
  ticker_clean = paste(str_extract_all(ticker, "[A-Z.0-9]")[[1]],collapse="")
  df = get(ticker_clean)
  
  return(df)
}

for (company in new_companies){
  print(paste("Downloading", company, "data..."))
  data = func_raw_data(company)
  time_series[[company]] = data
}
time_series[["last_update"]] = Sys.Date()
save(time_series, file = "time_series.RData")



