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
symbols = data.frame(read_csv("data/tickers.csv"))

# Not in function:
"%notin%" = Negate("%in%")

time_series = list()
last_update = list()

func_raw_data = function(company){
  ticker = as.character(symbols[symbols$Company == company,]$Ticker)
  getSymbols(ticker, src="yahoo")
  ticker_clean = paste(str_extract_all(ticker, "[A-Z.0-9]")[[1]],collapse="")
  df = get(ticker_clean)
  
  return(df)
}

for (company in symbols$Company){
  try(
  if (class(company) == "character"){
  print(paste("Downloading", company, "data..."))
  data = func_raw_data(company)
  time_series[[company]] = data
  })
}
last_update[["last_update"]] = Sys.Date()
save(time_series, file = "data/time_series.RData")
save(last_update, file = "data/last_update.RData")



