# Downloading data base
library(tidyverse)
library(dplyr)
library(DT)
library(zoo)
#Finance data:
library(quantmod)
# Modeling
library(tidyquant)
symbols = data.frame(read_csv("tickers/tickers.csv"))


func_raw_data = function(company){
  ticker = as.character(symbols[symbols$Company == company,]$Ticker)
  df = getSymbols(ticker, src="yahoo") %>% get()
  
  return(df)
}


time_series = list()
for (company in symbols$Company){
  data = func_raw_data(company)
  time_series[[company]] = data
}
time_series[["last_update"]] = Sys.Date()
save(time_series, file = "data/time_series.RData")
