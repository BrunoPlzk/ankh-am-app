#Update:
library(tidyverse)
library(dplyr)
library(DT)
library(zoo)
#Finance data:
library(quantmod)
# Modeling
library(tidyquant)
load("data/time_series.RData")
symbols = data.frame(read_csv("tickers/tickers.csv"))

company = "BNP Paribas"

for (company in symbols$Company){
  
  
  #Get ticker
  ticker = as.character(symbols[symbols$Company == company,]$Ticker)
  #Load existing data
  data = as.data.frame(time_series[[company]])
  #Get last_update date
  last_update = as.Date(time_series[["last_update"]])
  #Today's date
  today = Sys.Date()
  
  if (last_update < today){
    #Data to promote 
    to_promote = getSymbols(ticker, scr = "yahoo", from = last_update, to = today) %>% get()
    #Last existing date
    last_date = rownames(data)[nrow(data)]
    #Promote only new data
    to_promote = to_promote %>% filter(rownames(to_promote) != last_date)
    #Appending rows
    data = rbind(data, to_promote)
  }
  
  #Getting last quote
  to_update = getQuote(ticker) %>% select("Trade Time", "Last", "Open", "High", "Low", "Volume")
  colnames(to_update) = c("Date", paste(ticker,".Close",sep = ""), paste(ticker,".Open",sep = ""), paste(ticker,".High",sep = ""), paste(ticker,".Low",sep = ""), paste(ticker,".Volume",sep = ""))
  rownames(to_update) = as.Date(to_update %>% pull(Date) %>% first())
  to_update = to_update %>% select(-Date)
  to_update[,paste(ticker,".Adjusted", sep = "")] = to_update[,paste(ticker,".Close", sep = "")]
  to_update = to_update %>% select(colnames(data))
  
  #Last quote's date
  last_quote = rownames(to_update)[nrow(to_update)]
  
  #Updating or appending row depending on the date
  if (rownames(data)[nrow(data)] == last_quote){
    data[nrow(data),] = to_update[1,]
  } else {
    data = rbind(data, to_update)  
  }
  
  #Update data
  time_series[[company]] = as.xts(data)
}
save(time_series, file = "data/time_series.RData")
rm(list = ls())
