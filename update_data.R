#Update

# Used packages
packages = c("shiny", "shinydashboard", "tidyverse", "plotly", "dplyr","DT",
             "zoo", "quantmod", "readr","ggplot2","bizdays", "lubridate")

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

# Load time_series:
load("data/time_series.RData")
symbols = data.frame(read_csv("tickers.csv"))

#Update function
update = function(company){
  # company = "Danone"
  
  print(paste(company, "updating..."))
  
  #Get ticker
  ticker = as.character(symbols[symbols$Company == company,]$Ticker)
  ticker_clean = paste(str_extract_all(ticker, "[A-Z.0-9]")[[1]],collapse="")
  #Load existing data
  data = as.data.frame(time_series[[company]])
  #Get last_update date
  last_update = as.Date(time_series[["last_update"]])
  #Today's date
  today = Sys.Date()
  try(
  if (last_update < today){
    #Last existing date
    last_date = as.Date(rownames(data)[nrow(data)])
    
    #Data to promote 
    getSymbols(ticker, scr = "yahoo", from = last_date-30, to = today)
    to_promote = get(ticker_clean)
    
    #As a dataframe
    to_promote = as.data.frame(to_promote)
    
    #Getting last data
    data = data[!((rownames(data) %in% rownames(to_promote)) | (rownames(data) == today)),]
    
    #Appending rows
    data = rbind(data, to_promote)
  }, silent = TRUE
  )
  #Getting last quote
  to_update = getQuote(ticker) %>% select("Trade Time", "Last", "Open", "High", "Low", "Volume")
  colnames(to_update) = c("Date", paste(ticker_clean,".Close",sep = ""), paste(ticker_clean,".Open",sep = ""), paste(ticker_clean,".High",sep = ""), paste(ticker_clean,".Low",sep = ""), paste(ticker_clean,".Volume",sep = ""))
  rownames(to_update) = as.Date(to_update %>% pull(Date) %>% first())
  to_update = to_update %>% select(-Date)
  to_update[,paste(ticker_clean,".Adjusted", sep = "")] = to_update[,paste(ticker_clean,".Close", sep = "")]
  to_update = to_update %>% select(colnames(data))
  
  #Last quote's date
  last_quote = rownames(to_update)[nrow(to_update)]
  
  #Updating or appending row depending on the date
  if (rownames(data)[nrow(data)] == last_quote){
    data[nrow(data),] = to_update[1,]
  } else if (rownames(data)[nrow(data)] < last_quote){
    data = rbind(data, to_update)  
  }
  
  data = data %>% distinct()
  
  return(data)
}

#Looping over all companies in symbols csv:
for (company in symbols$Company){
  
  data = try(update(company))
  
  #Update data
  time_series[[company]] = data

}

time_series[["last_update"]] = Sys.Date()
save(time_series, file = "data/time_series.RData")
rm(list = ls())
