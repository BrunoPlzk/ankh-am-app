#Global:

# Used packages
packages = c('shiny', 'shinydashboard', 'tidyverse', 'plotly', 'dplyr','DT',
            'zoo', 'quantmod', 'readr','ggplot2','bizdays', 'lubridate')

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
  }
})

# #Shiny
# library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
# #Data manipulation
# library(tidyverse)
# library(dplyr)
# library(DT)
# library(zoo)
# #Finance data:
# library(quantmod)
# # Modeling
# library(tidyquant)
# #Data import:
# library(readr)
# #Data viz:
# library(plotly)
# library(ggplot2)
# #Calendar:
# library(bizdays)
# library(lubridate)

#Get symbols:
source("scripts/update_data.R")
symbols = data.frame(read_csv("data/tickers.csv"))
load("data/time_series.RData")
load("data/bbands_signaling.RData")

#Stock data
func_data = function(raw_data, company){
  ticker = as.character(symbols[symbols$Company == company,]$Ticker)
  ticker_clean = paste(str_extract_all(ticker, "[A-Z.0-9]")[[1]],collapse="")
  df = raw_data
  
  df[,paste(ticker_clean,".Close", sep = "")] = na.approx(df[,paste(ticker_clean,".Close", sep = "")])
  df$DailyReturns = 0
  df = as.data.frame(df[,c(paste(ticker_clean,".Close", sep = ""), "DailyReturns")])
  colnames(df) = c("Price", "DailyReturns")
  df[,"Date"] = as.Date(rownames(df), tz = "UTC")
  
  #Get last:
  # last_price = getQuote(ticker)
  # last_df = data.frame(Price = last_price[,"Last"][1],
  #                      DailyReturns = last_price[,"% Change"][1]/100,
  #                      Date = as.Date(last_price[,"Trade Time"][1],tz = "UTC"))
  # rownames(last_df) = last_df[,"Date"][1]
  # if (last_df[,"Date"][1] > df[,"Date"][nrow(df)]){
  #   df = rbind(df, last_df)
  # }
  df$Price = round(na.approx(df$Price),4)
  
  df = df %>% mutate(DailyReturns = round(Price/lag(Price,1) - 1,5))
  df = df %>% mutate(WeeklyReturns = round(Price/lag(Price,5) - 1,5))
  df = df %>% mutate(TwoWeeksReturns = round(Price/lag(Price,10) - 1,5))
  return(df)
}

#Technical analysis data
func_tech_data = function(raw_data){
  #ticker = as.character(symbols[symbols$Company == company,]$Ticker)
  df = as.data.frame(raw_data)
  colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  return(df)
  
}

#Stock market chart function:
func_plot_stock = function(data, x, y, x_label, y_label, color = "black"){
  fig = plot_ly(x = ~x,
                y = ~y,
                type = 'scatter',
                mode = 'lines',
                line = list(color = color, width = 1),
                showlegend = TRUE)
  fig = fig %>% layout(xaxis = list(title = x_label,
                                    showspikes = TRUE,
                                    spikemode  = 'across+toaxis',
                                    spikesnap = 'cursor',
                                    spikedash = 'dot',
                                    showline=FALSE,
                                    showgrid=TRUE,
                                    spikethickness = 1),
                       yaxis = list(title = y_label),
                       hovermode = "x",
                       spikedistance =  -1)
  return(fig)
}

#MACD chart function:
func_plot_macd = function(data){

  fig1 = plot_ly(data = data,
                 x = ~Date,
                 y = ~Price,
                 type = 'scatter',
                 mode = 'lines',
                 line = list(color = "black", width = 1),
                 showlegend = TRUE,
                 name = "Stock")
  fig1 = fig1 %>% layout(xaxis = list(
    showspikes = TRUE,
    spikemode  = 'across+toaxis',
    spikesnap = 'cursor',
    spikedash = 'dot',
    showline=TRUE,
    showgrid=TRUE,
    spikethickness = 1),
    hovermode = "x",
    spikedistance =  -1)
  fig2 = plot_ly(data,
                 x = ~Date)
  fig2 = fig2 %>% add_trace(y = ~diff,
                            type = "bar",
                            name = "Difference",
                            marker = list(color = "green"))
  fig2 = fig2 %>% add_trace(y = ~signal,type='scatter', mode = "lines", line = list(color = "red", width = 1), name = "Signal")
  fig2 = fig2 %>% add_trace(y = ~macd,type='scatter', mode = "lines", line = list(color = "blue", width = 1), name = "MACD")
  fig2 = fig2 %>% layout(xaxis = list(title = "Date",
                                      showspikes = TRUE,
                                      spikemode  = 'across+toaxis',
                                      spikesnap = 'cursor',
                                      spikedash = 'dot',
                                      showline=TRUE,
                                      showgrid=TRUE,
                                      spikethickness = 1),
                         yaxis = list(title = "MACD"),
                         hovermode = "x",
                         spikedistance =  -1)
  
  fig = subplot(fig1, fig2, nrows = 2, shareX = TRUE)
  return(fig)
}

#Boolinger function
func_plot_boolinger = function(data_boolinger){
  fig1 = plot_ly(data = data_boolinger,
                 x = ~Date)
  fig1 = fig1 %>% add_trace(y = ~mavg,
                            type='scatter',
                            mode = "lines",
                            line = list(color = "black", width = 1),
                            name = "MA",
                            showlegend = FALSE,
                            line = list(color = 'transparent')
  )
  fig1 = fig1 %>% add_trace(y = ~dn,
                            type='scatter',
                            mode = "lines",
                            line = list(color = "rgba(83, 51, 237, 1)", width = 1),
                            name = "Low-BBand",
                            showlegend = FALSE,
                            line = list(color = 'transparent')
  )
  fig1 = fig1 %>% add_trace(y = ~up,
                            type='scatter',
                            mode = "lines",
                            line = list(color = "rgba(83, 51, 237, 1)", width = 1),
                            name = "High-BBand",
                            showlegend = FALSE,
                            hoverinfo = FALSE,
                            fill = "tonexty",
                            fillcolor='rgba(83, 51, 237, 0.07)',
                            line = list(color = 'transparent'))
  fig1 = fig1 %>% add_trace(
    type= "candlestick",
    open = ~Open,
    close = ~Close,
    high = ~High,
    low = ~Low,
    name = "Price")
  fig1 = fig1 %>% layout(yaxis = list(fixedrange = FALSE, autorange = "visible"))
  fig2 <- data_boolinger 
  fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = ~paste("Volume",direction),
                           color = ~direction, colors = c('#7F7F7F', '#17BECF')) 
  fig2 <- fig2 %>% layout(yaxis = list(title = "Volume", fixedrange = FALSE, autorange = "visible"))
  fig <- subplot(fig1, fig2, heights = c(0.7,0.2), nrows=2,
                 shareX = TRUE)
  fig = fig %>% layout(xaxis = list(title = "Date",
                                    showspikes = TRUE,
                                    spikemode  = 'across+toaxis',
                                    spikesnap = 'cursor',
                                    spikedash = 'dot',
                                    rangeslider = list(visible = FALSE),
                                    showline=TRUE,
                                    showgrid=TRUE,
                                    spikethickness = 1),
                       yaxis = list(fixedrange = FALSE, autorange = 'visible'),
                       hovermode = "x",
                       spikedistance =  -1,
                       legend = list(orientation = 'h', x = 0.5, y = 1,
                                     xanchor = 'center', yref = 'paper',
                                     #font = list(size = 10),
                                     bgcolor = 'transparent'))
  return(fig)
}


#Build BBands strategy
#Build df_signal table
df_signal = data.frame(
  High = numeric(0),
  Low  = numeric(0),
  Close = numeric(0),
  MA = numeric(0),
  DOWN = numeric(0),
  UP = numeric(0),
  Recommendation = character(0))

#Loop for each company in symbols
for (company in symbols$Company){
  
  #Get optimal parameters:
  optimum = bbands_signaling[[company]]
  
  n_MA = optimum$MA_opt
  x_SD = optimum$SD_opt
  
  #Apply func_tech_data function
  df = func_tech_data(time_series[[company]])
  
  #Remove NA
  for (col in colnames(df)){
    df[,col] = round(na.approx(df[,col]),4)
  }
  
  #Get proper index
  dates = rownames(df)
  rownames(df) = seq(1,nrow(df))
  
  #Select useful columns to compute Boolinger-Bands
  df = df[,c("High", "Low", "Close")]
  
  #Compute MA(n), MA(n)+ p * SD(n)
  df =  cbind(df, BBands(df, n = n_MA, sd = x_SD)) %>% select(-pctB) %>% last()
  #rownames(df) = dates
  
  #Round up to 4 decimals
  for (col in colnames(df)){
    df[,col] = round(df[,col],4)
  }
  
  
  #Set recommendation to ""
  df$Recommendation = ""
  
  if (df$High >= df$up){
    df$Recommendation = "SELL"
  } else if (df$Low <= df$dn){
    df$Recommendation = "BUY"
  } else {
    df$Recommendation = "HOLD"
  }
  
  #Append company name
  rownames(df) = company
  
  #Select columns in the right order
  df = df %>% select(High, Low, Close, mavg, dn, up, Recommendation)
  
  #Set the same columns names as df_signal
  colnames(df) = colnames(df_signal)
  
  #Append row to df_signal
  df_signal = rbind(df_signal, df)
}

df_notif = cbind(df_signal, "company" = rownames(df_signal))
df_notif = df_notif %>%
  filter(Recommendation != "HOLD") %>%
  mutate(icon = ifelse(Recommendation == "BUY", "arrow-circle-up", "arrow-circle-down")) %>%
  mutate(status = ifelse(Recommendation == "BUY", "success", "danger"))


#Build BBands Optimal parameters
#Setting up the data frame
df_opt = data.frame(
  MA_opt = numeric(0),
  SD_opt = numeric(0),
  strategy_log_return = numeric(0),
  buy_hold_log_return = numeric(0),
  nb_years = numeric(0)
)

#Rounding up to 4 decimals
for (company in symbols$Company){
  
  df = bbands_signaling[[company]]
  df = df %>% select(c("MA_opt",
                       "SD_opt",
                       "strategy_log_return",
                       "buy_hold_log_return"))
  
  for (col in colnames(df)){
    df[,col] = round(df[,col],4)
  }
  
  serie = time_series[[company]]
  
  
  period = as.numeric(difftime(rownames(serie)[nrow(serie)],
                               rownames(serie)[1], unit = "weeks")/52.25)
  
  df$nb_years = round(period,2)
  
  rownames(df) = company
  
  df_opt = rbind(df_opt, df)
}

#Get evolutions in percentages
df_opt$strategy_log_return = exp(df_opt$strategy_log_return) - 1
df_opt$buy_hold_log_return = exp(df_opt$buy_hold_log_return) - 1

#Renaming log returns to returns
df_opt = df_opt %>% rename("strategy_return" = "strategy_log_return",
                           "buy_hold_return" = "buy_hold_log_return")

# Annualized returns:

df_opt = df_opt %>%
  rownames_to_column("name") %>%
  mutate(strategy_return_year = (1+strategy_return)^(1/nb_years) - 1,
         buy_hold_return_year = (1+buy_hold_return)^(1/nb_years) - 1) %>% 
  column_to_rownames("name")


df_opt = df_opt[,c("MA_opt", "SD_opt", "strategy_return", "buy_hold_return",
                   "strategy_return_year", "buy_hold_return_year", "nb_years")]

# Write CSV file for python recommendation
company = rownames(df_opt)
df_opt_py = cbind(company, df_opt)
write.csv(df_opt_py, file = "data/bbands.csv", row.names = FALSE)

# data = func_data("Air Liquide")
# data_macd = cbind(data, as.data.frame(MACD(data$Price)))
# data_macd$diff = data_macd$macd - data_macd$signal 
# func_plot_macd(data_macd)


# model_name = "BNP Paribas"
# func_pred_data = function(data, model_name){
#   #Loading model:
#   model <- load_model_hdf5(paste("models/",model_name,".hdf5",sep=""))
# 
#   X = as.numeric(model$input$shape[2][1])
#   Y = as.numeric(model$output$shape[2])
#   #Unknown days:
#   past_data = data[,c("Price", "Date")]
#   past_data$Type = "Actual"
#   past = data[(nrow(past_data)-(X-1)):nrow(past_data),]
#   X_past = t(past[,"Price"])
#   Y_pred = c(t(model %>% predict(array(X_past, dim = c(1,X,1)))))
# 
#   #Create sequence of days:
#   create.calendar("Finance", weekdays = c("saturday", "sunday"))
#   last_day = past %>%
#     last() %>%
#     pull(Date)
#   new_days = bizseq(last_day+1, last_day+(Y+100), cal = "Finance")[1:Y]
# 
#   #predictions =
#   pred_data = data.frame(Price = Y_pred, Date = as.Date(new_days))
#   pred_data$Type = "Prediction"
# 
#   #Known Y days:
#   known = data[(nrow(past_data)-2*(X-1)-1):(nrow(past_data)-(X)),]
#   X_known = t(known[,"Price"])
#   Y_known_pred = c(t(model %>% predict(array(X_known, dim = c(1,X,1)))))
# 
#   known_data = data.frame(Price = Y_known_pred, Date = data[(nrow(past_data)-(Y-1)):nrow(past_data),"Date"], Type = "Prediction")
# 
#   prediction_data = rbind(past_data, pred_data, known_data)
# 
#   return(prediction_data)
# }


# prediction_data = func_pred_data(data, "BNP Paribas")
# ggplotly(ggplot(data = prediction_data) +
#            geom_line(mapping = aes(x = Date, y = Price, color = Type), size=0.5) +
#            scale_color_tq()) 
# data = func_data("Airliquide")
# data$decrease_four_days = 0
# for (i in 5:nrow(data)){
#   i = 5
#   if (mean(data$DailyReturns[(i-3):i]) < 0){
#     data$decrease_four_days[i] = 1
#     }
# }


