#Global:

#Get symbols:
source("global.R")


#BBands_signaling

bbands_signaling = list()

#MA to test:
MA = seq(10,35,1)
SD = seq(2,5,0.1)

companies = symbols$Company

for (company in companies){

  # company = "Airbus"
  #company = "BNP Paribas"
  
  print(company)
  
  #Build df_signal table
  df_signal = data.frame(
    company = character(0),
    n_MA  = numeric(0),
    x_SD = numeric(0),
    buy_hold_return = numeric(0),
    strat_return = numeric(0))
    
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
  
  
  for (n_MA in MA){
    for(x_SD in SD){
    
    # n_MA = 5
    # x_SD = 3
      
    print(paste(company, ": computing", "MA =", n_MA, "and", "SD =", x_SD, "combination"))
    
    #Compute MA(n), MA(n)+ p * SD(n)
    df_loop =  cbind(df, BBands(df, n = n_MA, sd = x_SD)) %>% select(-pctB)
    rownames(df_loop) = dates
    
    #Round up to 4 decimals
    for (col in colnames(df_loop)){
      df_loop[,col] = round(df_loop[,col],4)
    }
    
    #Skipping first row
    df_loop = df_loop %>% drop_na()
    
    #Set recommendation to 0
    df_loop$Recommendation = 0
    
    for (i in 1:(nrow(df_loop)-1)){
      if (df_loop[i,"High"] >= df_loop[i,"up"]){
        df_loop[i+1,"Recommendation"] = 0
      } else if (df_loop[i,"Low"] <= df_loop[i,"dn"]){
        df_loop[i+1,"Recommendation"] = 1
      } else {
        
        if (df_loop[i,"Recommendation"] == 1){
          df_loop[i+1,"Recommendation"] = 1
        } else{
          df_loop[i+1,"Recommendation"] = 0
        }
        
      }
    }
    
    #Skipping first row
    df_loop$Date = rownames(df_loop)
    
    #Computing returns:
    df_loop = df_loop %>% mutate(return = log(Close)-log(lag(Close,1)))
    
    df_loop = df_loop %>% drop_na()
    
    #Strategy return
    df_loop$strat_return = df_loop$return * df_loop$Recommendation
    
    #Cumulative STRATEGY
    df_loop$strat_cum_return = cumsum(df_loop$strat_return)
    
    #Cumulative BUY & HOLD strategy
    df_loop$buy_hold_return = cumsum(df_loop$return)
    
    #Cleaning
    df_loop = df_loop %>% select(Date, everything())
    
    result = data.frame(company = company,
                        n_MA = n_MA,
                        x_SD = x_SD,
                        buy_hold_return = df_loop$buy_hold_return %>% last(),
                        strat_return = df_loop$strat_cum_return %>% last())
    
    df_signal = rbind(df_signal, result)
    }
  }
  
  optimum = df_signal %>% filter(strat_return == max(strat_return)) %>% first()
  
  final_result = data.frame(MA_opt = optimum$n_MA,
                            SD_opt = optimum$x_SD,
                            strategy_log_return = optimum$strat_return,
                            buy_hold_log_return = optimum$buy_hold_return)
  
  bbands_signaling[[company]] = final_result
}

bbands_signaling[["last_update"]] = Sys.Date()

save(bbands_signaling, file = "bbands_signaling.RData")
