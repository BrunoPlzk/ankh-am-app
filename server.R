#Server:

shinyServer(function(input, output, session) {
    
    ######### Dropdown Menu ###########
    output$NotificationMenu = renderMenu({
        notif <- apply(df_notif, 1, function(row) {
            notificationItem(text = HTML(paste("<strong>",row[["company"]],
                                               "</strong>", "signal:","<strong>",
                                               row[["Recommendation"]],"</strong>")),
                             icon = icon(row[["icon"]]),
                             status = row[["status"]]
                             )
                             
        })
    
        # This is equivalent to calling:
        #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
        dropdownMenu(type = "notifications", .list = notif)
    })
        
    
    ######### Reactive data ###########
    
    raw_data = reactive({
        company = input$company
        df = as.data.frame(time_series[[company]])
        
        #Return
        df
    })
    
    data = reactive({
        company = input$company
        df = func_data(raw_data(), company)
        
        #Return
        df
    })
    
    tech_data = reactive({
        company = input$company
        df = func_tech_data(raw_data)
        
        #Return
        df
    })
    
    ######### Observed data #########
    
    observeEvent(data(),{
        
        #Extract min. & max. dates from reactive data:
        min = min(data()$Date)
        max = max(data()$Date)
        
        #Update reference date:
        updateDateInput(session, "inDate",
                        label = paste("Select reference date : "),
                        value = max,
                        min   = min,
                        max   = max
        )
        
        #Update date range:
        if (input$RangeDate[1] < min){
            updateDateRangeInput(session, "RangeDate",
                                 label = paste("Select date range :"),
                                 start   = min,
                                 end   = max)
            updateDateRangeInput(session, "RangeDate",
                                 label = paste("Select date range :"),      
                                 min   = min,
                                 max   = max)
        } else {
            updateDateRangeInput(session, "RangeDate",
                                 label = paste("Select date range :"),      
                                 min   = min,
                                 max   = max)
            updateDateRangeInput(session, "RangeDate",
                                 label = paste("Select date range :"),
                                 start   = min,
                                 end   = max)
        }
        
        #Update date range technical analysis:
        if (input$RangeDateTech[1] < min){
            updateDateRangeInput(session, "RangeDateTech",
                                 label = paste("Select date range :"),
                                 start   = min,
                                 end   = max)
            updateDateRangeInput(session, "RangeDateTech",
                                 label = paste("Select date range :"),      
                                 min   = max-90,
                                 max   = max)
        } else {
            updateDateRangeInput(session, "RangeDateTech",
                                 label = paste("Select date range :"),      
                                 min   = min,
                                 max   = max)
            updateDateRangeInput(session, "RangeDateTech",
                                 label = paste("Select date range :"),
                                 start   = max-90,
                                 end   = max)
        }
        
    })
    
    # Observe optimal values for bbands:
    
    observeEvent(data(),{
         
      company = input$company
      
      MA_opt = df_opt[company, "MA_opt"]  
      SD_opt = df_opt[company, "SD_opt"] 
        
      updateNumericInput(session,
                         "nMA_bbands",
                         label = "Number of periods for moving average:",
                         min = 1,
                         max = 100,
                         value = MA_opt)
      
      updateNumericInput(session,
                         "nSD_bbands",
                         label = "Number of standard deviations to use:",
                         min = 1,
                         max = 100,
                         value = SD_opt)
      
    })
    
    
    ### Predictive data ####
    
    # predictive_data = reactive({
    # 
    #     predictive_data = func_pred_data(data(), as.character(input$company))
    #     predictive_data = predictive_data %>% filter(Date >= predictive_data$Date[nrow(predictive_data)] - 100)
    # 
    #     predictive_data
    # })
    
    #Value Boxes:
    
    #lastquote:
    output$last_quote = renderValueBox({
        ticker = as.character(symbols[symbols$Company == input$company,]$Ticker)
        last_quote = getQuote(ticker)
        last_price = last_quote[,"Last"][1]
        last_time = last_quote[,"Trade Time"][1]
        
        
        valueBox(
            value = paste(ticker,"=", last_price),
            subtitle = p(strong("Last trade:"),last_time),
            icon = icon("money-bill-alt")
        )
    })
    
    
    #1day:
    output$one_day = renderValueBox({
        val = as.numeric(data() %>% filter(Date == input$inDate) %>% select(DailyReturns))
        val = paste(round(val,4)*100,"%", sep = " ")
        valueBox(
            value = val,
            subtitle = p(strong("1 day"),"return at reference date"),
            icon = icon("percent"),
            color = "purple"
        )
    })
    
    #one_week:
    output$one_week = renderValueBox({
        val = as.numeric(data() %>% filter(Date == input$inDate) %>% select(WeeklyReturns))
        val = paste(round(val,4)*100,"%", sep = " ")
        valueBox(
            value = val,
            subtitle = p(strong("7 days"),"return at reference date"),
            icon = icon("percent"),
            color = "blue"
        )
    })
    #two_weeks:
    output$two_weeks = renderValueBox({
        val = as.numeric(data() %>% filter(Date == input$inDate) %>% select(TwoWeeksReturns))
        val = paste(round(val,4)*100,"%", sep = " ")
        valueBox(
            value = val,
            subtitle = p(strong("14 days"), "return at reference date"),
            icon = icon("percent"),
            color = "orange"
        )
    })
    
    #custom_day:
    output$custom_days = renderValueBox({
        validate(
            need(input$days != "", "Please select a number of days : ")
        )
        
        ndays = input$days
        
        if (ndays > 1){
            day_string = "days"
        } else{
            day_string = "day"
        }
        
        #Last date found:
        date_lag = data() %>%
            filter(Date <= input$inDate - ndays) %>%
            select(Date) %>%
            last() %>%
            pull(Date) 
        
        val1 = data() %>%
            filter(Date == input$inDate) %>%
            pull(Price)
        val2 = data() %>%
            filter(Date == date_lag) %>%
            pull(Price)
        
        val = val1/val2 - 1
        
        val = paste(round(val,4)*100,"%", sep = " ")
        valueBox(
            value = val,
            subtitle = p(strong(ndays, day_string),"return at reference date"),
            icon = icon("percent"),
            color = "red"
        )
    })
    
    #Range days:
    output$range_days = renderValueBox({
        validate(
            need(input$RangeDate[2] > input$RangeDate[1], "Please select a correct data range")
        )
        
        val1 = data() %>% filter(Date == input$RangeDate[1]) %>% pull(Price)
        val2 = data() %>% filter(Date == input$RangeDate[2]) %>% pull(Price)
        val = (val2/val1) - 1
        
        val = paste(round(val,4)*100,"%", sep = " ")
        valueBox(
            value = val,
            subtitle = paste("Return between date range"),
            icon = icon("percent"),
            color = "green"
        )
    })
    

    
    
    #Tables:
    output$data = renderDataTable({
        data = data() %>%
            arrange(desc(Date)) %>%
            select(Date, everything())
        
        datatable(data) %>%
            DT::formatPercentage(c("DailyReturns", "WeeklyReturns", "TwoWeeksReturns"),2)
    })
    
    #Plots:
    #Stock
    output$stock = renderPlotly({
        data = data() %>% filter((Date >= min(input$RangeDate))&(Date <= max(input$RangeDate)))
        
        if (input$scale == "Logarithmic scaling"){
            data$Price = log(data$Price)
        }
        
        func_plot_stock(data, data$Date, data$Price, "Date", "Closed Price", "black")
    })
    
    #DailyReturns:
    output$daily_returns = renderPlotly({
        data = data() %>% filter((Date >= min(input$RangeDate))&(Date <= max(input$RangeDate)))
        
        if (input$scale == "Logarithmic scaling"){
            data$DailyReturns = log(data$DailyReturns)
        }
        
        func_plot_stock(data, data$Date, data$DailyReturns, "Date", "Daily return", "blue")
    })
    
    #WeeklyReturns:
    output$weekly_returns = renderPlotly({
        data = data() %>% filter((Date >= min(input$RangeDate))&(Date <= max(input$RangeDate)))
        
        if (input$scale == "Logarithmic scaling"){
            data$WeeklyReturns = log(data$WeeklyReturns)
        }
        
        func_plot_stock(data, data$Date, data$WeeklyReturns, "Date", "7 days return", "purple")
    })
    
    #TwoWeeksReturns:
    output$two_weeks_returns = renderPlotly({
        data = data() %>% filter((Date >= min(input$RangeDate))&(Date <= max(input$RangeDate)))
        
        if (input$scale == "Logarithmic scaling"){
            data$TwoWeeksReturns = log(data$TwoWeeksReturns)
        }
        
        func_plot_stock(data, data$Date, data$TwoWeeksReturns, "Date", "14 days return", "green")
    })
    
    
    
    
    ###### Technical analysis:
    output$macd = renderPlotly({
        data = data()
        
        if (input$macd_scale == "Logarithmic scaling"){
            data$Price = log(data$Price)
        }
        
        macd = as.data.frame(round(MACD(data$Price, nFast = input$nFast, nSlow = input$nSlow, nSig = input$nSig),4))
        macd$diff = macd$macd - macd$signal
        
        data_macd = cbind(data, macd) %>% filter((Date >= min(input$RangeDateTech))&(Date <= max(input$RangeDateTech)))
        
        func_plot_macd(data = data_macd) 
    })
    
    output$boolinger = renderPlotly({
        #Get raw data
        data_boolinger = tech_data()
        
        #NA treatment:
        for (col in colnames(data_boolinger)){
            data_boolinger[,col] = na.approx(data_boolinger[,col])
        }
        
        if (input$bool_scale == "Logarithmic scaling"){
            for (col in colnames(data_boolinger)){
                if (col != "Volume"){
                data_boolinger[,col] = log(data_boolinger[,col])
                }
            }
        }
        #Add the date, remove index (cause issues with BBands() function)
        data_boolinger$Date = as.Date(rownames(data_boolinger))
        rownames(data_boolinger) = seq(1,nrow(data_boolinger))
        
        # colors column for increasing and decreasing
        data_boolinger$direction = NULL
        data_boolinger = data_boolinger %>% drop_na()
        for (i in 1:length(data_boolinger[,1])) {
            if (data_boolinger$Close[i] >= data_boolinger$Open[i]) {
                data_boolinger$direction[i] = 'Increasing'
            } else {
                data_boolinger$direction[i] = 'Decreasing'
            }
        }
        
        #Boolinger:
        bbands =  BBands(data_boolinger[,c("High", "Low", "Close")], n = input$nMA_bbands, sd = input$nSD_bbands)
        data_boolinger = cbind(data_boolinger, bbands)
        #Filtering on the date
        data_boolinger = data_boolinger %>% filter((Date >= min(input$RangeDateTech))&(Date <= max(input$RangeDateTech)))
        
        func_plot_boolinger(data_boolinger)
    })
    
    
    #### Predictive Models
    
    ## BBands strategy
    #Tables:
    output$bbands_signal = renderDataTable({
        
        #Show signal table:
        datatable(df_signal)
    })
    
    #Optimal parameters BBands:
    output$bbands_signal_opt = renderDataTable({
        
        #Show optimal parameters table:
        datatable(df_opt) %>%
            DT::formatPercentage(c("strategy_return", "buy_hold_return", "strategy_return_year", "buy_hold_return_year"),2)
    })
    
    
    
    
    # #Prediction data:
    # output$lstm_plot = renderPlotly({
    #     ggplotly(ggplot(data = predictive_data()) +
    #                  geom_line(mapping = aes(x = Date, y = Price, color = Type),
    #                            size=0.5) +
    #                  scale_color_tq() +
    #                  theme_light())
    #})
})
