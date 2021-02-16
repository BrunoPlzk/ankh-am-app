#User Interface:

dashboardPage(
    title = "ANKH Asset Management",
    skin = "black",
    dashboardHeader(title =  HTML(paste("ANKH", icon("ankh"), "Asset Management" )),
                    dropdownMenuOutput("NotificationMenu"),
                    titleWidth = 300
                    ),   #Header of the dashboard, icon+title
    dashboardSidebar(
        sidebarMenu(id = "sidebarid",
            #Creates tabs of the left hand side
            
            #Stock Analysis Item
            menuItem("Stock Analysis", tabName = "stock_analysis", icon = icon("poll"),
                selectInput("company", label = "Select company/index :", choices = symbols[,"Company"], multiple = FALSE),
                menuSubItem("Stock market", tabName = "stocks", icon = icon("chart-line")),
                menuSubItem("Data", tabName = "data", icon = icon("table")),
                menuSubItem("Technical analysis", tabName = "technical", icon = icon("chart-area")),
                conditionalPanel('input.sidebarid == "technical"',
                         dateRangeInput("RangeDateTech", "Date range:", weekstart = 1))),
            
            #Key Decision
            menuItem("Predictive Models", tabName = "key_decisions", icon = icon("bezier-curve"),
                        menuSubItem("BBands Signaling", tabName = "bbands_signaling", icon = icon("bootstrap"))
                        ),
            
            #Portfolio
            menuItem("Portfolio Analysis", tabName = "portfolio", icon = icon("money-check-alt")),
            menuItem("Contact", tabName = "about", icon = icon("at"))
            # menuItem("Predictions", tabName = "predictions", icon = icon("crosshairs"))
            # 
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "stocks",
                        fluidRow(
                            valueBoxOutput("last_quote", width = 6),
                            box(dateInput("inDate", "Input date",weekstart = 1), width = 3),
                            box(selectInput("scale", "Select scaling :", c("No scaling", "Logarithmic scaling")),width = 3),
                            tabBox(
                                title = "Plots",
                                tabPanel(title = "Stock", icon = icon("dollar-sign"), plotlyOutput("stock")),
                                tabPanel(title = "Daily Return", icon = icon("undo-alt"), plotlyOutput("daily_returns")),
                                tabPanel(title = "Weekly Return", icon = icon("undo-alt"), plotlyOutput("weekly_returns")),
                                tabPanel(title = "Biweekly Return", icon = icon("undo-alt"), plotlyOutput("two_weeks_returns")),
                                width = 12
                                )
                            
                            ),
                        fluidRow(
                            valueBoxOutput("one_day"),
                            valueBoxOutput("one_week"),
                            valueBoxOutput("two_weeks")
                        ),
                        fluidRow(
                            box(numericInput("days", value = 1, label = "Select number of lag days :", step = 1),
                                valueBoxOutput("custom_days", width = 15),
                                width = 4),
                            box(dateRangeInput("RangeDate", "Date",weekstart = 1),
                                valueBoxOutput("range_days", width = 15),
                                width = 4)
                        ),
                    p("Last update", code(Sys.Date()))
                
            ),
            tabItem(tabName = "data",
                    dataTableOutput("data")),
            
            tabItem(tabName = "technical",
                    
                    #TabBox section of the Technical analysis tab item
                    tabBox(title = "Analysis",
                           
                           #Moving Average Convergence Divergence
                           tabPanel(title = "MACD",
                                    icon = icon("wave-square"),
                                    plotlyOutput("macd", height = 600),
                                    br(),
                                    fluidRow(
                                        box(numericInput("nFast", label = "Number of periods for fast moving average:", value = 12, min = 1, max = 100), width = 4, height = 100),
                                        box(numericInput("nSlow", label = "Number of periods for slow moving average:", value = 26, min = 1, max = 100), width = 4, height = 100),
                                        box(numericInput("nSig", label = "Number of periods for signal moving average:", value = 9, min = 1, max = 100), width = 4, height = 100)
                                    ),
                                    fluidRow(box(selectInput("macd_scale", "Select scaling :", c("No scaling", "Logarithmic scaling")),width = 4))
                                    ),
                           #Boolinger bands
                           tabPanel(title = "Boolinger-Bands",
                                    icon = icon("bootstrap"),
                                    plotlyOutput("boolinger", height = 600),
                                    br(),
                                    fluidRow(
                                        box(numericInput("nMA_bbands", label = "Number of periods for moving average:", value = 0), width = 6),
                                        box(numericInput("nSD_bbands", label = "Number of standard deviations to use:", value = 0), width = 6)
                                    ),
                                    fluidRow(box(selectInput("bool_scale", "Select scaling :", c("No scaling", "Logarithmic scaling")),width = 4))
                           )
                           ,width = 12),
            
                    #Last update
                    p("Last update", code(Sys.Date()))
                    ),
            
            #BBands signaling
            
            tabItem(tabName = "bbands_signaling",
                    tabBox(title = "BBands Signaling",
                           
                           tabPanel(title = "Strategy",
                                    dataTableOutput("bbands_signal"),
                                    p("Last update", code(Sys.Date()))
                                    ),
                           
                           tabPanel(title = "Optimal Parameters",
                                    dataTableOutput("bbands_signal_opt"),
                                    p("Last update", code(bbands_signaling[["last_update"]]))
                                    )
                           
                           ,width = 12),
                    p("----")
                    
                    
                    
                    ),
        
            #Contact tab item
            tabItem(tabName = "about",
                    p(h2(strong("Co-founders:"))),
                    p(a("Bruno Pilarczyk", href="https://www.linkedin.com/in/bruno-pilarczyk-042739143", target="_blank"), em(" | Data Scientist"), style = "font-size:25px"),
                    p("e-mail: bruno.plzk@gmail.com",style = "font-size:20px"),
                    p(a("Rudolph Rousseaux", href="https://www.linkedin.com/in/rudolph-rousseaux-42b258137/", target="_blank"), em(" | Portfolio Manager"),style = "font-size:25px"),
                    p("e-mail: rudolph.rousseaux@gmail.com",style = "font-size:20px"))
            
            #, 
            # tabItem(tabName = "predictions",
            #         plotlyOutput("lstm_plot"))

        ) #end tabItems()
        
    ) #end dashboardBody()
)