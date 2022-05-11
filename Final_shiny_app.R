library(shiny)
library(fpp3)
library(shinydashboard)
data("aus_production")
#main plot
main_plot <- autoplot(aus_production, Beer)
 #seasonal
seasonal <- gg_subseries(aus_production, Beer)
#decomposition
decompo <- aus_production %>%
    model(STL(Beer)) %>%
    components() %>%
    autoplot()
#autocorrelation
autocor <- aus_production %>%
    ACF(Beer) %>%
    autoplot()
#Forecast
fit <- aus_production %>%
    model(TSLM(Beer ~ trend()))
#Naive
naive <- aus_production %>%
    model(NAIVE(Beer)) %>%
    forecast(h =12) %>%
    autoplot(aus_production)

#Seasonal Naive
seas_naive <- aus_production %>%
    model(SNAIVE(Beer ~ lag())) %>%
    forecast(h =12) %>%
    autoplot(aus_production)

#Mean
mean_plot <- aus_production %>%
    model(MEAN(Beer)) %>%
    forecast(h =12) %>%
    autoplot(aus_production)

#Drift
drift_plot <- aus_production %>%
    model(RW(Beer ~ drift())) %>%
    forecast(h =12) %>%
    autoplot(aus_production)

#Holt
holt <- aus_production %>%
    model(ETS(Beer ~ error("A") + trend("A") + season("N")))%>%
    forecast(h =12) %>%
    autoplot(aus_production)


#Holt-Winter's
hw_plot <- aus_production %>%
    model(
        additive = ETS(Beer ~ error("A") + trend("A") +
                           season("A")),
        multiplicative = ETS(Beer ~ error("M") + trend("A") +
                                 season("M"))) %>%
    forecast(h =12) %>%
    autoplot(aus_production)

#ARIMA

aus_production %>%
    
    mutate(Beer = difference(Beer)) %>%
    
    mutate(Beer = difference(Beer, 4)) %>%
    
    gg_tsdisplay(Beer, plot_type = "partial") 



arima_plot_1 <- aus_production %>%
    
    model(ARIMA(Beer ~ pdq(0, 1, 1) + PDQ(0, 1, 1))) %>%
    forecast(h =12) %>%
    autoplot(aus_production)



arima_plot_2 <- aus_production %>%
    
    model(ARIMA(Beer)) %>%
    forecast(h =12) %>%
    autoplot(aus_production)



ui <- dashboardPage(
    dashboardHeader(title = "BAS 475 Final App"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Midterm Components", tabName = "MidtermComponents"),
            menuItem("Simple Models", tabName = "SimpleModels"),
            menuItem("Holts and Holts/Winters", tabName = "HoltsandHoltsWinters"),
            menuItem("ARIMA and Parameter Estimates", tabName = "ARIMAandParameterEstimates")
        )
    ),
       
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "MidtermComponents",
                    fluidRow(
                        plotOutput("main_plot"),
                        
                        radioButtons("aus_production", "User Plot Selection",
                                     c("Seasonality" = "season",
                                       "Autocorrelation" = "auto",
                                       "Decomposition" = "decomp")),
                        
                        plotOutput("ts_plot"),
                        
                        mainPanel(strong("Seasonality interpretation:"),p( "Q2 has lowest production of Beer, while Q4 has the highest"),
                                  
                                  strong("Autocorrelation interpretation:"),p( "You can use every fourth lag as a very strong indicator"),
                                  
                                  strong("Decomposition interpretation:"), "Seasonality is strong and the trend is continually increasing"),
                        
                        titlePanel("Forecasting"),
                        plotOutput("fit"),
                        
                        mainPanel(strong("Forecasting interpretation:"), "Based on the trends in the current data, this forecast predicts Beer production to start to increase again", align = "center")
                    )
                        ),
        
            
            
            # Second tab content
            tabItem(tabName = "SimpleModels",
                    titlePanel("Naive Model"),
                    plotOutput("naive"),
                        
                    titlePanel("Seasonal Naive Model"),
                        plotOutput("seas_naive"),
                        
                    titlePanel("Mean Plot"),
                        plotOutput("mean_plot"),
                        
                    titlePanel("Drift Plot"),
                        plotOutput("drift_plot")
            ),
    # Third tab content
    tabItem(tabName = "HoltsandHoltsWinters",
            titlePanel("Holts Exponential Smoothing"),
            plotOutput("holt"),
                       
            titlePanel("Holts/Winters Exponential Smoothing"),
                       plotOutput("hw")
    ),
    #Fourth tab content
    tabItem(tabName = "ARIMAandParameterEstimates",
            titlePanel("Manually Selected ARIMA Model"),
            plotOutput("arima_plot_1"),
                       
                       
            titlePanel("Auto Selected ARIMA Model"),
                       plotOutput("arima_plot_2")
)
)
    ))    
    
        
       


server <- shinyServer(
    function(input, output) {

    output$main_plot <- renderPlot({
        main_plot
    })
    
    output$ts_plot <- renderPlot({
        switch(input$aus_production,
               season = seasonal,
               auto = autocor,
               decomp = decompo)
    })
    
    output$fit <- renderPlot({
        fit %>%
            forecast() %>%
            autoplot(aus_production)
    })
    output$naive <- renderPlot({
        aus_production %>%
            model(NAIVE(Beer)) %>%
            forecast(h =12) %>%
            autoplot(aus_production)
    })
    output$seas_naive <- renderPlot({
        aus_production %>%
            model(SNAIVE(Beer ~ lag())) %>%
            forecast(h =12) %>%
            autoplot(aus_production)
    })
    output$mean_plot <- renderPlot({
        aus_production %>%
            model(MEAN(Beer)) %>%
            forecast(h =12) %>%
            autoplot(aus_production)
    })
    output$drift_plot <- renderPlot({
        aus_production %>%
            model(RW(Beer ~ drift())) %>%
            forecast(h =12) %>%
            autoplot(aus_production)
    })
    output$holt <- renderPlot({
        aus_production %>%
            model(ETS(Beer ~ error("A") + trend("A") + season("N")))%>%
            forecast(h =12) %>%
            autoplot(aus_production)
    })
    output$hw <- renderPlot({
        aus_production %>%
            model(
                additive = ETS(Beer ~ error("A") + trend("A") +
                                   season("A")),
                multiplicative = ETS(Beer ~ error("M") + trend("A") +
                                         season("M"))) %>%
            forecast(h =12) %>%
            autoplot(aus_production)
    })
    output$arima_plot_1 <- renderPlot({
        aus_production %>%
        model(ARIMA(Beer ~ pdq(0, 1, 1) + PDQ(0, 1, 1))) %>%
            forecast(h =12) %>%
            autoplot(aus_production)
    })
    output$arima_plot_2 <- renderPlot({
        aus_production %>%
        model(ARIMA(Beer)) %>%
            forecast(h =12) %>%
            autoplot(aus_production)
    })
})


shinyApp(ui = ui, server = server)
