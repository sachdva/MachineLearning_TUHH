# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)
library(quantmod)
library(lubridate)

source(file = "stock_analysis_functions.R")


# UI ----
ui <- fluidPage(
  title = "Stock Analyzer",
  

  # 1.0 HEADER ----
    div(
      h1("Stock Analyzer"),
      p("This is my second shiny project")
    ),
  
  # 2.0 APPLICATION UI -----
  div(
    column(
      width = 4, multiple = F, actionsBox = FALSE, liveSearch = TRUE, size = 10,
      wellPanel(
        
        # Add content here 
        pickerInput(inputId = "market", 
                    label   = h5("Stock Index"),
                    choices = c("DOW", "SP500", "DAX", "NASDAQ")),
        h5("Stock List (Pick One to Analyze)"),
        uiOutput("indices"),  
        dateRangeInput(inputId = "date_range", 
                       label   = h4("Date Range"), 
                       start   = today() - days(180), 
                       end     = today(),
                       startview = "year"),
        
        actionButton(inputId = "analyze", 
                     label   = "Analyze", 
                     icon    = icon("download")), 
        sliderInput(inputId = "shortavg", 
                    label   = h4("Short Moving Average"), 
                    min     = 5,
                    max     = 40, 
                    value   = c(20), # Initial values of the slider
                    step    = 1, # Interval between each selectable value
                    round   = TRUE, # Round to the nearest integer
                    ), 
        sliderInput(inputId = "longavg", 
                    label   = h4("Long Moving Average"), 
                    min     = 50,
                    max     = 120, 
                    value   = c(50), # Initial values of the slider
                    step    = 1, # Interval between each selectable value
                    round   = TRUE, # Round to the nearest integer
                    )
        
        
        
        
        
        
        
      )
    ), 
    column(
      width = 8,
      div(
        
        div(h4(textOutput("stockname"))),
        
        div(plotlyOutput("plot"))
        
      )
    ), 
    
    column(
      width = 12,
      div(h4("Analyst Commentary")),
      div(textOutput("acomm"))
      
      )
  )
)

    # 3.0 ANALYST COMMENTARY ----

# SERVER ----
server <- function(input, output, session) {
  output$indices <- renderUI({
    stock_list_tbl <- get_stock_list(input$market)
    pickerInput(inputId = "stock_selection",
                choices = stock_list_tbl %>% purrr::pluck("label")
    )
  })
  
  
  #Make a function that like apply in the last exercise, use functions 
  # get_stock_list done above 
  # get_symbol_from_user_input
  # get_stock_data
  # plot_stock_data
  # to make a table and plot
  apply <- function(){
    # Stock Symbol ----
    stock_symbol <- eventReactive(input$analyze, {
      input$stock_selection
    }) # I don't understand this line
    
    #change some stuff
    symbol <- get_symbol_from_user_input(stock_symbol())
    stkdata <- get_stock_data(symbol, 
                        from = input$date_range[1], 
                        to   = input$date_range[2], 
                        input$shortavg, input$longavg)
      
    #give some outputs
    output$stockname <- renderText(stock_symbol())
    output$plot <- renderPlotly(plot_stock_data(stkdata))
    output$acomm <- renderText(generate_commentary(stkdata,symbol))
    
    }
  
    
    #NOW apply will apply when TOUCHED
    observeEvent(eventExpr = input$analyze, handlerExpr = {
      apply()
    })
    observeEvent(eventExpr = input$shortavg, handlerExpr = {
      apply()
    })
    observeEvent(eventExpr = input$longavg, handlerExpr = {
      apply()
    })
    observeEvent(eventExpr = input$date_range, handlerExpr = {
      apply()
    })
  
  
  
}
# RUN APP ----
shinyApp(ui = ui, server = server)
