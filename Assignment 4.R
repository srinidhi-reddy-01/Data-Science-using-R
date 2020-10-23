library(shiny)
library(ggplot2)
library(tidyverse)
library(tidyquant)
library(shiny)
library(shinythemes)
library(janitor)
library(shinycssloaders)
library(plotly)
library(ggplot2)
portfolio <- c(
  "ADANIPORTS.ns",
  "ASIANPAINT.ns",
  "AXISBANK.ns",
  "DIVISLAB.ns",
  "DRREDDY.ns",
  "EICHERMOT.ns",
  "GAIL.ns",
  "GRASIM.ns",
  "HCLTECH.ns",
  "HDFCBANK.ns",
  "HDFCLIFE.ns",
  "HEROMOTOCO.ns",
  "HINDALCO.ns",
  "HINDUNILVR.ns",
  "HDFC.ns",
  "ICICIBANK.ns",
  "ITC.ns",
  "IOC.ns",
  "INDUSINDBK.ns",
  "INFY.ns",
  "JSWSTEEL.ns",
  "KOTAKBANK.ns",
  "LT.ns",
  "M&M.ns",
  "MARUTI.ns",
  "NTPC.ns",
  "NESTLEIND.ns",
  "ONGC.ns",
  "POWERGRID.ns",
  "RELIANCE.ns",
  "SBILIFE.ns",
  "SHREECEM.ns",
  "SBIN.ns",
  "SUNPHARMA.ns",
  "TCS.ns",
  "TATAMOTORS.ns",
  "TATASTEEL.ns",
  "TECHM.ns",
  "TITAN.ns",
  "UPL.ns",
  "ULTRACEMCO.ns",
  "WIPRO.ns",
  "BAJAJ-AUTO.ns",
  "BAJFINANCE.ns",
  "BAJAJFINSV.ns",
  "BPCL.ns",
  "BHARTIARTL.ns",
  "BRITANNIA.ns",
  "CIPLA.ns",
  "COALINDIA.ns"
)


ui <- fluidPage(
  
  title = "Stock Data Moving average ",
  plotlyOutput("plot1"),
  
  hr(),
  
  fluidRow(
    
    
           selectInput("stock1", 'Choose a Stock',portfolio ),
           sliderInput('moving_avg1', 'Choose a Moving Average ',5,200,7),
           sliderInput('moving_avg2', 'Choose a Moving Average ',5,200,30),
           sliderInput('moving_avg3', 'Choose a Moving Average ',5,250,200)
    
  )
)



server <- function(input, output){
  
  output$plot1 <- renderPlotly({
    ggplotly(
      input$stock1%>%
        tq_get(get = "stock.prices",
               from = "2015-01-01",
               to = "2020-09-29") %>% 
        filter(adjusted > 0) %>% 
        tq_mutate(select = adjusted,
                  mutate_fun = SMA,
                  n = input$moving_avg1,
                  col_rename = "sma_1",
                  period = "daily") %>% 
        tq_mutate(select = adjusted,
                  mutate_fun = SMA,
                  n = input$moving_avg2,
                  col_rename = "sma_2",
                  period = "daily") %>% 
        tq_mutate(select = adjusted,
                  mutate_fun = SMA,
                  n = input$moving_avg3,
                  col_rename = "sma_3",
                  period = "daily") %>% 
        ggplot(aes(x = date,
                   y = adjusted)) + 
        geom_line() +
        geom_line(aes(y = sma_1), color = "blue") + 
        geom_line(aes(y = sma_2), color = "red") +
        geom_line(aes(y = sma_3), color = "brown")
    )
    
  })
  
}


shinyApp(ui, server)