# shiny dashboards
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "shiny dash") ,
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tab 1", tabName = "Section1"),
      menuItem("Tab 2", tabName = "Section2")
    )
  ) ,
  dashboardBody(
    tabItem(
      #Section 1 -----
      tabItem("Section1",
              fluidRow(
                box(
                  title = "Control slider",
                  sliderInput("slider1", "Num of rows:",1,100,50)
                ),
                box(plotOutput("plot1", height = 200))
                
              ))
      ),
    tabItem(
      
      tabItem("Section2",
              fluidRow(
                box(
                  title = "Control slider",
                  sliderInput("slider2", "Num of rows:",1,100,50)
                ),
                
                box(plotOutput("plot2", height = 200))
              ))
    )
    
    )
    
)

server <- function(input, output){
  output$plot1 <- renderPlot({
    
    iris %>%
      slice(1:input$slider1)%>%
      ggplot(aes(x = Petal.Length))+
      geom_histogram()
  })
  
  output$plot2 <- renderPlotly({
    
    iris %>%
      slice(1:input$slider2)%>%
      ggplot(aes(x = Petal.Length))+
      geom_histogram() %>% ggplotly()
    
  })
}

shinyApp(ui, server)
