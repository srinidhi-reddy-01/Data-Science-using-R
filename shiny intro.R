#how to build dashboards

library(tidyverse)
library(shiny)
library(plotly)


ui <- fluidPage(
  sliderInput("myslider",
              "Number of points",
              0,10,5),
  textOutput("mytext"),
  selectInput("species",
              "Select Species",
              iris %>% pull(Species) %>% unique()),
  plotOutput("myplot")
)

server <- function(input , output, session){
  output$mytext <- renderPrint (
    paste0("The slider value is : ", input$myslider)
  )
  output$myplot <- renderPlot (
    iris %>%
      filter(Species == input$species )%>%
      ggplot(aes(x = Petal.Length,
                 y = Petal.Width))+
      geom_point(size = 5) 
    
  )
  
}

shinyApp(ui = ui, server = server)

