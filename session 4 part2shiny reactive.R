

# shiny : reactive programming
library(shiny)
library(tidyverse)
library(DT)

# UI ----
ui <- fluidPage(# App title ----
                titlePanel("Hello Shiny!"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    selectInput(
                      "species",
                      "Select Species",
                      choices = iris %>% pull(Species) %>% unique()
                    )
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(plotOutput("iris_plot"),
                            verbatimTextOutput("max_petal_length"),
                            DTOutput("iris_table"))
                ))
# ----

server <- function(input, output){
  
  filtered_iris <- reactive({
    iris %>% 
      filter(Species == input$species)
  })
  
  output$iris_plot <- renderPlot({
    filtered_iris() %>% 
      ggplot(aes(x = Petal.Length,
                 y = Petal.Width)) + 
      geom_point(size = 5)
  })
  
  output$iris_table <- renderDT({
    filtered_iris()
  })
  
  compute_filtered_iris_max_petal_length <- reactive({
    filtered_iris() %>% 
      pull(Petal.Length) %>% 
      max()
  })
  
  output$max_petal_length <- renderPrint({
    compute_filtered_iris_max_petal_length()
  })
}

shinyApp(ui, server)