library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)
library(shinycssloaders)
library(plotly)
library(ggplot2)




ui <- fluidPage(
  
  titlePanel("Sirpi Data science using R"),
  
  navlistPanel(
    "Session 1 ",
    tabPanel("Classwork 1",
             plotOutput("livesess_1")),
    tabPanel("Assignment 1",
             plotOutput("assignment1")),
    
    "Session 2 ",
    tabPanel("Classwork 2",
             plotOutput("livesess_2")),
    tabPanel("Assignment 2",
             plotOutput("assignment2a"),
             plotOutput("assignment2b")),
    
    "Session 3",
    tabPanel("Classwork 3")
  )
)


server <- function(input,output){
  
  #live session 1
  
  output$livesess_1 <- renderPlot ({
    
    "raw_data_source_sector.csv" %>% 
      read_csv() %>% 
      clean_names() %>% 
      rename(sector = direction)  %>% 
      mutate(sector = sector %>% factor() %>% fct_inorder()) %>% 
      ggplot(aes(x = sector,
                 y = parameter1)) + 
      geom_line(aes(group = source,
                    color = source,
                    linetype = source),
                size = 3) + 
      geom_point(aes(shape = source,
                     color = source),
                 size = 7,
                 alpha = 0.5) + 
      labs(x = "Sector",
           y = "Param 1",
           title = " Param 1 source S1 S2 Comparisons",
           #subtitle = "data source : zbc",
           caption = "By Srinidhi") + 
      theme_minimal() + 
      theme(axis.text.x = element_text(size = 16,
                                       face = "bold",
                                       angle = 90),
            axis.text.y = element_text(size = 16,
                                       face = "bold"),
            axis.title = element_text(size = 16,
                                      face = "bold"),
            plot.title = element_text(size = 24,
                                      face = "bold"))
    
  })
  
  
  #assignment 1
  
  output$assignment1 <- renderPlot({
    "raw_data_source_sector.csv" %>%
      read_csv()%>%
      clean_names()%>%
      rename(sector = direction)%>%
      mutate(sector = sector %>% factor()%>% fct_inorder(),
             parameter3 = parameter1 - parameter2)%>%
      filter(parameter3 > 0 ,
             source == "S1" )%>%
      ggplot(aes(x = fct_reorder(sector, parameter3) ,
                 y = parameter3,
                 label = round(parameter3, digits = 2)))+
      geom_col()+
      
      labs(x = "Sector",
           y =  "Param 3",
           title = "Param 3",
           subtitle = "filter (param 3 > 0 ) , source == S1",
           caption = "By Srinidhi ")+ 
      theme_minimal()+
      theme(axis.text.x = element_text(size = 10,
                                       face = "bold",
                                       angle = 90),
            axis.text.y = element_text(size = 10,
                                       face = "bold"),
            axis.title.x =element_text(size = 15,
                                       face = "bold"),
            axis.title.y =element_text(size = 15,
                                       face = "bold"))+
      geom_label()+
      
      geom_hline(aes (yintercept = 0.05),
                 colour="#990000", linetype="dashed")
    
  })
    
    #live session 2
    
    output$livesess_2 <- renderPlot({
      
      "raw_data_source_sector.csv" %>% 
        read_csv() %>% 
        rename(sector = direction) %>% 
        mutate(sector = sector %>% factor() %>% fct_inorder()) %>% 
        pivot_longer(c(Parameter1,Parameter2),
                     names_to = "param",
                     values_to = "value") %>% 
        mutate(sector = sector %>% str_replace("Sector","") %>% parse_number()) -> raw_data_tidy
      
      
      raw_data_tidy %>% 
        group_by(param) %>% 
        summarise(mean_val = value %>% mean(),.groups = "drop") -> by_param_mean_raw_data_tidy
      
      
      ggplot() + 
        geom_jitter(data = raw_data_tidy,
                    aes(x = param,
                        y = value,
                        color = sector),
                    size = 4,
                    alpha = 0.6,
                    position = position_jitter(height = 0,
                                               width = 0.2)) + 
        scale_color_continuous(low = "yellow",
                               high = "red") + 
        geom_label(data = by_param_mean_raw_data_tidy,
                   aes(x = param,
                       y = mean_val,
                       label = mean_val %>% round(2))) + 
        theme_minimal()
      
    })
    
    
    #assignment 2
    
    output$assignment2a <- renderPlot({
      
      
      "AAPL_TSLA_Jul_Aug_2020.csv" %>%
        read_csv() %>%
        clean_names() %>%   
        group_by(symbol) %>% 
        mutate(mean_vol_millions = volume%>% mean()/1e6) %>%
        select(symbol,mean_vol_millions)%>%
        group_by(symbol)%>%
        unique()-> new_tidy
      
      
      new_tidy%>%
        ggplot(aes(x = symbol,
                   y = mean_vol_millions  ))+
        geom_col()+
        geom_label(aes(label=paste0(mean_vol_millions %>% round(1),"M"))) +
        labs(x = "symbol",
             y =  "mean_vol_millions",
             title = "Mean volume of trades AAPL & TSLA ",
             subtitle = "July and August 2020 ",
             caption = "By Srinidhi ")
      
      
    })
    
    output$assignment2b <- renderPlot({
      
      
      "AAPL_TSLA_Jul_Aug_2020.csv" %>%
        read_csv() %>% 
        clean_names() %>% 
        group_by(symbol) %>%
        pivot_longer(c(open,close,high,low),
                     names_to = "measure",
                     values_to = "price")%>% 
        group_by(measure) %>% 
        ggplot(aes(x = date,
                   y = price)) +
        geom_line(aes(colour = measure),
                  size = 1.5)+
        facet_wrap(symbol~measure,ncol=4)
      
      
    })
    
  
}


shinyApp(ui,server)
