library(plotly)
library(ggplot2)
library(tidyverse)
library(janitor)
#how to use view
#how to use mutate
#how to use 

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


  ggsave("param_3_assignment.pdf",
         width = 11,
         height = 8,
         unit = "in")


