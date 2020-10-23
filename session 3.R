#session3

library(janitor)
library(tidyverse)
library(ggplot2)
library(tidytext)

#plot the mean sepal_length sepal_width petal_length petal_width for all unique species
#for all the species



iris%>%
  as_tibble()%>%
  clean_names()%>%
  group_by(species)%>%
  mutate(s_len_mean = sepal_length%>% mean(),
         s_wid_mean = sepal_width%>% mean(),
         p_len_mean = petal_length%>% mean(),
         p_wid_mean = sepal_width%>% mean())%>%
  select(-sepal_length, -sepal_width, -petal_length, -petal_width)%>%
  unique()%>%
  pivot_longer(c(s_len_mean,s_wid_mean,p_len_mean,p_wid_mean),
               names_to = "sep_pet",
               values_to = "meanval")%>% 
  group_by(species)%>% 
  ggplot(aes(x = species %>% reorder_within (-meanval,sep_pet) ,
             y = meanval,
             color = species,
             label = round(meanval, digits = 2))) +
  geom_col(aes(color = species,
               fill = species))+
  theme_minimal()+
  geom_label()+
  facet_wrap(~sep_pet,ncol = 2, scales = "free") +
  labs(x = "species",
       y =  "mean value",
       title = "Sess 3",
       subtitle = "Iris analysis",
       caption = "By Srinidhi ")+
  theme(axis.text.x = element_text(size = 5.5,
                                   face = "bold",
                                   angle = 0),
        axis.text.y = element_text(size = 10,
                                   face = "bold"),
        axis.title.x =element_text(size = 15,
                                   face = "bold"),
        axis.title.y =element_text(size = 15,
                                   face = "bold"))


#how to writa a function

square_it <- function(x){
  
  x^2
}

5 %>% square_it()

1:20 %>% square_it()

sincx <- function(x){
  sin(x)/x
}    

tibble(x = c(-360:360)*3,
       x_rad = x/360*pi,
       sin = sin(x_rad),
       cos = cos(x_rad),
       sinc = sincx(x_rad)) %>%
  pivot_longer(-c(1:2),
               names_to = "trig_fn",
               values_to = "value")%>%
  ggplot(aes(x = x,
             y = value))+
  geom_line()+
  facet_wrap(~trig_fn, scales = "free", ncol = 1)+
  geom_vline(xintercept = seq(-360 *3 ,360*3, by = 360),
             colour = "red")

    
    
    
