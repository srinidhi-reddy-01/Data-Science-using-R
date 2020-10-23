#Session 7

library(tidyverse)
library(janitor)
library(readxl)

dir(pattern = ".xlsx")

"covid_statewise_status_Oct17_from_mohfw_govt.xlsx" %>%
  read_excel() -> raw_data 

#raw_data %>% view()

c("s_no","state",
  "active_cumulative", "active_daily_increment",
  "cured_cumulative", "cured_daily_increment",
  "death_cumulative", "death_daily_increment") -> col_names_

#remove the untidy names
raw_data%>%
  slice(-c(1:2)) -> tidy_data


colnames(tidy_data) <- col_names_

#tidy_data %>% view()

tidy_data%>%
  mutate(across(-contains("state"), as.integer)) -> tidy_data

tidy_data %>% 
  arrange(-death_daily_increment) %>% 
  head(4) %>% 
  mutate(state = state %>% factor() %>% fct_reorder(-death_daily_increment)) %>% 
  ggplot(aes(x = state,
             y = death_daily_increment))+
  geom_col()


"https://www.dropbox.com/sh/y5d7p6zna23dhq5/AACcVUD2_4vMFIggBCJxLAFda?dl=0&preview=data_for_plot.csv?dl=1" %>%
  read_csv()%>% view()

#---------databases-----------

library(RSQLite)
library(DBI)

library(tidyverse)
library(janitor)
library(readxl)


con <- dbConnect(RSQLite::SQLite(), ":memory:")

con %>% 
  dbListTables()

con %>% 
  dbWriteTable("covid-oct17",tidy_data)

dbListTables(con)

con %>% 
  dbListFields("covid-oct17")

con %>% 
  tbl("covid-oct17") %>%
  arrange(-death_daily_increment) %>%
  head(4)%>%
  collect()

con%>%
  dbDisconnect()

#reference links
#https://db.rstudio.com/databases/sqlite/
#https://db.rstudio.com/getting-started/database-queries/

# WHat chart to use where?

# 1variable - numeric : histogram

tibble(col1 = rnorm(100, mean =5, sd = 1))%>%
  ggplot(aes(x = col1)) +
  geom_histogram(bins = 5)

# 1 variable categorical : bar plot
tibble(col1 = c("A","B","A","A","B"))%>%
  ggplot(aes(x=col1))+
  geom_bar()

tibble(x = -100:100,
       y = x^2)%>%
  ggplot(aes(x = x,
             y = y))+
  geom_point()


tibble(x = c(1:10),
       y = x^2 + rnorm(10, mean = 3, sd = 1))%>%
  ggplot(aes(x = x,
             y = y))+
  geom_point()+
  geom_smooth(method = "lm")

#2 variaboss : 1 categorical and other numeric
#column chart , box plot

#3  or more variables :
#color, shape, size, alpha, facet(multiple variables)

#Uncertainty visualization
#built in geoms 
#ggplot2 book

tibble(x = 1:3,
       y = c(15,10,11),
       error = c(1.1, 0.5, 1))%>%
  ggplot(aes(x =x,
             y= y,
             ymin = y - error,
             ymax = y + error))+
  #geom_ribbon(fill = "grey")+
  #geom_crossbar()+
  #geom_errorbar()+
  geom_linerange()+
  geom_label(aes(label = y))

#practice this!!!

# df %>%
#   ggplot(aes(x = x,
#              y = y))+
#   geom_point()
# 
# df %>%
#   ggplot(aes(x = x,
#              y = y))+
#   geom_point()

#POint path line area and polygon

tibble(x = c(3,2,5),
       y = c(7,8,9)) %>%
  ggplot(aes(x = x,
             y = y))
  #geom_point(size = 10, shape = 25, color = "red")+
  #geom_path()
  #geom_line()
  #geom_area(fill = "blue")+
  #geom_polygon()
  











