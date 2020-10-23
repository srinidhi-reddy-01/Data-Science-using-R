# session 6


library(tidyverse)
library(janitor)
library(lubridate)
library(tidygraph)
library(ggraph)
library(leaflet)


dir(".csv")

"cable_vendor_data_with_timestamp.csv" %>%
  read_csv()%>%
  ggplot(aes(x = cable_length_in_cms))+
  geom_histogram(binwidth = 0.1,
                 aes(fill = vendor))+
  geom_vline(xintercept = 10,
             color = "red",
             linetype = 2)
"cable_vendor_data_with_timestamp.csv" %>%  
  read_csv()%>% 
  mutate(year = ts %>% year(),
         month = ts %>% month(),
         day = ts %>% day(),
         hour = ts %>% hour(),
         minute = ts %>% minute(),
         second = ts %>% second(),
         day_of_week_num = ts %>% wday(),
         day_of_week = ts %>% wday(label = TRUE)) %>%
  ggplot(aes(x = cable_length_in_cms,
             fill = vendor))+
  geom_histogram(binwidth = 0.1,
                 aes(fill = vendor))+
  geom_vline(xintercept = 10,
             color = "red",
             linetype = 2)+
  facet_wrap(~minute, ncol = 5, scales = "fixed")
  

# tidygraph
tibble(col1 = 1:15)
tribble(~ A, ~ B,
        1, 2,
        3, 4)

tribble(~ from, ~ to, ~ year,
        "India", "USA", "2000",
        "USA", "Canada", "2000",
        "USA", "Switzerland", "2005",
        "Switzerland", "India", "2005",
        "India", "Japan", "2010",
        "USA", "China", "2010",
        "India", "Malaysia","2010") %>% 
  as_tbl_graph() %>% 
  ggraph() + 
  geom_edge_link(aes(colour = year),
                 arrow = arrow()) + 
  geom_node_point(size = 20, alpha = 0.5,
                  color = "yellow") + 
  geom_node_text(aes(label = name),
                 colour = "blue") + 
  theme_graph()

tribble(~ from, ~ to, ~ year,
        "India", "USA", "2000",
        "USA", "Canada", "2000",
        "USA", "Switzerland", "2005",
        "Switzerland", "India", "2005",
        "India", "Japan", "2010",
        "USA", "China", "2010",
        "India","Malaysia","2010") -> A

tribble(~ from, ~ to, ~ year,
        "India", "Canada", "2000",
        "USA", "Canada", "2000",
        "USA", "India", "2005",
        "Switzerland", "India", "2005",
        "India", "China", "2010",
        "USA", "China", "2010",
        "China","Malaysia","2010") -> B

A %>% 
  mutate(traveller = "A") %>% 
  as_tbl_graph() %>% 
  mutate(name = name) -> Agraph

B %>% 
  mutate(traveller = "B") %>% 
  as_tbl_graph() %>% 
  mutate(name = name) -> Bgraph

# Display all nodes of A and B
# What countries ?
Agraph %>% 
  graph_join(Bgraph) %>% 
  ggraph() + 
  geom_node_point(size = 20, alpha = 0.3) + 
  geom_node_text(aes(label = name)) + 
  theme_graph()

# What trips were made
Agraph %>% 
  graph_join(Bgraph) %>% 
  ggraph() + 
  geom_edge_arc(arrow = arrow(),
                colour = "brown") + 
  geom_node_point(size = 20, 
                  alpha = 0.3,
                  aes(colour = name)) + 
  geom_node_text(aes(label = name),
                 size = 8) + 
  theme_graph()

# who made the trip when ?
Agraph %>% 
  graph_join(Bgraph) %>% 
  ggraph() + 
  geom_edge_arc(arrow = arrow(),
                aes(colour = year,
                    linetype = traveller)) + 
  geom_node_point(size = 20, 
                  alpha = 0.3,
                  aes(colour = name)) + 
  geom_node_text(aes(label = name),
                 size = 8) + 
  theme_graph()

Agraph %>% 
  graph_join(Bgraph) %>% 
  ggraph() + 
  geom_edge_arc(arrow = arrow(),
                aes(colour = year,
                    linetype = traveller)) + 
  geom_node_point(size = 20, 
                  alpha = 0.3,
                  aes(colour = name)) + 
  geom_node_text(aes(label = name),
                 size = 8) + 
  theme_graph() + 
  facet_wrap(~ traveller)

Agraph %>% 
  graph_join(Bgraph) %>% 
  ggraph() + 
  geom_edge_arc(arrow = arrow(),
                aes(colour = year,
                    linetype = traveller)) + 
  geom_node_point(size = 20, 
                  alpha = 0.3,
                  aes(colour = name)) + 
  geom_node_text(aes(label = name),
                 size = 4) + 
  facet_wrap(year ~ traveller) +
  theme_graph(foreground = "grey") 

Agraph %>% 
  graph_join(Bgraph) %>% 
  ggraph() + 
  geom_edge_arc(arrow = arrow(),
                aes(colour = year,
                    linetype = traveller)) + 
  geom_node_point(size = 20, 
                  alpha = 0.3,
                  aes(colour = name)) + 
  geom_node_text(aes(label = name),
                 size = 4) + 
  facet_wrap(year ~ traveller, nrow = 1) +
  theme_graph(foreground = "grey") 

# Finer customizations
Agraph %>% 
  graph_join(Bgraph) %>% 
  ggraph(layout = "linear",
         circular = TRUE) +
  geom_edge_link(arrow = arrow(length = unit(5,"mm")),
                 end_cap = circle(10, 'mm'),
                 start_cap = circle(10, 'mm'),
                 width = 1.5,
                 alpha = 0.8,
                 aes(colour = year, linetype = traveller)) +
  geom_node_point(size = 20, alpha = 0.2, colour = "lightblue") +
  geom_node_label(aes(label = name),colour = "darkgreen") +
  theme_graph() +
  scale_edge_colour_manual(values = c("pink", "magenta","darkblue"))

 # ifelse logic inside tidyverse pipeline
iris %>% 
  mutate(include = ifelse(Petal.Length > 5, "Inc", "Exc")) %>% view()

# random normal distributions
# samplings and cut intervals

tibble(x = rnorm(100, mean = 6, sd = 2)) %>% 
  mutate(x_bin = x %>% cut_interval(10)) %>%
  group_by(x_bin) %>% 
  summarise(count_x = n(),
            mean_x = x %>% mean(na.rm = TRUE),
            .groups = "drop") %>% 
  ggplot(aes(x = x_bin,
             y = count_x)) + 
  geom_col() + 
  theme(axis.text.x = element_text(size = 20))


tibble(x = sample(1:1000,100)) %>% 
  mutate(x_bin = x %>%
           cut_interval(10)) %>% 
  group_by(x_bin) %>% 
  summarise(count_x = n(),
            mean_x = x %>% 
              mean(na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(include = ifelse(count_x > 10,"in","out")) %>% 
  ggplot(aes(x = x_bin,
             y = count_x)) + 
  geom_col(aes(fill = include)) + 
  theme(axis.text.x = element_text(size = 20)) + 
  geom_hline(yintercept = 10,
             color = "red",
             linetype = 2)

# Geographical Maps
"openaq.csv" %>% 
  read_csv() %>% 
  select(longitude,latitude) %>% 
  distinct() %>% view()
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lat = ~latitude,
             lng = ~longitude)
  
  
#view countries etc  Map box

  
  
  
  
  
  
  
  

  
  
  
  
  

