# Plot parameter 1 vs sector for both sources
# line chart 

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
       caption = "By Sirpi") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 16,
                                   face = "bold",
                                   angle = 90),
        axis.text.y = element_text(size = 16,
                                   face = "bold"),
        axis.title = element_text(size = 16,
                                  face = "bold"),
        plot.title = element_text(size = 24,
                                  face = "bold")) + 
  ggsave("param_source_comparison.pdf",
         width = 11,
         height = 8,
         unit = "in")
