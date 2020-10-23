# Row count of the tibble.
"raw_data_source_sector.csv" %>% 
  read_csv() %>% 
  nrow()

# View the tibble.
"raw_data_source_sector.csv" %>% 
  read_csv() %>% 
  View("raw_data")

# Slice rows
"raw_data_source_sector.csv" %>% 
  read_csv() %>%
  slice(1:20) 

# Slect columns
# Try ?select
"raw_data_source_sector.csv" %>% 
  read_csv() %>%
  select(contains("Param"))  

# Colnames 
"raw_data_source_sector.csv" %>% 
  read_csv() %>%
  colnames()

# Pull Unique 
"raw_data_source_sector.csv" %>% 
  read_csv() %>%
  pull(source) %>% 
  unique()

"raw_data_source_sector.csv" %>% 
  read_csv() %>%
  pull(direction) %>% 
  unique() %>% 
  length()



# filter and arrange by descending order 
# of the value in a particular column ?
"raw_data_source_sector.csv" %>% 
  read_csv() %>%
  filter(direction == "Sector1") %>% 
  arrange(-s_no) 


"raw_data_source_sector.csv" %>% 
  read_csv() %>% 
  select(all_of(c("s_no","direction","Parameter1"))) %>% 
  filter(Parameter1 > 0.2, direction == "Sector13") %>% 
  mutate(new_col = Parameter1 * 10) %>% 
  mutate(new_col2 = paste0(s_no,"-",Parameter1))


#mutate(parameter = Parameter1+ Parameter2)%>%
#unite( Parameter1, Parameter2)%>%
#columns to rowss


"raw_data_source_sector.csv" %>% 
  read_csv() %>% 
  #filter(source == "S1")%>% 
  rename(sector = direction)%>%
  
  #newww code function
  
  pivot_longer(c(Parameter1, Parameter2),
               names_to = "param",
               values_to = "value") %>%
  mutate(sector = sector %>% factor()%>% fct_inorder())%>%
  mutate(sector = sector %>% str_replace("Sector","")%>% parse_number())%>%
  
  ggplot(aes(x = param ,
             y = value)) +
  geom_jitter(aes(color = sector ),
              size = 10,
              position = position_jitter(width = 0.3 ,
                                         height = 0))+
  scale_color_continuous(low = "yellow",
                         high = "red")




"raw_data_source_sector.csv" %>%
  read_csv() %>%
  rename(sector = direction) %>%
  mutate(sector = sector %>% factor()%>%fct_inorder())%>%
  pivot_longer(c(Parameter1 , Parameter2),
               names_to = "param",
               values_to = "value")%>%
  ggplot(aes(x = sector,
             y = value))+
  geom_line(size = 4,
            aes(group = source)) +
  geom_point(size = 8,
             alpha = 0.5)+
  facet_wrap(source ~ param, scales = "fixed") #fixed




# Grouping and Summarizing
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


raw_data_tidy %>% 
  group_by(param,sector) %>% 
  summarise(mean_val = value %>% mean(),.groups = "drop") ##??
  
  
  
  
  
  
  