#assignment 2

#part1

"AAPL_TSLA_Jul_Aug_2020.csv" %>%
  read_csv() %>%
  clean_names() %>%   #view()
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

#part2

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
  #scale_x_date(breaks = "one month")
  

#sir code

"AAPL_TSLA_Jul_Aug_2020.csv" %>% 
  read_csv() %>% 
  select(-volume) %>% 
  pivot_longer(c(open,close,low,high),
               names_to = "measure",
               values_to = "price") %>% 
  ggplot(aes(x = date,
             y = price)) + 
  geom_line(aes(color = measure),
            size = 2) +
  facet_wrap(symbol~measure,ncol = 4) +
  scale_x_date(breaks = "1 month") + 
  labs(title = "AAPL and TSLA Bull Run : 2020") 


  



  #aes(label = mean_vol_millions)