

"openaq.csv" %>% 
  read_csv() %>% 
  dim()

"openaq.csv" %>% 
  read_csv() %>% 
  select(all_of(c("location","city","local","parameter","value","unit"))) %>% 
  rename(ts = local) %>% # pull(city) %>% unique()
  filter(city %in% c("Bengaluru","Chennai","Navi Mumbai","Jodhpur")) %>% 
  mutate(year = ts %>% year(),
         month = ts %>% month(),
         day = ts %>% day()) %>% 
  filter(day == 26) %>% 
  group_by(city,parameter) %>% 
  summarise(avg_value = value %>% mean() %>% round(1),.groups = "drop") %>% 
  ggplot(aes(x = city,
             y = avg_value,
             fill = city)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~ parameter, scales = "free", ncol = 2) + 
  theme_minimal()

# fimd pm25 <0 in the raw data 

"openaq.csv" %>% 
  read_csv() %>% 
  select(all_of(c("location","city","local","parameter","value","unit"))) %>% 
  rename(ts = local) %>% # pull(city) %>% unique()
  filter(city %in% c("Bengaluru","Chennai","Navi Mumbai","Jodhpur")) %>% 
  filter(parameter == "pm25") %>% 
  filter(value <= 0) %>% View()


# filter -ve values
"openaq.csv" %>% 
  read_csv() %>% 
  select(all_of(c("location","city","local","parameter","value","unit"))) %>% 
  rename(ts = local) %>% # pull(city) %>% unique()
  filter(city %in% c("Bengaluru","Chennai","Navi Mumbai","Jodhpur")) %>% 
  mutate(year = ts %>% year(),
         month = ts %>% month(),
         day = ts %>% day()) %>% 
  filter(day == 26,
         value > 0) %>% 
  group_by(city,parameter) %>% 
  summarise(avg_value = value %>% mean() %>% round(1),.groups = "drop") %>% 
  ggplot(aes(x = city,
             y = avg_value,
             fill = city)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~ parameter, scales = "free", ncol = 2) + 
  theme_minimal()

# make NA and drop
"openaq.csv" %>% 
  read_csv() %>% 
  select(all_of(c("location","city","local","parameter","value","unit"))) %>% 
  rename(ts = local) %>% # pull(city) %>% unique()
  filter(city %in% c("Bengaluru","Chennai","Navi Mumbai","Jodhpur")) %>% 
  mutate(year = ts %>% year(),
         month = ts %>% month(),
         day = ts %>% day()) %>% 
  filter(day == 26) %>%
  mutate(value = ifelse(value < 0, NA,value)) %>% 
  drop_na() %>% 
  group_by(city,parameter) %>% 
  summarise(avg_value = value %>% mean() %>% round(1),.groups = "drop") %>% 
  ggplot(aes(x = city,
             y = avg_value,
             fill = city)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~ parameter, scales = "free", ncol = 2) + 
  theme_minimal()

# Modal Analysis using geom_density
"openaq.csv" %>% 
  read_csv() %>% 
  select(all_of(c("location","city","local","parameter","value","unit"))) %>% 
  rename(ts = local) %>% # pull(city) %>% unique()
  filter(city %in% c("Bengaluru","Chennai","Navi Mumbai","Jodhpur")) %>%
  ggplot(aes(x = value)) + 
  geom_density()

"openaq.csv" %>% 
  read_csv() %>% 
  select(all_of(c("location","city","local","parameter","value","unit"))) %>% 
  rename(ts = local) %>% # pull(city) %>% unique()
  filter(city %in% c("Bengaluru","Chennai","Navi Mumbai","Jodhpur")) %>%
  filter(value > 0 & value < 1000) %>% 
  ggplot(aes(x = value)) + 
  geom_density() + 
  facet_wrap(~ parameter + city, scales = "free", ncol = 4, drop = FALSE)





# Stock market topic 
library(tidyquant)
library(plotly)
library(visdat)

"aapl" %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2020-09-23") %>% 
  ggplot(aes(x = date,
             y = adjusted)) + 
  geom_line() + 
  theme_tq() -> p1

p1 %>% 
  ggplotly()

portfolio <- c("aapl","fb","tsla")

portfolio %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2020-09-23") %>% 
  ggplot(aes(x = date,
             y = adjusted)) + 
  geom_line(aes(color = symbol)) + 
  labs(title = portfolio %>% paste(collapse = ","),
       color = "symbol") +
  theme_tq() -> p2


p2 %>% 
  ggplotly()

# squared <- function(x){
#   
#   x^2
# }
# 
# 25%>%
#   squared()
stock%>% View()
# Write a function to pull a stock 

pull_stock <- function(stock)
  {
   stock%>% 
    tq_get(get = "stock.prices",
           from = "2010-01-01",
           to = "2020-09-23") %>% 
    ggplot(aes(x = date,
               y = adjusted)) + 
    geom_line() + 
    theme_tq()
}

max_stock_val <- function(stock){
  stock %>% 
    tq_get(get = "stock.prices",
           from = "2010-01-01",
           to = "2020-09-23") %>% 
    pull(adjusted) %>% 
    max()
}

"aapl" %>% 
  pull_stock() %>% 
  ggplotly()


"aapl" %>% 
  max_stock_val()

"fb" %>% 
  max_stock_val()

portfolio %>% 
  map_dbl(max_stock_val)

tibble(portfolio = portfolio,
       max_stock_val_10yr = portfolio %>% 
         map_dbl(max_stock_val)) %>% view()
  mutate(portfolio = portfolio %>% factor() %>% fct_reorder(-max_stock_val_10yr)) %>% 
  ggplot(aes(x = portfolio,
             y = max_stock_val_10yr)) + 
  geom_col(aes(fill = portfolio))


"tatasteel.ns" %>% 
  tq_get(get = "stock.prices",
         from = "2015-01-01",
         to = "2020-09-01") %>% 
  drop_na() %>% 
  tq_mutate(select = adjusted,
            mutate_fun = SMA,
            n = 7,
            col_rename = "sma_7",
            period = "daily") %>% 
  tq_mutate(select = adjusted,
            mutate_fun = SMA,
            n = 50,
            col_rename = "sma_50",
            period = "daily") %>% 
  tq_mutate(select = adjusted,
            mutate_fun = SMA,
            n = 200,
            col_rename = "sma_200",
            period = "daily") %>% 
  ggplot(aes(x = date,
             y = adjusted)) + 
  geom_line() + 
  geom_line(aes(y = sma_7), color = "blue") + 
  geom_line(aes(y = sma_50), color = "red") +
  geom_line(aes(y = sma_200), color = "brown") -> p3

p3 %>% 
  ggplotly()
 