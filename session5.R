#Session 5

library(tidyverse)
library(conflicted)
library(gganimate)
library(gifski)
theme_set(theme_bw())
library(visdat)

conflict_prefer("filter", "dplyr")

#cricket data from a recent IPL match


file.choose()%>%
  read.csv(n_max = 20,
           col_names = c("col1","key","value")) ->raw_data



dir()


file.choose()%>%
  read_csv(skip = 20,
           col_names = c("col1",
                         "innings",
                         "over_and_ball",
                         "batting_team",
                         "striker",
                         "non-skriker",
                         "bowler",
                         "runs_off_bat",
                         "extras",
                         "kind_of_wicket", 
                         "dismissed_player")) -> raw_data


#make tidy data



raw_data %>%
  separate(over_and_ball, into = c("over","ball"), convert = TRUE) %>%
  mutate(runs = runs_off_bat + extras)%>%
  mutate(over = over + 1) %>%
  group_by(batting_team, over) %>%
  mutate(cumm_runs_over_wise = cumsum(runs)) %>% 
  ungroup()%>%
  group_by(batting_team) %>%
  mutate(cumm_runs_team_wise = cumsum(runs)) %>%
  #filter(batting_team == "Kings XI Punjab ")
  ungroup() -> TIDY_DATA_IPL



"bowler_country.csv" %>%
  read_csv() -> bowler_country




TIDY_DATA_IPL %>%
  left_join(bowler_country) %>% 
  rename( "bowler_country" = "country" ) -> tidy_data_extracols 


tidy_data_extracols%>% 
  view()

tidy_data_extracols %>%
  vis_dat()    #coooool  to see datatype

tidy_data_extracols %>%
  vis_miss() 

tidy_data_extracols %>%
  drop_na() %>% view()

tidy_data_extracols %>%
  drop_na() %>% view()

tidy_data_extracols %>%
  pull(striker) %>% unique()

tidy_data_extracols %>%
  filter(striker == "R Tewatia")%>%
  mutate(ball_number = row_number())%>%
  ggplot(aes(x = ball_number,
             y = runs))+
  geom_col(fill = "pink")


tidy_data_extracols %>%
  ggplot(aes(x = ball,
             y = cumm_runs_over_wise,
             color = batting_team))+
  geom_step(alpha = 0.7, size = 2)+
  facet_wrap(~over, nrow = 1, scales = "free_x")+
  theme_minimal()+
  theme(title = element_text(face = "bold",
                             size = 24),
        axis.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))+
  scale_color_manual(values = c ("red", "green"), labels = c("KXIP","RR"))+
  labs() -> animate_plot




animate_plot1 <- ggplot( data = tidy_data_extracols,aes(x = ball,
                           y = cumm_runs_over_wise,
                           color = batting_team))+
  wormplot_gg(alpha = 0.7, size = 2)+
  facet_wrap(~over, nrow = 2, scales = "free_x")+
  theme_minimal()+
  theme(title = element_text(face = "bold",
                             size = 24),
        axis.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))+
  scale_color_manual(values = c ("red", "green"), labels = c("KXIP","RR"))+
  labs()

animate_plot1

output1.gif <- animate_plot1+ transition_reveal(ball)

output1.gif

animate(output1.gif,end_pause = 12,duration = 6, fps = 20, width = 1366, height = 657, renderer = gifski_renderer())

anim_save("output1.gif")




