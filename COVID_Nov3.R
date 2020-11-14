library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(gganimate)
library(gifski)
library(png)
library(ggrepel)
library(scales)
#change the path below to the address to the file on your computer.

path <- "~/Desktop/Teaching/R_Scripts/Covid_Project/COVID-19-master_Nov3/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
df_1 <- read.csv(path)
head(df_1)

covid_full_1 <- df_1 %>% select(c(7, 12:297)) 
library(janitor)
covid_map_state_1 <- covid_full_1 %>% group_by(Province_State) %>% 
  summarize_at(vars(2:286), sum) %>% adorn_totals('row')
covid_map_state_1$Province_State <- tolower(covid_map_state_1$Province_State)

#unique(covid_map_full$Province_State)
us_states <- map_data("state")
cont_states <- unique(us_states$region)

covid_cont_states_1 <- covid_map_state_1 %>% 
  filter(Province_State %in%  cont_states) %>%
  dplyr::rename(region = Province_State)

col_names_1<- colnames(covid_cont_states_1)[c(2:286)]
new_col_names_1 <- seq(as.Date("2020/1/21"), by = "day", length.out = 286) #get dates in date-time format

covid_cont_states_1 <- covid_cont_states_1 %>% 
  setNames(new_col_names) %>%
  dplyr::rename(region = "2020-01-21")




#Prepare data for plotting
covid_cont_state.long_1 <- pivot_longer(covid_cont_states_1, 
                                      cols = c(2:286), 
                                      names_to = "date", 
                                      values_to = "cases")


#Choose states to plot
plot_states <- c("new york", "florida", "texas", "california")

#data for plotting specific states
covid_plot_data_1 <- covid_cont_state.long_1 %>% filter(region == plot_states)

#Plot the data
q_1 <- covid_plot_data_1 %>% 
  ggplot(aes(x = as.Date(date), 
             y = cases, 
             group = region,
             color = region))+
  labs(title = "Time Series Plot of Confirmed Cases of COVID",
       subtitle = "Jonathan Fernandes PhD, University of Maryland, College Park.",
       x = "Date", 
       y = "Number of Cases")+
  geom_line()+
  geom_point()+
  geom_text(aes(label = factor(region)), 
            hjust = 0, 
            position = position_dodge(width=0.9),  
            size=4)+
  scale_x_date(limits = as.Date(c("2020-1-22","2020-11- 02")), 
               date_breaks = "1 month",
               date_labels="%B")+
  scale_y_continuous(labels = scales::comma)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10),
    axis.title.y = element_text(color="black", size=10),
    legend.position = "none")#+

q_1

p_1 <- covid_plot_data_1 %>% 
  ggplot(aes(x = as.Date(date), 
             y = cases, 
             group = region,
             color = region))+
  labs(title = "Time Series Plot of Confirmed Cases of COVID",
       subtitle = "Jonathan Fernandes PhD, University of Maryland, College Park.",
       x = "Date", 
       y = "Number of Cases")+
  geom_line()+
  scale_x_date(limits = as.Date(c("2020-1-22","2020-11- 02")), 
               date_breaks = "1 month",
               date_labels="%B")+
  scale_y_continuous(labels = scales::comma)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10),
    axis.title.y = element_text(color="black", size=10))
  
p_1  
  
q_1 <- q_1 +  theme(
  #panel.background = element_rect(fill = "white"),
  plot.margin = margin(5,10 ,5, 1, "mm"),
  plot.background = element_rect(
    fill = "white",
    colour = "black",
    size = 1
  )
)


anim_1 <- q_1 + transition_reveal(as.Date(date))
animation_1 <- animate(anim_1,
                     width = 750, 
                     height = 650,
                     duration = 20,
                     end_pause = 20,
                     renderer = gifski_renderer())

animation_1
#facet_grid(~region)


