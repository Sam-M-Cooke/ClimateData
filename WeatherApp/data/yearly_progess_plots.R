library(tidyverse)
library(showtext)


#font_add_google(name = "Rubik", family = "rubik") # if you need to install the font
showtext_auto()

colour <- c("#BCD2E8",'#91BAD6', '#73A5C6', '#528AAE','#2E5984', '#1E3F66')
heat <- c('#B31313','#FF9000','#FDDA16','#FFEE82')

theme_set(theme_minimal())
theme_replace(text = element_text(family = "Rubik", size = 20, colour = colour[5]),
              strip.text = element_text(hjust = 0))



#scrape the data from the met office
source("~/Climate/WeatherApp/scrape.r")

weatherdata %>% 
  filter(year == 1910 & station != 'Southampton') %>% 
  select(station) %>% 
  unique() %>% 
  pull()-> stations

#select Sheffield as the station of choice
weatherdata %>% filter(station == 'Sheffield') -> sheffield


sheffield %>% 
  mutate(month = lubridate::month(month, label = TRUE)) %>% 
  group_by(year) %>% 
  mutate(ytd_rain = cumsum(rain_mm)) %>%
  ggplot(aes(x=month, y=ytd_rain, group = year))+
  geom_line(alpha = 0.25, size = 1, color = colour[2])+
  geom_line(data = .%>% filter(year == 2022),aes(x=month, y=ytd_rain), colour = colour[4], size = 1)+
  geom_text(data = . %>% filter(year == 2022, month == 'Jul'), aes(x= month, y= ytd_rain, label = year), 
            nudge_x = .75, nudge_y = 20, colour = colour[4], size = 15)+
  labs(title = "Cumulative rainfall in Sheffield, monthy by month",
       subtitle = "Is 2022 unusually dry?",
       caption = "@CookeComms | MetOffice weather station data")+
  xlab("Month")+
  ylab("Cumulative rain (mm)")-> plot1




  
sheffield %>% mutate(avg_temp = (temp_max+temp_min)/2) %>% 
  mutate(month = lubridate::month(month, label = TRUE)) %>% 
  group_by(year) %>% 
  mutate(cum_mean_temp = cummean(avg_temp)) %>%
  ggplot(aes(x=month, y=cum_mean_temp, group = year))+
  geom_line(alpha = 0.1, size = 1.3, colour = heat[2])+
  geom_line(data = .%>% filter(year == 2022),aes(x=month, y=cum_mean_temp), colour = heat[1], size = 1.5, alpha = .55)+
  geom_text(data = . %>% filter(year == 2022, month == 'Jul'), aes(x= month, y= cum_mean_temp, label = year), 
            nudge_x = .75, nudge_y = .5, colour = heat[1], size = 15)+
  labs(title = "Rolling mean temperature in Sheffield",
       subtitle = "2022 is unusually hot",
       caption = "@CookeComms | MetOffice weather station data")+
  xlab("Month")+
  ylab("Year-to-date average temperature (celcius)")-> plot2

weatherdata %>% 
  filter(station %in% stations) %>% 
  mutate(month = lubridate::month(month, label = TRUE)) %>% 
  group_by(station, year) %>% 
  mutate(ytd_rain = cumsum(rain_mm)) %>%
  ggplot(aes(x=month, y=ytd_rain, group = year))+
  geom_line(alpha = 0.25, color = colour[2])+
  geom_line(data = .%>% filter(year == 2022),aes(x=month, y=ytd_rain), colour = colour[4])+
  geom_text(data = . %>% filter(year == 2022, month == 'Jul'), aes(x= month, y= ytd_rain, label = year), 
            nudge_x = 1, nudge_y = 20, colour = colour[4], size = 10)+
  facet_wrap(~station)+
  xlab("Month")+
  ylab("Cumulative rain (mm)")+
  labs(title = "Cumulative rain across the oldest weather stations",
       caption = "@CookeComms | MetOffice weather station data")-> plot3

  
weatherdata %>% 
  filter(station %in% stations) %>% 
  mutate(avg_temp = (temp_max+temp_min)/2) %>% 
  mutate(month = lubridate::month(month, label = TRUE)) %>% 
  group_by(station, year) %>% 
  mutate(cum_mean_temp = cummean(avg_temp)) %>%
  ggplot(aes(x=month, y=cum_mean_temp, group = year))+
  geom_line(alpha = 0.1, colour = heat[2])+
  geom_line(data = .%>% filter(year == 2022),aes(x=month, y=cum_mean_temp), colour = heat[1], alpha = .55)+
  geom_text(data = . %>% filter(year == 2022, month == 'Jul'), aes(x= month, y= cum_mean_temp, label = year), 
            nudge_x = .75, nudge_y = .5, colour = heat[1], size = 10)+
  facet_wrap(~station)+
  labs(title = "Year-to-date average temps across oldest weather stations",
       caption = "@CookeComms | MetOffice weather station data")+
  xlab("Month")+
  ylab("Year-to-date average temperature (celcius)") -> plot4




  
weatherdata %>% 
  filter(station %in% stations) %>% 
  mutate(month = lubridate::month(month, label = TRUE)) %>% 
  group_by(station, year) %>% 
  mutate(ytd_rain = cumsum(rain_mm)) %>%
  filter(month == 'Jul') %>%
  ggplot(aes(y=ytd_rain, x=station))+
  geom_jitter(alpha = 0.4, colour = colour[2])+
  geom_boxplot(alpha = 0.3, outlier.alpha = 0, colour = colour[3])+
  geom_point(data = . %>% filter(year==2022), colour = colour[5], size = 2)+
  geom_text(data = . %>% filter(year==2022 & station == 'Oxford'), aes(label = year), colour = colour[5], nudge_x = .25, size = 10, hjust = 0.2)+
  xlab("Station")+
  ylab("Rainfall up to July")+
  labs(title = "Distribution of yearly rainfall up to July",
       subtitle = 'How does 2022 compare?') -> plot5

weatherdata %>% 
  filter(station %in% stations) %>% 
  mutate(avg_temp = (temp_max+temp_min)/2) %>% 
  mutate(month = lubridate::month(month, label = TRUE)) %>% 
  group_by(station, year) %>% 
  mutate(cum_mean_temp = cummean(avg_temp)) %>%
  filter(month == 'Jul') %>%
  ggplot(aes(y=cum_mean_temp, x=station))+
  geom_jitter(alpha = 0.4,colour = heat[2])+
  geom_boxplot(alpha = 0.2, outlier.alpha = 0, colour = heat[1], size = 0.2)+
  geom_point(data = . %>% filter(year==2022), colour = heat[1], size = 2)+
  geom_text(data = . %>% filter(year==2022 & station == 'Oxford'), aes(label = year), colour = heat[1], nudge_x = .25, size = 10, hjust = 0.2)+
  xlab("Station")+
  ylab("Cumulative average temperature")+
  labs(title = "Distribution of cumulative average temperature up to July",
       subtitle = 'How does 2022 compare?') -> plot6

ggsave('plot1.jpg',plot1,device = "jpg",scale = .8, height = 9, width = 16, units = 'cm', dpi = 400)
ggsave('plot2.jpg',plot2,device = "jpg",scale = .8, height = 9, width = 16, units = 'cm', dpi = 400)
ggsave('plot3.jpg',plot3,device = "jpg", height = 9, width = 16, units = 'cm', dpi = 400)
ggsave('plot4.jpg',plot4,device = "jpg", height = 9, width = 16, units = 'cm', dpi = 400)
ggsave('plot5.jpg',plot5,device = "jpg", scale = .8, height = 9, width = 12, units = 'cm', dpi = 400)
ggsave('plot6.jpg',plot6,device = "jpg", scale = .8, height = 9, width = 12, units = 'cm', dpi = 400)
  
