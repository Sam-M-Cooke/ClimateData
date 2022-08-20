library(tidyverse)
library(showtext)

#font_add_google(name = "Rubik", family = "rubik") # if you need to install the font
showtext_auto()

colour <- c("#BCD2E8",'#91BAD6', '#73A5C6', '#528AAE','#2E5984', '#1E3F66')

theme_set(theme_minimal())
theme_replace(text = element_text(family = "Rubik", colour = colour[5], size = 36),
              strip.text = element_text(hjust = 0),
              plot.title.position = 'plot')


weatherdata <- read_csv('~/Climate/WeatherApp/data/weatherdata.csv')


#data transformation
weatherdata %>% 
  mutate(avg_temp = (temp_max+temp_min/2)) %>% 
  group_by(year, station) %>% 
  mutate(cum_rain = cumsum(rain_mm),
         mean_temp = cummean(avg_temp)) %>% 
  filter(month == 7) -> cumulative_rain

cumulative_rain %>% 
  group_by(station) %>% 
  summarise(mean_rainfall = mean(cum_rain, na.rm = TRUE),
            summary_mean_temp = mean(mean_temp, na.rm = TRUE)) -> station_summarised


cumulative_rain %>% 
  filter(year == 2022) %>% 
  left_join(station_summarised, by='station') %>% 
  mutate(distance = cum_rain - mean_rainfall,
         index = (cum_rain/mean_rainfall)*100,
         mean_index = 100)-> plot_data


text_size = 8

#plot
plot_data %>% 
  ggplot()+
  aes(y=reorder(station, mean_rainfall))+
  geom_segment(aes(x = mean_rainfall, xend=cum_rain, y = reorder(station, mean_rainfall), yend = reorder(station, mean_rainfall)),
               alpha = 0.5, colour = colour[4])+
  geom_point(aes(x=mean_rainfall), colour = colour[2])+
  geom_point(aes(x=cum_rain), colour = colour[5])+
  geom_text(data = . %>% filter(station == "Cardiff"),
            aes(x=mean_rainfall,y=station, label = "Average amount of rain by July"), colour = colour[4], hjust = 0, nudge_x = 20,
            size = text_size)+
  geom_text(data = . %>% filter(station == "Cardiff"),
            aes(x=cum_rain,y=station, label = "Rain by July in 2022"), colour = colour[5], hjust = 1, nudge_x = -20,
            size = text_size)+
  scale_x_continuous(limits = c(0,NA))+
  ylab("Weather Station")+
  xlab("Rainfall (mm)")+
  labs(title = "Rainfall to July this below average almost everywhere",
       subtitle = "Except for parts of Scotland and Northern Ireland",
       caption = '@CookeCooms | Met Office weather station data') -> plot


ggsave('Dumbellplot.jpg', plot,device = "jpg", scale = .8, height = 12, width = 9, units = 'cm', dpi = 600)
