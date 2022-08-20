library(tidyverse)
library(rvest)

### This script scrapes the weather data.

#select the source data from Met Office
url <- read_html("https://www.metoffice.gov.uk/research/climate/maps-and-data/historic-station-data")

#scrape the urls of the individual weather stations
regionurls <- url %>% 
  html_nodes("table") %>% 
  html_nodes("td") %>% 
  html_nodes("a") %>% 
  html_attr("href")


#function to scrape the actual weather data
weather_data_reader <-  function(url){
  name <- as.character(read_lines(url,n_max = 1))  
  read_fwf(url, skip=8, 
           fwf_cols(year=9, month = 5, temp_max = 7,temp_min = 8, frost_days = 7, rain_mm = 8, sun = 8),
           col_types = c(year = "c", month = "c", temp_max = "c", temp_min = "c", frost_days = "c", rain_mm = "c", sun = "c"),
           na = c('---','--','-'),
           comment = c('*')) %>% 
    mutate(station = name)
}

#function call to scrape and map to a tibble
data <- map_dfr(regionurls,weather_data_reader)

setwd("~/Climate/WeatherApp/data")

#Clean the data. The function call above needed to have all columns the same type so seemed easier to clean the set afterwards.
data %>% 
  mutate(year = parse_number(year),
         month = parse_number(month),
         temp_max = parse_number(temp_max),
         temp_min = parse_number(temp_min),
         frost_days = parse_number(frost_days),
         rain_mm = parse_number(rain_mm),
         sun = parse_number(sun),
         station = str_extract(station, "[^(   |/)]*"), 
         station = parse_factor(station)) %>% 
  drop_na(year) -> weatherdata

levels(weatherdata$station)

weatherdata %>% 
  write_csv('weatherdata.csv')
