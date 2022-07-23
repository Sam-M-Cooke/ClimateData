library(tidyverse)
library(shiny)
library(lubridate)

#data is scraped from another script.
data = read_csv("~/Climate/WeatherApp/data/weatherdata.csv")

labels <- labs(caption = 'Data from historic weather station data from the Met Office | www.metoffice.gov.uk | @Cookecomms',
               fill = "Deviation from the 1970-2000\naverage temp in °C")


#This is the function that plots climate bars the chart.
climate_bars <- function(plotdata, ref_temp){

  ggplot(plotdata, aes(year,(avg_temp-ref_temp), fill=(avg_temp-ref_temp)))+
    geom_col()+
    scale_fill_gradient2(low='#08519c',high='#a50f15',mid='#deebf7',midpoint=0)+
    theme(plot.background = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_blank())+
    labels
  #Add in the themes, scales ect. TO DO!
}

climate_stripes <- function(plotdata, ref_temp){
  ggplot(plotdata, aes(year, 1, fill = (avg_temp-ref_temp)))+
    geom_tile()+
    scale_fill_gradient2(low='#08519c',high='#a50f15',mid='#deebf7',midpoint=0)+
    coord_fixed(ratio = 16)+
    theme(plot.background = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_blank())+
    labels
}



#remove the stations that don't have at least 70 years of data
#drop the current year as the data won't be complete
#change months from numbers an ordered factor
data %>% 
  group_by(station) %>% 
  filter(min(year)<1950) %>%
  mutate(avg_temp = (temp_max + temp_min)/2,
         month = month(month, label = TRUE)) %>% 
  filter(year < year(Sys.time())) -> weather

#dump the stations into a vector for use in a list.
stations <- unique(weather$station)

ui <- (
  fluidPage(
    sidebarLayout(
      sidebarPanel(selectInput("station", "Select a weather station", choices=stations),
                   selectInput("plottype", "Select a plot type", choices = c("Climate stripes",
                                                                             "Climate bars"), selected = 'Climate bars')
                  
      ),
      mainPanel(plotOutput("plot"),
                textOutput('ref_temp')
      )
      
      )
  )
)

server <- function(input, output, session){
  weatherdata <- reactive(weather)
  
  plotdata <- reactive(filter(weatherdata(), station == input$station) %>% 
                         group_by(year) %>% 
                         mutate(avg_temp = mean(avg_temp))
  )
  
#This defines the reference range in terms of temperature to base the rest of the charts off of.
 ref_temp <- reactive({plotdata() %>% 
     group_by(station) %>% 
     filter(year > 1970 & year <2000) %>%
     summarise(avg_temp = mean(avg_temp, na.rm = TRUE)) -> ref_temp
   pull(ref_temp)
 }
 )
 
 output$ref_temp <- renderText({ref_temp()
 })
                        
 output$plot <- renderPlot(
  if(input$plottype == "Climate stripes") {
    climate_stripes(plotdata(),ref_temp())
  } else if(input$plottype == "Climate bars") {
    climate_bars(plotdata(),ref_temp())
  }
  else {
    NULL
  }
 )
 
}


shinyApp(ui, server)


