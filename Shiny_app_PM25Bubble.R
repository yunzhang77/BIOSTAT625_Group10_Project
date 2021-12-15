library(ggplot2)
library(viridis)
library(stringr)
library(maps)
library(dplyr)
library(plotly)

##load data
michigan <- map_data("county", "michigan")
air_polllution_data <- readRDS("data/data2019.rds")
date <- as.Date(air_polllution_data$Date)
air_polllution_data_trimmed <- data.frame(date, air_polllution_data$`Daily Mean PM2.5 Concentration`, 
                                          air_polllution_data$DAILY_AQI_VALUE, air_polllution_data$`Site ID`,
                                          air_polllution_data$SITE_LATITUDE, air_polllution_data$SITE_LONGITUDE)
colnames(air_polllution_data_trimmed) <- c("Date", "PM2.5", "AQI", "Site", "Lat", "Long")
sitenames <- as.data.frame(unique(air_polllution_data_trimmed$Site))
colnames(sitenames) <- c("Site")

# function that generates tables based on user input - returns a df contains 10-days of data 
table_of_interest <- function(date, type){
  date <- as.Date(date)
  selected_entry <- subset(air_polllution_data_trimmed, Date >= date & Date < (date + 10))
  
  # calculate mean values of each site if more than one record is found
  selected_entry_mean <- aggregate(.~Site, data = selected_entry, mean)
  
  return(selected_entry_mean)
}

plot_map <- function(date, type){
  data_table <- table_of_interest(date, type)
  mybreaks <- c(12, 35.5, 55.5, 150.5)
  ggplot() +
    geom_polygon(data = michigan, aes(x = long, y = lat, group = group), fill = "grey") +
    geom_point(data = table_of_interest(date, type), aes(x=Long, y=Lat, size = PM2.5, color = PM2.5)) +
    scale_size_continuous(name="PM2.5", range=c(1,10)) +
    scale_color_viridis(name = "PM2.5", breaks = mybreaks) +
    theme_void() + coord_map() +
    guides(color = guide_legend()) +
    theme(
      legend.position = "topright",
    )
}

ui <- fluidPage(
  titlePanel("Michigan PM2.5 Data"),
  sidebarLayout(
    # define all side bar panels 
    sidebarPanel(
      # dropdown menu for user to choose variable of interest
      selectInput("var", 
                  label = "Choose variable to plot:", 
                  choices = list("PM2.5", "AQI"),
                  selected = "PM2.5"),
      
      # slider bar for user to choose date of interest
      sliderInput("slider",
                  label = "Time",
                  min = as.Date("2019-01-01"), max = as.Date("2019-12-31"),
                  value = as.Date("2019-01-01"), timeFormat = "%F", step = 10)
    ),
    
    # main panel that returns a text message, map plot, and a box plot
    mainPanel(
      textOutput("selected_variables"),
      fluidRow(splitLayout(cellWidths = c("75%", "25%"), 
                           plotOutput("map", height = "auto"), plotOutput("boxplot")))
    )
  )
)

server <- function(input, output){
  # return text message on which data variable user picked
  output$selected_variables <- renderText({
    paste("You have selected to look at", input$var, "data.")
  })
  
  # render map object 
  output$map <- renderPlot({
    plot_map(input$slider, input$var)
  }, height = 600, width = 400)
  
  # render a boxplot containing all datapoints based on the time user selected
  output$boxplot <- renderPlot({
    display_table <- table_of_interest(input$slider, input$var)
    boxplot(display_table$PM2.5, main = "Data Distribution",
            ylab = "PM2.5")
  })
}

# Launch app
shinyApp(ui = ui, server = server)
