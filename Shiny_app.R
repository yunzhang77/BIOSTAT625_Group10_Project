# code contributors: Stuart Brabbs (implement bubble plot) and Yun Zhang (construct Shiny App)

# Load required libraries
library(shiny)
library(shinydashboard)
library(maps)
library(stringr)
library(ggplot2)
library(viridis)
library(dplyr)
library(plotly)

################## DATA PROCESSING ##################
# import air pollution data, modify date type, and remove unnecessary columns
michigan <- map_data("county", "michigan")
air_polllution_data <- readRDS("data/data2019.rds")
date <- as.Date(air_polllution_data$Date)

# generate processed data for plotting county map 
air_polllution_data_trimmed_county <- data.frame(date, air_polllution_data$`Daily Mean PM2.5 Concentration`, 
                                                 air_polllution_data$DAILY_AQI_VALUE, air_polllution_data$COUNTY)
colnames(air_polllution_data_trimmed_county) <- c("Date", "PM2.5", "AQI", "County")

# change the name of "St. Clair" county to match map data
air_polllution_data_trimmed_county$County[air_polllution_data_trimmed_county$County == "St. Clair"] <- "St Clair"

# generate list of Michigan counties from Maps package for visualization
county_names <- map("county", regions = "Michigan", namesonly = TRUE, plot = FALSE)
county_names <- as.data.frame(str_to_title(sub("michigan,", "", county_names)))
colnames(county_names) <- "County"

# generate processed data for plotting bubble map
air_polllution_data_trimmed_bubble <- data.frame(date, air_polllution_data$`Daily Mean PM2.5 Concentration`, 
                                                 air_polllution_data$DAILY_AQI_VALUE, air_polllution_data$`Site ID`,
                                                 air_polllution_data$SITE_LATITUDE, air_polllution_data$SITE_LONGITUDE)
colnames(air_polllution_data_trimmed_bubble) <- c("Date", "PM2.5", "AQI", "Site", "Lat", "Long")
sitenames <- as.data.frame(unique(air_polllution_data_trimmed_bubble$Site))
colnames(sitenames) <- c("Site")

# function that generates tables based on user input - returns a df contains average of 10-days data 
table_of_interest <- function(date, type){
  date <- as.Date(date)
  # filter data that only contains 10-day of data
  selected_entry <- subset(air_polllution_data_trimmed_county, Date >= date & Date < (date + 10))
  
  # calculate mean values of each county if more than one record is found
  selected_entry_mean <- aggregate(.~County, data = selected_entry, mean)
  data_display <- merge(county_names, selected_entry_mean, by = "County", all.x = TRUE) 
  
  if (type == "PM 2.5"){
    # based on PM2.5 value generate new column contains HEX colors
    data_display$color_code <- unlist(lapply(data_display$PM2.5, PM25_level))
    return(data_display)
  } else{
    # based on AQI value generate new column contains HEX colors
    data_display$color_code <- unlist(lapply(data_display$AQI, AQI_level))
    return(data_display)
  }
}

# function that generates tables based on user input - returns a df contains average of 10-days data for bubble plot
table_of_interest_bubble <- function(date, type){
  date <- as.Date(date)
  selected_entry <- subset(air_polllution_data_trimmed_bubble, Date >= date & Date < (date + 10))
  
  # calculate mean values of each site if more than one record is found
  selected_entry_mean <- aggregate(.~Site, data = selected_entry, mean)
  return(selected_entry_mean)
}

# function to generate plots based on user input
plot_map <- function(date, type, plot_type){
  # if county map is selected by user, plot county map
  if (plot_type == 1){
    data_table <- table_of_interest(date, type)
    map("county", regions = "Michigan", fill = TRUE, col = data_table$color_code,  
        resolution = 1, lty = 1, projection = "polyconic", 
        myborder = 1, mar = c(1, 1, 1, 1))
  } else {
    # if bubble plot is selected by user, plot bubble plot. Data variable selected by user will be plotted
    if (type == "PM 2.5"){
      data_table <- table_of_interest(date, type)
      mybreaks <- c(12, 35.5, 55.5, 150.5)
      ggplot() +
        geom_polygon(data = michigan, aes(x = long, y = lat, group = group), fill = "grey") +
        geom_point(data = table_of_interest_bubble(date, type), aes(x = Long, y = Lat, size = PM2.5, color = PM2.5)) +
        scale_size_continuous(name = "PM2.5", range = c(1, 10)) +
        scale_color_viridis(name = "PM2.5", breaks = mybreaks) +
        theme_void() + coord_map() +
        guides(color = guide_legend()) +
        theme(legend.position = "topright")
    } else{
      data_table <- table_of_interest(date, type)
      mybreaks <- c(50, 100, 150, 200, 300)
      ggplot() +
        geom_polygon(data = michigan, aes(x = long, y = lat, group = group), fill = "grey") +
        geom_point(data = table_of_interest_bubble(date, type), aes(x = Long, y = Lat, size = AQI, color = AQI)) +
        scale_size_continuous(name = "AQI", range = c(1, 10)) +
        scale_color_viridis(name = "AQI", breaks = mybreaks) +
        theme_void() + coord_map() +
        guides(color = guide_legend()) +
        theme(legend.position = "topright")
    }
  }
}

# functions that return HEX color code based on PM2.5 value. White is returned if missing value. 
PM25_level <- function(PM25){
  PM25 <- as.numeric(PM25)
  if (is.na(PM25)) {return("#FFFFFF")}
  if (PM25 <= 12.0) {return("#32CD32")}
  else if (12.1 <= PM25 & PM25 <= 35.4) {return("#FFFF00")}
  else if (35.5 <= PM25 & PM25 <= 55.4) {return("#FFA500")}
  else if (55.5 <= PM25 & PM25 <= 150.4) {return("#FF2E2E")}
  else if (150.5 <= PM25 & PM25 <= 250.4) {return("#D10000")}
  else if (250.5 <= PM25) {return("#750000")}
  else {return("#FFFFFF")}
}

# functions that return HEX color code based on AQI value. White is returned if missing value. 
AQI_level <- function(AQI){
  AQI <- as.numeric(AQI)
  if (is.na(AQI)) {return("#FFFFFF")}
  if (AQI <= 50.0) {return("#32CD32")}
  else if (50.0 < AQI & AQI <= 100) {return("#FFFF00")}
  else if (100 < AQI & AQI <= 150) {return("#FFA500")}
  else if (150 < AQI & AQI <= 200) {return("#FF2E2E")}
  else if (200 < AQI & AQI <= 300) {return("#D10000")}
  else if (300 < AQI) {return("#750000")}
  else {return("#FFFFFF")}
}

################## SHINY ##################
# Build user interface 
ui <- fluidPage(
  titlePanel("Air Pollution data of Michigan Counties (PM2.5 and AQI)"),
  sidebarLayout(
    # define all side bar panels 
    sidebarPanel(
      # dropdown menu for user to choose variable of interest
      selectInput("var", 
                  label = "Choose variable to plot:", 
                  choices = list("AQI", "PM 2.5"),
                  selected = "PM 2.5"),
      
      # radio button for user to choose which type of plot to draw
      radioButtons("plot_type", label = "Select plot type:",
                   choices = list("County Map" = 1, "Bubble Plot" = 2), selected = 1),
      
      
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

# Build server function
server <- function(input, output){
  # return text message on which data variable user picked
  output$selected_variables <- renderText({
    paste("You have selected to look at", input$var, "data.")
  })
  
  # render map object 
  output$map <- renderPlot({
    plot_map(input$slider, input$var, input$plot_type)
  }, height = 600, width = 500)
  

  # render a boxplot containing all datapoints based on the time and data variable user selected
  output$boxplot <- renderPlot({
    display_table <- table_of_interest(input$slider, input$var)
    
    if (input$var == "PM 2.5"){
      boxplot(display_table$PM2.5, main = "Data Distribution",
              ylab = "PM2.5 Concentration (ug/m3)")
    } else{
      boxplot(display_table$AQI, main = "Data Distribution",
              ylab = "AQI")
    }
  })
}

# Launch app
shinyApp(ui = ui, server = server)

