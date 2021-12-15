# Load required libraries
library(shiny)
library(shinydashboard)
library(maps)
library(stringr)

################## DATA PROCESSING ##################
# import air pollution data, modify date type, and remove unnecessary columns
air_polllution_data <- readRDS("data/data2019.rds")
date <- as.Date(air_polllution_data$Date)
air_polllution_data_trimmed <- data.frame(date, air_polllution_data$`Daily Mean PM2.5 Concentration`, 
                                          air_polllution_data$DAILY_AQI_VALUE, air_polllution_data$COUNTY)
colnames(air_polllution_data_trimmed) <- c("Date", "PM2.5", "AQI", "County")

# generate list of Michigan counties from Maps package for visualization
county_names <- map("county", regions = "Michigan", namesonly = TRUE, plot = FALSE)
county_names <- as.data.frame(str_to_title(sub("michigan,", "", county_names)))
colnames(county_names) <- "County"

# function that generates tables based on user input - returns a df contains 10-days of data 
table_of_interest <- function(date, type){
  date <- as.Date(date)
  selected_entry <- subset(air_polllution_data_trimmed, Date >= date & Date < (date + 10))
  
  # calculate mean values of each county if more than one record is found
  selected_entry_mean <- aggregate(.~County, data = selected_entry, mean)
  data_display <- merge(county_names, selected_entry_mean, by = "County", all.x = TRUE) 
  
  # based on PM2.5 value generate new column contains HEX colors
  data_display$color_code <- unlist(lapply(data_display$AQI, AQI_level))
  return(data_display)
}

# function to generate plots based on user input
plot_map <- function(date, type){
  data_table <- table_of_interest(date, type)
  map("county", regions = "Michigan", fill = TRUE, col = data_table$color_code,  
      resolution = 1, lty = 1, projection = "polyconic", 
      myborder = 1, mar = c(1,1,1,1))
}

AQI_level <- function(AQI) {
  AQI <- as.numeric(AQI)
  if(is.na(AQI)) {return("#FFFFFF")}
  if (AQI <= 25) {return("#32CD32")}
  else if (26 <= AQI & AQI <= 50) {return("#FFFF00")}
  else if (51 <= AQI & AQI <= 75) {return("#FFA500")}
  else if (76 <= AQI & AQI <= 100) {return("#FF2E2E")}
  else if (101 <= AQI & AQI <= 150) {return("#D10000")}
  else {return("#750000")}
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
                  selected = "AQI"),
      
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
    plot_map(input$slider, input$var)
  }, height = 600, width = 500)
  
  # render a boxplot containing all datapoints based on the time user selected
  output$boxplot <- renderPlot({
    display_table <- table_of_interest(input$slider, input$var)
    boxplot(display_table$AQI, main = "Data Distribution",
            ylab = "AQI")
  })
}

# Launch app
shinyApp(ui = ui, server = server)