# code contributors: Stuart Brabbs (implement both plots) and Yun Zhang (add code documentation)

# Load required libraries
library(ggplot2)

# import air pollution data
air_pollution_data <- readRDS("data/data2019.rds")

# identify counties with data and generate new data frame to store result
county <- unique(air_pollution_data$COUNTY)
mean_pm25 <- c()
for (i in county) {
  mean_pm25 <- c(mean_pm25, 
                 mean(air_pollution_data$`Daily Mean PM2.5 Concentration`[air_pollution_data$COUNTY==i]))
}

# county population 
countypop <- c(120502, 103856, 36785, 406211, 284900, 261670, 657974, 2046, 99423, 881217, 25032, 
               15052, 1274395, 296200, 160383, 8047, 372258, 1793561)

df <- data.frame(county, countypop, mean_pm25)

# make bar plot of mean PM2.5 by county
ggplot(data = df, aes(x = county, y = mean_pm25)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("County") +
  ylab("Mean PM2.5") +
  labs(title = "2019 Mean PM2.5 Concentration by County")

# make scatter plot showing PM2.5 concentration by county population
ggplot(data = df, aes(x = countypop, y = mean_pm25)) +
  geom_point(color = "steelblue", size = 2.5) +
  xlab("County Population") +
  ylab("Mean PM2.5") +
  labs(title = "2019 Mean PM2.5 Concentration Scatterplot by County Population")

