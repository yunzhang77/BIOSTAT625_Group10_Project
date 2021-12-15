library(ggplot2)

air_polllution_data <- readRDS("data/data2019.rds")

county <- unique(air_polllution_data$COUNTY)
mean_pm25 <- c()
for (i in county) {
  mean_pm25 <- c(mean_pm25, 
                 mean(air_polllution_data$`Daily Mean PM2.5 Concentration`[air_polllution_data$COUNTY==i]))
}

countypop <- c(120502, 103856, 36785, 406211, 284900, 261670, 657974, 2046, 99423, 881217, 25032, 
               15052, 1274395, 296200, 160383, 8047, 372258, 1793561)

df <- data.frame(county, countypop, mean_pm25)

##make barplot of mean pm2.5 by county
ggplot(data = df, aes(x = county, y = mean_pm25)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("County") +
  ylab("Mean PM2.5") +
  labs(title = "2019 Mean PM2.5 Concentration by County")

ggplot(data = df, aes(x = countypop, y = mean_pm25)) +
  geom_point(color = "steelblue", size = 2.5) +
  xlab("County Population") +
  ylab("Mean PM2.5") +
  labs(title = "2019 Mean PM2.5 Concentration Scatterplot by County Population")

