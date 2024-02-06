library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
apr <- read.csv("uberdataset/uber-raw-data-apr14.csv")
may <- read.csv("uberdataset/uber-raw-data-may14.csv")
june <- read.csv("uberdataset/uber-raw-data-jun14.csv")
july <- read.csv("uberdataset/uber-raw-data-jul14.csv")
aug <- read.csv("uberdataset/uber-raw-data-aug14.csv")
sept <- read.csv("uberdataset/uber-raw-data-sep14.csv")

data <- rbind(apr, may, june, july, aug, sept)
cat("The dimensions of the data are:", dim(data))
head(data)
data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

data$hour = factor(hour(hms(data$Time)))
data$month <- factor(month(data$Date.Time, label=TRUE))
head(data)
hourly_data <- data %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())
datatable(hourly_data)

ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="steelblue", 
           color="red") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
# Aggregate the data by month and hour
month_hour_data <- data %>% group_by(month, hour) %>%  dplyr::summarize(Total = n())

ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)


