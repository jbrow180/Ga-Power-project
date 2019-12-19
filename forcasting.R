library(fpp2)
library(dplyr)
library(zoo)
library(rio)
library(tidyverse)
setwd("~/Desktop/Project")
data<- read.csv("forecast.csv")
datax <-read.csv("df.csv")
data <- select(data, -c(X))

Date <- as.Date(data$Date, "%m/%d/%y")
 X <- data.frame(date,data)
 X <- select(X,-c(Date))
#Declare as time series data
Y <- ts(X[,2],start=c(2018,1),frequency = 8760)
#preform preliminary aalysis

#time plo
autoplot(Y) +ggtitle("Timeplot: Total Usage per hour each Day") + ylab("KHW")

#investigation of transformations 
#total the first difference to remove the trend
DY <- diff(Y)
#tIME plot Difference Data 
autoplot(DY, col="red") +ggtitle("Timeplot: Change In Usage per hour each Day") + ylab("KHW")

#Series Appears Trend stationary, use to investgate seasonality
ggseasonplot(DY)+ + ggtitle("Seasonal plot:Change in daily usage") + ylab("usage daily")
?ggseasonplot
#our y series has trend we remove the the trends by doing the difference of y
#and we are trying to look at the seasonality by months
monthplot(DY, labels = NULL, times = time(DY), phase = cycle(DY),
          ylab = deparse(substitute(DY)))


X %>%
  mutate(month2 = as.Date(paste0("2018","-01"),"%Y-%m-%d")) %>%
  ggplot(aes(x = month2, y = Total.use.hr.KWH)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ncol = 3) +
  labs(title = "Montly Total Daily Precipitation - Boulder, Colorado",
   subtitle = "Data plotted by year",
    y = "Daily precipitation (inches)",
      x = "Month") + theme_bw(base_size = 15) +
  scale_x_date(date_labels = "%b")
#Hypotheis that the bill will be higher in summer then the usage is more 

#Forecast with Vario``
