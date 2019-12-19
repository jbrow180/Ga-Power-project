library(tseries)
library(forecast)
library(dplyr)
library(zoo)
library(rio)
library(tidyverse)

datax <- select(datax, -c(X))

date <- as.Date(datax$Date, "%m/%d/%y")
X <- data.frame(date,datax)
X <- select(X,-c(Date))

#create a time series object based on total usage 
time <- ts(X[,c("Total.use.hr.KWH")])


X$clean_count = tsclean(time)

ggplot()+ geom_line(data = X, aes(x =date, y=clean_count)) + ylab("clean_count")
#test to see if the number were the same in the new colum created. is there differ in the total kwh and clean count 
ggplot()+ geom_line(data = X, aes(x =date, y=Total.use.hr.KWH)) + ylab("Total.use.hr.KWH")

#outlier missing data,
X$count_wa = ma(X$Total.use.hr.KWH, order = 168)
X$cont_Ma = ma(X$Total.use.hr.KWH, order = 720)
summary(X)
# replace the na with the average of the moving count 
X2 <- replace(X, TRUE, lapply(X, na.aggregate))

summary(X2)
#
ggplot() +
  geom_line(data = X, aes(x = date, y = Total.use.hr.KWH, colour ="Total Use per hour "))  + 
  geom_line( data = X, aes(x = date, y = count_wa, colour ="Weekly moving average")) +
  geom_line(data = X, aes(x = date, y = cont_Ma, colour ="Monthly moving average"))

ggplot() + 
  geom_line(data = X2, aes(x = date, y = clean_count, colour ="counts"))  + 
  geom_line(data = X2, aes(x = date, y = count_wa, colour ="Weekly moving average")) +
  geom_line(data = X2, aes(x = date, y = cont_Ma, colour ="Monthly moving average"))
#decomosig data taking seanlity trend and cycling into acco





count_ma <- ts(na.omit(X2$Total.use.hr.KWH),frequency = 288)
decomp = stl(count_ma, "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

#test for stationary varience agumentr dick fuller test a pvalue less .05 indicate 
#the data set is stationary if your is not statniaory you cant move on
adf.test(count_ma, alternative ="stationary")

#plot between series and it lag auto correlation betweent series and it lag 
#####################################################
acf(count_ma,main="")

pacf(count_ma,main="")

count_d1 =diff(deseasonal_cnt, differences = 1)
plot(count_d1)

count_ma = diff(count_d1, differences = 1)

adf.test(count_d1, alternative ="stationary")

acf(count_d1,main="Acf for Diff")
pacf(count_d1,main="pacf for diff")

library(WDI)

plot(data =X2,Total.use.hr.KWH~date)

ggplot(X, aes(x =Heating.Gas.KWH , y =Gen.Int.LightsElectricity.KWH,col="")) +
  geom_point()


ggplot(X, aes(x =date , y =Heating.Gas.KWH, col="Heating.Gas.Kwh")) +
  geom_point()

ggplot(data = X, aes(x = date, y =Electricity.HVAC.KWH )) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Heating Venelating and Air Condotioning every hour Monthly",  x = "Date", y = "Electricity.HVAC.KWH")


names(X2)
appl <-
  library(ggplot2)
  library(lubridate)









library(forecast)
#get auto pdq values 
model<-auto.arima(count_ma,seasonal = FALSE)
model
summary(model)
accuracy(model)
predict(model,n.ahead =8760,se.fit = T )
TUForecast <- forecast(object =model,h=8760 )
plot(TUForecast)
par(mar=c(1,1,1,1))
coef(model)
This is the fourth model and it is the model of best fit.

$$y=53.66(dislikes)-41.77(comment\_count)+23.30(likes)$$
  
  ``