install.packages("tidyverse")

library(readxl)
library(forecast)
library(ggplot2)
library(lubridate)
library(dplyr)

temp.data <- read_excel("C:/Users/phalg/Downloads/R files/TempData.xlsx")

head(temp.data)
# Data Cleaning
temp.data$Date = strptime(temp.data$Date, "%Y-%m-%d")
head(temp.data)
temp.data$Date  <-  as.POSIXct(temp.data$Date)
temp.data$Temperature <- as.numeric(temp.data$Temperature)

#  Aggregation - Monthly and Weekly

temp.data <- mutate(temp.data, MonthYear = paste(year(Date),formatC(month(Date), width = 2, flag = "0")))
temp.data <- mutate(temp.data,WeekYear = paste(year(Date),formatC(week(Date),width = 2,flag = "0")))
temp_weeks <- aggregate(temp.data$Temperature,by=list(temp.data$WeekYear),FUN = function(x) min(x,na.rm =T))

# Converting Data into Time Series

temp.ts <- ts(temp_months$x,frequency = 12,start = c(1981,01), end = c(1990, 12))
temp.ts2 <- ts(temp_weeks$x,frequency = 52,start = c(1981,01),end = c(1990,52))
temp.ts3 <- ts(temp.data$Temperature,frequency = 365,start = c(1981,0),end = c(1990,365))

plot(temp.ts)
plot(temp.ts2, xlab="Years", ylab="Temperature", main="Temperature as a function of time series")
plot(temp.ts3)
?zoom

# Decomposition

temp.decomp <- decompose(temp.ts)
temp.decomp2 <- decompose(temp.ts2)
temp.decomp3 <- decompose(temp.ts3)

#Plotting the graph for weekly data

plot(temp.decomp2)

show(temp.decomp2$type)

# Splitting the data into training and validation

temp.train.ts <- window (temp.ts2, start=c(1981,1), end=c(1988,52))
temp.valid.ts <-window (temp.ts2, start=c(1989,1), end=c(1990,52))  


plot(temp.train.ts)
Acf(temp.train.ts)
Pacf(temp.train.ts)


#Seasonal Naives Model

temp.snaive.fit <- snaive(temp.train.ts)
temp.snaive.fit.forc <- forecast(temp.snaive.fit,h=104)
accuracy(temp.snaive.fit.forc,temp.valid.ts)



# Linear model with trend trend^2 and seasonality 

temp.tslm <- tslm(temp.train.ts ~ trend + I(trend^2)+ season)

summary(temp.tslm)


plot(temp.train.ts,xlab="Time",ylab="Temparature",main="Monthly minimum Temparature(Linear Model)",xaxs="i",lty=1)
lines(temp.tslm$fitted, lwd = 2,col="blue",lty=2)


#ARIMA Modelling


temp.train.ts %>% plot()
temp.train.ts %>% Acf()
temp.train.ts %>% Pacf()


temp.train.ts %>% diff() %>% plot()
temp.train.ts %>% diff() %>% Acf()
temp.train.ts %>% diff() %>% Pacf()

temp.train.ts %>% diff(52) %>% plot()
temp.train.ts %>% diff(52) %>% acf()
temp.train.ts %>% diff(52) %>% Pacf(lag.max = 10)



temp.arima.fit<-temp.train.ts %>% Arima(order=c(1,0,1),seasonal = c(1,1,1))
acf(temp.arima.fit$residuals)
pacf(temp.arima.fit$residuals)
temp.arima.fit

plot(temp.train.ts,xlab="Time",ylab="Temparature",main="Monthly minimum Temparature(Arima)",xaxs="i",lty=1)
lines(temp.arima.fit$fitted, lwd = 2,col="blue",lty=2)


temp.autofit <- auto.arima(temp.train.ts)
Acf(temp.autofit$residuals)
Pacf(temp.autofit$residuals)
temp.autofit 

plot(temp.train.ts,xlab="Time",ylab="Temparature",main="Monthly minimum Temparature(Auto Arima)",xaxs="i",lty=1)
lines(temp.autofit$fitted, lwd = 2,col="blue",lty=2)



temp.tslm.forc <- forecast(temp.tslm,h=104,level = 0)
temp.arima.fit.forc <- forecast(temp.arima.fit,h=104,level = 0)
temp.autofit.forc <- forecast(temp.autofit,h = 104,level = 0)

temp.tslm.pred <- predict(temp.tslm,h=104)
temp.arima.fit.pred <- predict(temp.arima.fit,n.ahead = 104)




accuracy(temp.tslm.forc,temp.valid.ts)
accuracy(temp.arima.fit.forc,temp.valid.ts)
accuracy(temp.autofit.forc,temp.valid.ts)

plot(temp.train.ts,xlab="Time",ylab="Temparature",main="Monthly minimum Temparature",xaxs="i",lty=1,xlim=c(1981,1994))
lines(temp.valid.ts)
lines(temp.tslm.forc$mean, lwd = 2,col="orange",lty=3)
lines(temp.arima.fit.pred$pred, lwd = 2,col="blue",lty=2)
legend(1991.5, 4, legend=c("Actual","Linear","Arima"),
       col=c("black","orange", "blue"), lty=c(1,3,2), cex=0.8)



# Final Forecasting


# Linear Model

temp.tslm.final <- tslm(temp.ts2 ~ trend + I(trend^2)+ season)
summary(temp.tslm.final)


# ARIMA Model
temp.arima.fit.final <-temp.ts2 %>% Arima(order=c(1,0,1),seasonal = c(1,1,1))
acf(temp.arima.fit.final$residuals)
pacf(temp.arima.fit.final$residuals)
temp.arima.fit.final

#Predictions

temp.tslm.final.pred <- forecast(temp.tslm.final,h= 52)
temp.arima.fit.final.pred <- predict(temp.arima.fit.final,n.ahead = 52)
temp.snaive.fit.pred <- predict(temp.snaive.fit,h=52)

plot(temp.ts2,xlab="Time",ylab="Temparature",main="Monthly minimum Temparature",xaxs="i",lty=1,xlim=c(1981,1993))
lines(temp.arima.fit.final.pred$pred, lwd = 2,col="blue",lty=1)
lines(temp.tslm.final.pred$mean, lwd = 2,col="orange",lty=1)
abline(v=1991,col = "red",lty =1)
legend(1981.5, 3, legend=c("Actual","Linear","Arima"),
       col=c("black","orange", "blue"), lty=c(1,1,1), cex=0.8)

text(1986, 19, "Actual",font=2)
text(1992, 19, "Forecast",font=2)

arrows(1981, 18, 1991, 18, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(1991, 18, 1992, 18, code = 3, length = 0.1, lwd = 1,angle = 30)
