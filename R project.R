#loading packages
library(forecast)
library(tseries)
library(readxl)
library(ggplot2)

#loading data
TempData <- read_excel("TempData.xlsx")
View(TempData)
str(TempData)


#creating the time series
tempratureTS <- ts(TempData$Temperature, start=c(1981,1), end=c(1990,12), frequency=12)
summary(tempratureTS)
str(tempratureTS)



#EDA 
tempratureTS
sum(is.na(tempratureTS))
frequency(tempratureTS)
cycle(tempratureTS)
plot(tempratureTS, xlab="Year", ylab="Temprature")
autoplot(tempratureTS, xlab ="Years" , ylab ="Temprature" )
boxplot(tempratureTS~cycle(tempratureTS), xlab="Month", ylab = "Temprature")

#time series decomposition
decomposeTempratureTS <- decompose(tempratureTS, "multiplicative")
plot(decomposeTempratureTS)

#testing stationarity of time series adf.test(tempratureTS)
adf.test(tempratureTS)

#testing autocorrelation
acf(tempratureTS)
pacf(tempratureTS)
autoplot(acf(tempratureTS,plot=FALSE))

decomposeTempratureTS$random

#splitting the dataSet
trainDataset.ts <- window (tempratureTS, start=c(1981,1), end=c(1988,12))
validationDataset.ts <-window (tempratureTS, start=c(1989,1), end=c(1990,12))
trainDataset.ts
validationDataset.ts

#forecasting 
#1 random walk with drift
tempratureTSDecomposeLog <- stl(log10(trainDataset.ts), s.window = 'p')
plot(tempratureTSDecomposeLog)
forecastTempratureSTL <- forecast(tempratureTSDecomposeLog, method = "rwdrift", h=24)
forecastTempratureSTL
plot(forecastTempratureSTL)
accuracy(forecastTempratureSTL)


#2 holtwintersmoothing - Simple exponential 
holtWinterSmoothing <- ets(trainDataset.ts, model="ZZZ")
holtwinterSmoothing.prediction <- forecast(holtWinterSmoothing, h=24)
plot(holtwinterSmoothing.prediction)
accuracy(holtwinterSmoothing.prediction)
holtWinterSmoothing
holtwinterSmoothing.prediction

#3 holtwintersmoothing - Double exponential 
holtWinterSmoothing <- ets(trainDataset.ts, model="ZZZ", gamma=FALSE)
holtwinterSmoothing.prediction <- forecast(holtWinterSmoothing, h=24)
plot(holtwinterSmoothing.prediction)
accuracy(holtwinterSmoothing.prediction)
holtWinterSmoothing
holtwinterSmoothing.prediction

#4 auto arima
autoarimaTempratureTS <- auto.arima(trainDataset.ts)
autoarimaTempratureTS
forecastTempratureAA <- forecast(autoarimaTempratureTS, h=24)
plot(forecastTempratureAA)
accuracy(forecastTempratureAA)


#5 My Arima (0,0,0) season (1,1,1)
?arima
myArima1 <- arima(trainDataset.ts, order=c(0,0,0), seasonal = c(1,1,1))
accuracy(myArima1)
myArima2 <- arima(trainDataset.ts, order=c(12,1,1), seasonal = c(1,1,1))
accuracy(myArima2)
myArima3 <- arima(trainDataset.ts, order=c(1,1,1), seasonal = c(1,1,1))
accuracy(myArima3)
myArima4 <- arima(trainDataset.ts, order=c(0,1,1), seasonal = c(1,1,1))
accuracy(myArima4)
plot(myArima2)
acf(myArima2$residuals)
pacf(myArima2$residuals)
myArimaforecast <- forecast(myArima2, h=24)
plot(myArimaforecast)
accuracy(myArimaforecast)




#6 tslm
TempratureTSLM <- tslm(trainDataset.ts~trend+season, lambda = 0)
summary(TempratureTSLM)
forecastTempratureTSLM <- forecast(TempratureTSLM, h=24)
forecastTempratureTSLM
accuracy(forecastTempratureTSLM)
plot(forecastTempratureTSLM)+guides(colour=guide_legend(title="FOrecast"))




