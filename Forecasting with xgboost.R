library(forecast)
library(dplyr)
library(xgboost)
temp.data <- read_excel('C:/Users/phalg/Downloads/R files/TempData.xlsx')
temp.data$Date = strptime(temp.data$Date, "%Y-%m-%d")
temp.data$Date  <-  as.POSIXct(temp.data$Date)
temp.data$Temperature <- as.numeric(temp.data$Temperature)

#creating a ts object
temp.ts <- ts(temp.data$Temperature,start = c(1981,0),end = c(1990,365),frequency = 365) 
plot(temp.ts)

#plotting decomposed series - additive
temp.decomp <- decompose(temp.ts)
plot(temp.decomp)

#plotting decomposed series - multiplicative
temp.decomp <- decompose(temp.ts,"multiplicative")
plot(temp.decomp)

#partitioning training and validation data
temp.train.ts <- window (temp.ts, start=c(1981,1), end=c(1988,365))
temp.valid.ts <-window (temp.ts, start=c(1989,1), end=c(1990,365))
length(temp.valid.ts)
length(temp.train.ts)

#fitting a tslm model
model1 <- tslm(temp.train.ts ~ trend + I(trend^2) + season)
pred11 <- forecast(model1,h=730,level=0)
summary(model1)
# Acf(model1$residuals,lag.max = 60)
# Pacf(model1$residuals,lag.max = 60)
plot(temp.train.ts,type="l",col="red",ylab="Temperature in Celsius",
     xlab="Year",ylim=c(0,35),xlim=c(1981,1990.5))
lines(model1$fitted.values,col="blue")
rect(1989,-2,1991,38,col="lightgrey")
lines(temp.valid.ts,col="darkgreen")
lines(pred1$mean,col="black")
legend(1985,38, c("Training (true)","Training (forecast)",
                   "Validation (true)","Validation (forecast)"),
       lty=c(1,1,1,1),
       col=c("red", "blue", "darkgreen","black"), bty="n",cex=0.6)
box()
box(which="plot")
accuracy(pred1$mean, temp.valid.ts)
#autoplot
autoplot(temp.train.ts,ylab="Temperature 
         in Celsius",xlab="Year",series="Training (true)") + 
  autolayer(temp.tslm$fitted.values,
            series="Training (forecast)")+
  autolayer(temp.valid.ts, series="Validation (true)")+
  autolayer(pred11$mean,series="Validation (forecast)")



#box transformations
lambda <- BoxCox.lambda(temp.train.ts)
lambda
autoplot(BoxCox(temp.train.ts,lambda))

#fitting a model with sinusoidal cosine method
x=seq(1,length(temp.train.ts))
model2 <- tslm(temp.train.ts ~ sin(2*pi/365*x)+cos(2*pi/365*x))
plot(temp.train.ts,type="l",col="red",ylab="Temperature in Celsuis",
     xlab="Year")
lines(model2$fitted.values,col="blue",lwd=2)
Acf(model2$residuals,lag.max = 30)
Pacf(model2$residuals,lag.max = 30)
#autoplot
autoplot(temp.train.ts,
         ylab="Temperature in Celsuis",
     xlab="Year",color="cyan") + 
  autolayer(model2$fitted.values,lwd=1)

  
#non-seasonal arima 1
model3 <- Arima(model2$residuals,order=c(2,0,0),seasonal = c(0,0,0))
Acf(model3$residuals,lag.max = 30)
Pacf(model3$residuals,lag.max = 365)
plot(model2$residuals,type="l",col="red",ylab="Temperature in Celsuis",
     xlab="Year")
predict=predict(model3,n.ahead=350)
lines(predict$pred+11.12704+1.83179*sin(2*pi/365*x)+3.81381*cos(2*pi/365*x),type="l",col='red')
lines(model3$fitted.values,col="blue",lwd=2)
Acf(model2$residuals,lag.max = 30)
Pacf(model2$residuals,lag.max = 30)


#non-seasonal arima 2
model4 <-temp.train.ts %>% Arima(order=c(1,1,1),seasonal=list(order=c(1,0,0),period=12))
Acf(model4$residuals, lag.max = 365)
Pacf(model4$residuals, lag.max = 365)

#seasonal arima
model4 <-temp.train.ts %>% Arima(order=c(1,1,0),seasonal = c(0,0,1))
Acf(model4$residuals)
Pacf(model4$residuals)

#xg boost model
data2 <- temp.data %>% 
  dplyr::select(Date, Temperature)
data3 <- xgb.DMatrix(as.matrix(data2 %% dplyr::select(Date)))



