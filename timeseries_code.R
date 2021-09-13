#install.packages("fpp2")
library(fpp2)



#install.packages("xlsx")
#install.packages("readxl")



library(xlsx)
library(readxl)
library(forecast)
library(ggplot2)
library(tseries)



Production <- read_excel("C:/Users/prath/Desktop/STATS/TA/Time series/Production_Data.xlsx")
Production


Production$TIME <- NULL
Production

#creating a time series object in R
model <- ts(Production, start=c(2016),frequency = 12)
model


start(model)
end(model)
frequency(model)
plot(model)
autoplot(model)
ggtsdisplay(model)

ggseasonplot(model, year.labels = TRUE, year.labels.left = TRUE)+
  ylab("production") +
  ggtitle("seasonal plot")


ggsubseriesplot(model) + ylab("production") + ggtitle("seasonal plot")


#Plotting a simple moving average
plot(model, main = "Raw time series")
plot(ma(model, 5))

autoplot(model)+
  autolayer(ma(model, 7))+
  autolayer(ma(model, 3))


ggtsdisplay(model)



# seasonal decomposition
model
plot(model)
fit.dec <- decompose(model, type = "multiplicative")
fit.dec
plot(fit.dec)


#Forecasting method
## Average Method (average of all previous observation of time series)
prod_mean <- meanf(model,h=3)
summary(prod_mean)
plot(prod_mean)



forecast(prod_mean,3)
plot(forecast(prod_mean,3), xlab = "TIME", ylab = "Production")



## NAIVE MODEL(Random walk model)
prod_naive <- naive(model,h=3)
summary(prod_naive)
plot(prod_naive)



forecast(prod_naive,3)
plot(forecast(prod_naive,3), xlab = "TIME", ylab = "Production")


## SEASONAL NAIVE MODEL
prod_seasonnaive <- snaive(model,h=3)
summary(prod_seasonnaive)
plot(prod_seasonnaive)



#fit simple exponential smoothing model
exfit <- ses(model, h=2)
exfit
round(accuracy(exfit), 2)
autoplot(exfit)
autoplot(exfit) + autolayer(fitted(exfit), series = "fitted")



#Fitting with holt
model
plot(model)
modelfit <- holt(model)
modelfit <- holt(model, h=5)
modelfit

#holtWinters model

fit1 <- window(model, start = 2016)
fit2 <- hw(fit1 , seasonal = "additive")
fit3 <- hw(fit1 , seasonal = "multiplicative")
autoplot(fit1)+
  autolayer(fit2, series = "HW additive forecast", PI = FALSE)+
  autolayer(fit3, series = "HW multiplicative forecast", 
            PI = FALSE)+
  xlab("Month")+
  ylab("production")+
  ggtitle("total production")+
  guides(colour=guide_legend(title = "forecast"))

accuracy(fit2)
accuracy(fit3)


#differencing and checking for stationary
plot(model)
#checking the order of deferencing is required
ndiffs(model)

#augmented Dickey-fuller Test
adf.test(model)


#ACF/PACF plots
Acf(model)
Pacf(model)


#fitting ARIMA model
fitAR <- arima(model, order = c(2,0,2))
fitAR
checkresiduals(fitAR)


fitAR <- arima(model, order = c(0,0,9))
fitAR
checkresiduals(fitAR)

accuracy(fitAR)
qqnorm(fitAR$residuals)
qqline(fitAR$residuals)
hist(fitAR$residuals)

#Evaluating model fit

Box.test(fitAR$residuals, type = "Ljung-Box")


#forecasting with the fitted model
forecast(fitAR, 3)
plot(forecast(fitAR, 3), xlab = "month", ylab = "Total production")


#Automated ARIMA Forecasting
plot(model)
fitAAR <- auto.arima(model)
fitAAR


accuracy(prod_naive)
accuracy(prod_seasonnaive)
accuracy(exfit)
accuracy(fit2)
accuracy(fit3)
accuracy(fitAR)





