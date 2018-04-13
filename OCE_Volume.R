## Useful libraries for time series forecast. There are overlaps in functionalities ##
library(smooth)
library(forecast)
library(graphics)
library(datasets)
library(tseries)
library(fpp2)
install.packages("xlsx")
library(xlsx)
# Import datasets #
GVO <- OCE_Controlled_Monthly_Volume_Forecast_2018
# Load CompanyX data from Library{datasets}#

CompanyX <- ts(GVO[[3]], start=c(2014, 1), end=c(2017, 12), frequency=12)
#CompanyX <- ts(OCE_CNY[[2]], start=c(2015, 1), end=c(2018, 10), frequency=17)

plot(CompanyX)
BY <- CompanyX


BY3 <- ma(BY, order=3) 
BY9 <- ma(BY, order=5)
BY19 <- ma(BY, order=7)

ts.plot(BY, BY3, BY9, BY19, lty=c(1:4), col=c('black','red','dark blue', 'forest green'))
BY3
# Forecasting using MA #
#Library{smooth} #

BY3H <- sma(BY, order=3, h=17, holdout=T, intervals="p", level=0.95) 
BY9H <- sma(BY, order=9, h=10, holdout=T, intervals="p", level=0.95) 
plot(BY3H)
BY3H$forecast
BY9H$forecast
#################################
## TS Decomposition ##

monthplot(CompanyX, col='maroon', main="Seasonal Subplot")


# Decompose time series into multiplicative components #
CompX.mult <- decompose(CompanyX, type="multiplicative")
plot(CompX.mult$figure, type="l") # Plots seasonality indices
plot(CompX.mult$trend, type="l") # Plots trend
plot(CompX.mult)
CompX.mult$trend
CompX.mult$random
# Predictive Accuracy: MAPE #

## CompanyX  ##
CompX1 <- window(CompanyX, start=c(2014,1), end=c(2017,1))
CompXHO <- window(CompanyX, start=c(2017,2), end=c(2017,12))

# Predict next 17 future values and compare #


## CompanyX ##

CompX.hw1 <- hw(CompanyX, seasonal='m', initial='o')
CompX.hw1$model

#CompXFC <- hw(CompX1, seasonal='m',alpha=0.140, beta=0.0001, gamma=0.002, h=11 )
CompXFC <- hw(CompX1, seasonal='m',alpha=0.140, beta=0.0009405, gamma=0.0025, h=11 )

plot(CompXFC)

CompXFC$mean

  CompXFC$fitted
#write.xlsx(CompXFC$mean, "/ALBERTA2.xlsx")

## Forecast by Exponential Smoothing ##

Vec2<- cbind(CompXHO,CompXFC$mean)
ts.plot(Vec2, col=c("blue", "red"), main="CompanyX: Actual vs Forecast")
MAPE <- mean(abs(Vec2[,1]-Vec2[,2])/Vec2[,1])
MAPE


# predictive accuracy #
library(forecast)


######################

# Autocorrelation #

######################

# Differencing
Clay <- CompX1
adf.test(Clay)

Clay1 <- lag(Clay, k=-1)
Claydf1<- diff(Clay, lag=1) # Lag 1 difference

Clay2 <- lag(Clay, k=-2)
Claydf2<- diff(Clay, lag=2) # Lag 2 difference

plot(Claydf1, col="purple", main="First Difference: Clay Brick")
adf.test(Claydf1)

acf(Clay, main="Clay Brick")
acf(Claydf1, main="First Difference: Clay Brick")

##### BMW ######

BMW <- CompX1
plot(BMW, col="blue")
acf(BMW, lag=100)
pacf(BMW, lag=100)

BMW1 <- diff(BMW, order=1)
plot(BMW1, ylab="First Difference: BMW", col="brown")
acf(BMW1, lag=50)
pacf(BMW1, lag=50)

BMW.autofit <- auto.arima(BMW)
summary(BMW.autofit)

#BMW.arima.fit <- arima(BMW, c(2, 0, 0), c(1,1,0))

BMW.arima.fit <- arima(BMW, c(2, 0, 0), seasonal = list(order = c(1,1,0), period = 12),
                       method = "CSS-ML")
summary(BMW.arima.fit)

pred1 = predict(BMW.autofit, n.ahead = 12)
pred1

pred2 = predict(BMW.arima.fit, n.ahead = 12)
pred2

  par(mfrow=c(1,2))
acf(ts(BMW.autofit$residuals),main='ACF Residual')
pacf(ts(BMW.autofit$residuals),main='PACF Residual')

par(mfrow=c(1,2))
acf(ts(BMW.arima.fit$residuals),main='ACF Residual')
pacf(ts(BMW.arima.fit$residuals),main='PACF Residual')

Box.test(BMW.arima.fit$residuals, lag = 36, type = "Ljung-Box")
Box.test(BMW.autofit$residuals, lag = 36, type = "Ljung-Box")
adf.test(BMW)
adf.test(BMW1)
