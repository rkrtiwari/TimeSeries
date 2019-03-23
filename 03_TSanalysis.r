
# Data Loading

USbeer=read.csv(file.choose())   # choose US beer dataset
beer.ts=ts(USbeer[,2], frequency=12, start=c(1970,1), end=c(1977,12))
ts_info(beer.ts)

### Decompose
# Additive:
# Y = trend + seasonality + remainder

# Multiplicative:
# Y = trend * seasonality * remainder

# decompose the time series
library(forecast)
ts_decompose(beer.ts, type="additive")

ts_decompose(beer.ts, type="multiplicative")

# decomposition based on Loess smoother (STL)
dc <- stl(beer.ts, s.window="per") # periodic seasonality
plot(dc)

dc <- stl(beer.ts, s.window=13) # seasonality extraction with specified window length
plot(dc)

dc <- stl(beer.ts, s.window=13, t.window=121) # seasonality and trend extraction 
#with specified window length
plot(dc)


### 2 Stationarize

# Detrending : Here, we simply remove the trend component from the time series.
# Differencing : This is the commonly used technique to remove non-stationarity.
# Seasonality : Seasonality can easily be incorporated in the ARIMA model directly.

# Dickey-Fuller Test
## see if it is stationary enough to do time series modelling
library(tseries)
plot(diff(log(beer.ts)))
adf.test(diff(log(beer.ts)))


##################### Exponential Smoothing ####################
# simple moving average (SMA). 
# -------------------------

library("TTR")
par(mfrow=c(1,1))
plot.ts(beer.ts)
lines(SMA(beer.ts,n=3), col='red')
lines(SMA(beer.ts,n=12), col="blue")


# smoothing with splines
#-----------------------
beer.ts.spl <- smooth.spline(beer.ts)
beer.ts.spl # this is not a time series any more
plot(beer.ts.spl)

beer.ts.spl <- ts(beer.ts.spl$y, frequency=12, start=c(1970,1), end=c(1977,12))
plot(beer.ts)
lines(beer.ts.spl, col="red")


# smoothing with running windows
#-------------------------------
# rolling median
beer.ts.rmed <- rollmedian(beer.ts, 3)
plot(beer.ts)
lines(beer.ts.rmed, col="red")

# rolling max
beer.ts.rmed <- rollmax(beer.ts, 3)
plot(beer.ts)
lines(beer.ts.rmed, col="red")

# use a own function, e.g. quantile 0.9
beer.ts.q09 <- rollapply(beer.ts, 3, FUN=function(x) {
  quantile(x, 0.9, na.rm=TRUE)
})
plot(beer.ts)
lines(beer.ts.q09, col="red")


############################### Forecast models ##################################

# Simple Average - simple average of all data points
# Naive Method - the last observation value
# Seasonal Navie - the last observation value from previous seasonal cycle
# Drift Method - forecast value increase or decrease over time based on 
# average change in historical data

beer.fit.a <- meanf(beer.ts, h = 120) # h: number of period for forecasting
beer.fit.n <- naive(beer.ts, h = 120)
beer.fit.sn <- snaive(beer.ts, h = 120)
beer.fit.dri <- rwf(beer.ts, h = 120, drift = TRUE) #random walk forecast

par(mfrow=c(1,1))

plot.ts(beer.ts, main = "Monthly Beer Production in Australia", xlab = "Year", ylab = "ML", xlim = c(1970, 1980))
lines(beer.fit.a$mean, col = "blue")
lines(beer.fit.n$mean, col = "yellow4")
lines(beer.fit.dri$mean, col = "seagreen4")
lines(beer.fit.sn$mean, col = "red")
legend("topleft",lty=1,col=c("blue","yellow4","seagreen4", "red"), cex = 0.75,
       legend=c("Mean method","Naive method","Drift Naive method", "Seasonal naive method"))


#### regression analysis
# without seasonlity
beer.fit.lm <- tslm(beer.ts ~ trend)
summary(beer.fit.lm)
f <- forecast(beer.fit.lm, h = 120, level = c(80,95)) # h: number of period
# for forecasting, level: confidence level for prediction interval
plot.ts(beer.ts, main = "Monthly Beer Production in Australia", xlab = "Year", 
        ylab = "ML", xlim = c(1970,1985))
lines(f$fitted, col = "blue")
lines(f$mean, col = 'red')


# with seasonality
beer.fit.lm2 <- tslm(beer.ts ~ trend + season)
summary(beer.fit.lm2)
f2 <- forecast(beer.fit.lm2, h = 120, level = c(80,95))
plot.ts(beer.ts, main = "Monthly Beer Production in Australia", xlab = "Year", 
        ylab = "ML", xlim = c(1970,1990), ylim = c(8,20))
lines(f2$fitted, col = "blue")
lines(f2$mean, col = 'red')


#### simple exponential smoothing (ses)
# Used for the time seriese without trend and seasonality
# alpha: smoothening parameter
# h: number of periods for forecasting
plot.ts(beer.ts, main = "Monthly Beer Production in Australia", 
        xlab = "Year", ylab = "ML", xlim = c(1970,1978))

beer.fit.ses1 <- ses(beer.ts, alpha = 0.20, h = 12)
beer.fit.ses2 <- ses(beer.ts, alpha = 0.60, h = 12)
beer.fit.ses3 <- ses(beer.ts, alpha = 0.87, h = 12)

plot(beer.fit.ses1, type="o", 
     main = "Monthly Beer Production in Australia", 
     xlab = "Year", ylab = "ML")
lines(beer.fit.ses1$fitted, col = "blue", type="o")
lines(beer.fit.ses1$mean, col = "blue", type="o")
lines(beer.fit.ses2$fitted, col = "green", type="o")
lines(beer.fit.ses2$mean, col = "green", type="o")
lines(beer.fit.ses3$fitted, col = "red", type="o")
lines(beer.fit.ses3$mean, col = "red", type="o")
legend("topleft",lty=1, col=c(1,"blue","green","red"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.87)), pch = 1)


### Holt's Linear Trend Method
# In the Holt's linear trend models, there are 3 variations.

# Holt's Linear Trend
# Expontential Linear Trend - The level and slope are mutipled instead of 
# added in Holt's model
# Damped Trend - The trend became flat after a period increase or decrease. 
# It is usually very useful in the business world since typical growth or 
# decline will stop after a certain period of time.


beer.ts.yrl <- aggregate(beer.ts, nfrequency=1) # compute summary statistics
beer.ts.yrl <- window(beer.ts.yrl , start = 1970, end = 1975) #Extract subset
beer.fit.holt1 <- holt(beer.ts.yrl, alpha = 0.2, beta = 0.2, h = 6)
beer.fit.holt2 <- holt(beer.ts.yrl, alpha = 0.2, beta = 0.2, 
                       initial = 'simple', exponential = TRUE, h = 6)
beer.fit.holt3 <- holt(beer.ts.yrl, alpha = 0.2, beta = 0.2, 
                       initial = 'optimal', damped = TRUE, h = 6)

plot(beer.fit.holt1, type="o", fcol="white", 
     main = "Yearly Beer Production in Australia", 
     xlab = "Year", ylab = "ML")
lines(beer.fit.holt1$fitted, col = "blue")
lines(beer.fit.holt2$fitted, col = "green")
lines(beer.fit.holt3$fitted, col = "red")
lines(beer.fit.holt1$mean, col = "blue", type="o")
lines(beer.fit.holt2$mean, col = "green", type="o")
lines(beer.fit.holt3$mean, col = "red", type="o")
legend("topleft", lty = 1, col = c("black", "blue", "green", "red"), 
       c("Data", "Holt's Linear Trend", "Exponential Trend", "Damped Trend"))

## Holt-Winters' Seasonal Trend
beer.ts3 <- window(beer.ts, start = 1960, end = 1975)
beer.ts.qtr <- aggregate(beer.ts3, nfrequency=4)
beer.fit.hw1 <- hw(beer.ts.qtr, h = 20, seasonal = "additive")
beer.fit.hw2 <- hw(beer.ts.qtr, h = 20, seasonal = "multiplicative")

plot(beer.fit.hw1, type="o", fcol="white", main = "Quarterly Beer Production in Australia", 
     xlab = "Year", ylab = "ML")
lines(beer.fit.hw1$fitted, col = "blue", lty=2)
lines(beer.fit.hw2$fitted, col = "red", lty=2)
lines(beer.fit.hw1$mean, col = "blue", type="o")
lines(beer.fit.hw2$mean, col = "red", type="o")
legend("topleft", lty = 1, pch = 1, col = c("black", "blue", "red"),
       c("Data", "Holt Winters' Additive", "Holt Winters' Multiplicative"))


#################################################################################
# ARIMA Model

fit <- auto.arima(beer.ts, ic="bic", approximation=T, trace=FALSE, allowdrift=F)
summary(fit)
accuracy(fit)
pred <- predict(fit, n.ahead = 50)
ts.plot(beer.ts, pred$pred, lty = c(1,3), col=c(5,2))


# Neural nework is probably one of the hottest machine learning algorithms which models human brain 
#and neural system. The lagged value can be used as inputs to a neural network similar to autoregression model

beer.fit.nn <- nnetar(beer.ts)
summary(beer.fit.nn)
plot(forecast(beer.fit.nn, h = 20), xlab = "Year", ylab = "ML", 
     xlim = c(1970, 1980), ylim = c(0, 20))

