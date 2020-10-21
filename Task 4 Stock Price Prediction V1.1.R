setwd("D:/Study/Internsala/Spark Foudation Grip/Task 4")

# Stock price/performance prediction using numerical analysis of historical stock prices
# Stock to analyze and predict - SENSEX (S&P BSE SENSEX)

# Importing Libraries

library(quantmod)
library(tseries)
library(xts)
library(timeSeries)
library(forecast)

# Pull data from Yahoo finance
getSymbols('^BSESN', from = '2015-01-01', to= '2020-09-30')  # around 5 years of data for S&P BSE SENSEX
class(BSESN)  ## XTS zoo
BSESN_close_price = BSESN[,4]

# PLot the data
plot(BSESN_close_price)
par(mfrow= c(1,1))

# Plot the PACF and ACF , lookig for the identifiable lags
par(mfrow=c(1,2))
Acf(BSESN_close_price, main= 'ACF for the differetiates time series')
Pacf(BSESN_close_price, main = 'PACF for differentiated time series')

# Check for missing values
anyNA(BSESN_close_price)
sum(is.na(BSESN_close_price)) # 5 na in the data set
BSESN[complete.cases(BSESN[,4]), ]

# Imputation and removing of NAs

library(tidyr)
row.has.na <- apply(BSESN_close_price, 1, function(x){any(is.na(x))})
sum(row.has.na)
BSESN_close_price_filtered <- BSESN_close_price[!row.has.na,]

# another way to drop na
BSESN_close_price_filtered_1<-BSESN_close_price %>% na.omit()

# FIlling na values Fill missing values  using last observation
BSESN_close_price_filtered_2<-BSESN_close_price %>% na.locf() 

# Filling missing value Fill missing values using next observation
BSESN_close_price_filtered_3<-BSESN_close_price %>% na.locf(fromLast = T) 

# Filling approximate values in place of NAs
BSESN_close_price_filtered_4<-BSESN_close_price %>% na.approx() 


# ADF test for stationarity of the time series
print(adf.test(BSESN_close_price_filtered))  #  p value = .09282 time serie is stationary

# Lets use Auto arima for forecasting
auto.arima(BSESN_close_price_filtered, seasonal = F) # ARIMA(2,1,0)  AIC=20615.7   AICc=20615.72   BIC=20631.46

# fit the model
fit_1 = auto.arima(BSESN_close_price_filtered, seasonal = F)
tsdisplay(residuals(fit_1), lag.max = 40, main = "(2,1,0) Model residual")

fit_2 = arima(BSESN_close_price_filtered, order = c(1,1,36))
tsdisplay(residuals(fit_2), lag.max = 40, main = "(1,1,36) Model Residuals")  # No lags


fit_3 = arima(BSESN_close_price_filtered, order = c(1,1,31))
tsdisplay(residuals(fit_3), lag.max = 40, main = "(1,1,1) Model Residuals")  # Default models lets check for exploring

# PLot the arima models term = 100 days  
par(mfrow=c(2,2))
term <- 100
fcast1 <- forecast(fit_1, h=term)
plot(fcast1)
fcast2<- forecast(fit_2, h=term)
plot(fcast2)
fcast3 <- forecast(fit_3, h=term)
plot(fcast3)
# From all the three plots we can see that there is holding pattern reflected.
# In the plot 2 we can see there is slight incerease in the forecasted values for the next 100 days. Kind of like buying the stock

# Accuracy mape substract from 100
accuracy(fcast1)
100 -0.7400135  #  99.25999%
accuracy(fcast2)
100-0.7360962   # 99.2639%   ****** best accuracy
accuracy(fcast3)
100-0.7381304  # 99.26187%


# Ploting for next 50 days
term <- 50
fcast1 <- forecast(fit_1, h=term)
plot(fcast1)
fcast2<- forecast(fit_2, h=term)
plot(fcast2)
fcast3 <- forecast(fit_3, h=term)
plot(fcast3)

# for next 200 days it is saying to hold off for all the three models
# Ploting for next 200 days
term <- 200
par(mfrow=c(2,2))
fcast1 <- forecast(fit_1, h=term)
plot(fcast1)
fcast2<- forecast(fit_2, h=term)
plot(fcast2)
fcast3 <- forecast(fit_3, h=term)
plot(fcast3)


# for next 200 days also all models saying to hold off the stocks
