data(AirPassengers)
AP <- AirPassengers
str(AP)
head(AP)
ts(AP, frequency  =12, start = c(1949,1))
attributes(AP)
plot(AP)

#We can observe that the time series is non stationary, so we do log Tranform

#Log Transform
AP <- log(AP)
plot(AP)
#Now, we can observe it better, now its additive

decomp <- decompose(AP)
decomp$figure
plot(decomp$figure,
     type = 'b',
     xlab = 'month',
     ylab = 'Seasonality Index',
     col = 'blue',
     las = 2) # So that numbers appear vertical
# June<July,AUg,Sept have high passengers travelling

plot(decomp)


#ARIMA: Auto Regressive Integrated Moving Average
install.packages("forecast")
library(forecast)

model <- auto.arima(AP)
model

#p: AR order
#d: degree of differencing
#q: MA order
#AIC: Akaike information criterion
#AICc: AIC with correction
#BIC: Bayesian information criterion
attributes(model)
model$coef

# ACF and PACF plots

acf(model$residuals, main = 'Correlogram')
#residuals: diff bw fitted and actual values
#dotted lines are significance bounds
#Autocorrelations for insample forecast errors do not exceed significance bounds
# We can see 1 line just touches the significance bounds, 
#So we can't say if some values are significant or not. So we do Ljung Box test
pacf(model$residuals, main = 'Partial correlogram')


#Ljung-Box test

Box.test(model$residuals, lag = 20, type = 'Ljung-Box')
#Gives us the summary- X squared values, df(degrees of freedom)
#p value- if <0.05 for 95% confidence interval, can say there is statistical significant
# but its quite high so can conclude that there is little evidence of non zero auto correloations in the insample forecast errors at lags 1 to 20


#Residual Plot
hist(model$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogram of Residuals',
     freq = FALSE)

#Highest density values are concentrated around zero
# Looks like a normal distribution curve
#Cans ee by plotting
lines(density(model$residuals))
# So, curve is fina and we can do forecast

#Forecast
f <- forecast(model, 48) # Forecasting for next 48 months
library(ggplot2)
autoplot(f)
accuracy(f)
