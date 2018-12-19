#Dataset 3: Air Passenger 1946 -1960
# Huong, LE

#IMPORT THE LIBRARY
library(ggfortify)
library(tseries)
library(forecast)

#IMPORT THE DATA
df = read.csv('AirPassenger_1949.csv',header=F)

#SPLIT INTO TRAINING SET AND TEST SET
trainingset = df[1:132,]
testset = df[133:144,]

#CONVERTING TRAINING SET INTO TIMESERIES
model = ts(trainingset, frequency = 12, start = c(1949,1))

#PLOT THE DATA
plot.ts(model, main = 'Time Series of Air Passenger 1949-1959')
#DECOMPOSE DATA: trend and seasonal
autoplot(decompose(model))

#EXAM ACF AND PACF TO FIND GOOD Q AND P
par(mfcol = c(2,1 ))
acf(model, main="ACF of Number of Air passenger in a year")
acf(model, type="partial", main="PACF of Number of Air passenger in a year")

#DECOMPOSE THE TREND AND SEASONAL 
decompose<- decompose(model,"multiplicative")
autoplot(decompose)
x = decompose$random 
autoplot(acf(decompose$random[7:126],plot=FALSE))+ labs(title="Correlogram of Air Passengers Random Component from 1949 to 1959") 

#RUN auto.arima()
arima <- auto.arima(model)
arima
ggtsdiag(arima)
# Series: model 
# ARIMA(1,1,0)(0,1,0)[12] 
# 
# Coefficients:
#   ar1
# -0.2431
# s.e.   0.0894
# 
# sigma^2 estimated as 109.8:  log likelihood=-447.95
# AIC=899.9   AICc=900.01   BIC=905.46

#PREDICTION AND VISUALIZATION
forecast <- forecast(arima, level = c(95), h = 12)
forecast
autoplot(forecast)


preds <- predict(arima, n.ahead = 12)
preds

#VISUALIZE TESTSET AND PREDICTION
ts.plot(testset,preds$pred, main = 'Actual vs ARIMA Predictions',
        col = c('green','black'), lty = c(1,2), lwd = c(3,3))
legend('topleft',legend = c('Actual Data','ARIMA Predictions'), 
       col = c('green','black'), lwd = c(3,3), lty = c(1,2))


#VISUALIZE THE MODEL AND ARIMA MODEL
plot(model, col = 'green', main = 'Actual vs ARIMA Model',
     ylab = 'Deaths', lwd = 3)
lines(arima$fitted, col = 'black', lwd = 3, lty = 2)
legend('topleft',legend = c('Actual','ARIMA Fit'), 
       col = c('green','black'), lwd = c(3,3), lty = c(1,2))

