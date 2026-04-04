library(forecast)
library(tseries)
library(urca)
library(fracdiff)
#we require fracdiff for fractional differncing of tseries

set.seed(123)
n<-10000
d<-0.3
ar_coeff<-c(0.5)#setting ar coefficients 
ma_coeff<-c(0.2)#setting up the ma coeff

#simu8lating arfima model
arfima_sim<-fracdiff.sim(n,d=d,ar=ar_coeff,ma=ma_coeff)
#plot the simulated time series
plot(arfima_sim$series)

#estimate the arfima parameters 
arfima_model<-arfima(arfima_sim$series)

#get summary using summary command 
summary(arfima_model)
##as we can see using the summary command we get that estimated value of d is very near to actual d set by us 

#we can plot ACF and PACF plot side by side using the par command 
par(mfrow=c(1,1))#c(x,y) x denotes the row and y denotes the columns 
Acf(residuals(arfima_model),main="acf plot ")
Pacf(residuals(arfima_model),main="pacf plot")

#we check for stationarity of residuals using KPSS test! In that we have null hypo as residuals are stationay 
kpss_test<-ur.kpss(residuals(arfima_model))
summary(kpss_test)

#forecasting using ARFIMA model 
forecast_horizon<-2000
arfima_forecast<-forecast(arfima_model,h=forecast_horizon)
plot(arfima_forecast)

#data now is airpassengers
data("AirPassengers")
y<-AirPassengers
plot(y,main="monthly airline passengers ")

kps_test<-ur.kpss(y)
summary(kps_test)
#under kpss the series is non stationary as t stat value very greater than any value of significane level

#differncing the series to make it stationary (if needed)
differenced_y<-diff(y)

#check acf and pacf of differenced series
par(mfrow=c(1,2))
Acf(differenced_y)
Pacf(differenced_y)

arfima_model1<-arfima(y)
summary(arfima_model1)

Acf(residuals(arfima_model1))
Pacf(residuals(arfima_model1))
#in the above plots we can see significant spikes at lag 12 and 24 which indicates seasonblity

kpss_test_residual<-ur.kpss(residuals(arfima_model1))
summary(kpss_test_residual)
#test-statistic is: 0.7253 which is greater than 0.574 thus we reject the null and say series is non stationary 

forecast_h<-12
arfima_f<-forecast(arfima_model1,h=forecast_h)
plot(arfima_f)
#we can see that arfima isnt able to capture the trend properly!!