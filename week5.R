library(TTR)
#ttr is used for technical trading rules it has all the basics like moving avg, macd etc

data("nottem")
force(nottem)
#force is used to force the data set inside environment and we can work on that 

#STL is used to decompose the time series in seasonal trend and residual aspect
nottem_stl<-stl(nottem,s.window = "periodic")

plot(nottem)
plot(nottem_stl)
#we can see a seasonal aspect but no trend of upwards or downwards trend

data("AirPassengers")
head(AirPassengers)
plot(AirPassengers)

airpass_stl<-stl(AirPassengers,s.window = "periodic")
plot(airpass_stl)

airpass_ts<-ts(AirPassengers)

airpass_ts_sma3<-SMA(airpass_ts,n=3)
airpass_ts
airpass_ts_sma3
#when we un sma3 we cam observe that first 2 values are na because they are used as basis on which we calculate the avg
plot(airpass_ts_sma3)
plot(airpass_ts-airpass_ts_sma3)


airpass_ts_sma8<-SMA(airpass_ts,n=8)
plot(airpass_ts_sma8-airpass_ts)

#we can say that variance for sma3 is much lesser as compared to sma8 hence we should go with 3 if selecting from sma

##ema
airpass_ema3<-EMA(AirPassengers,n=3)
airpass_ema8<-EMA(AirPassengers,n=8)

plot.ts(airpass_ema3) 
plot.ts(airpass_ema8)

plot.ts(EMA(AirPassengers,n=12))
#we should choose ema=3 cuz we as degree inc plot tends to get smoother so its better if we choose a plot in which tendency of data is preserved

#holt expo smoothing forecasting 

Airpassf<-HoltWinters(AirPassengers,gamma = F)
Airpassf
Airpassf$SSE
plot(Airpassf)
library(forecast)
airpassf19<-forecast(Airpassf,h=19)
plot(airpassf19)
#we can seee that the forecast isnt properly fittin into the dataset and its failing to capture the seasonality 
#hence we move to holt wiinter which is able to capture all the seasonality and the trends properly
#holts winter 

hw_air<-HoltWinters(log(AirPassengers))
#we apply log transformation to deal with the changing variance problem of dataset 
#hece we use log transformation to deal with that problem 

hw_air
hw_air$SSE
airf10<-forecast(hw_air,h=10)
#forecast ahead for 10 years 
plot(airf10)
#we can clearly see that holtwinters is able to capture the seasonality very well rather than onloy holts technique 
airf10$mean
