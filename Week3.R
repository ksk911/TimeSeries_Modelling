library(tseries)
library(forecast)
getwd()
##using "as.ts" converts it to time seriees datastructure 
bank_case<-as.ts(scan('bank_case.txt'))
bank_case
par(mfrow=c(1,1))
plot(bank_case,main="timeseries")
##one can clearly observe that the mean is non stationary and to verify that we can also check it 
##using the acf and pacf test
acf(bank_case,main="acf for real estate loan dataset",lag.max = 10)
##for any given lag we can see that lags are outside the limits hence significant 
##the blue linhe is significance level 
##lagmax 10 tells us that we are checking if there is any echo in 10 months apart ie value in october is it affected by jan??
#as we cannot see any decay te model we can also tell from the acf test the dataset is non stationary 
pacf(bank_case,lag.max = 10,main="pacf for dataset")
##after running the pacf plot we can see that after lag 1 all the correlation is insignificant! 
#however from acf plot we can conclude that the dataset is nonstationary, hence its very important to use both plots 

adf.test((bank_case))
##if p value is high we fail to reject the null hypo and accept it as being true 
##so we accept it as non stationary data

##whenever we have a trend we difference it 
bank_case_d1<-diff(bank_case)
adf.test(bank_case_d1)
##still p value is significant again perform the same differencing function
bank_case_d2<-diff(bank_case_d1)
adf.test(bank_case_d2)
##now p value is significantly low we can say we reject the null hypothesis and now its a stationary process
par(mfrow=c(1,1))
plot(bank_case_d2,main="SOD TIME Series")
#clrly now its stationarity can be observed 
acf(bank_case_d2,lag.max = 10)
pacf(bank_case_d2,lag.max = 10)
#now tht data is stationay we can fit a model onto it 

##arima model 

bank_fit<-arima(x=bank_case,order = c(0,2,1))
#we have chosen the 0 and 1 bcuz we have have seen that lag is cutting of after 1 so it can be at max 1 to 1
#2 is how many times we have differncing the dataset 
bank_fit
##we have to minimize the akai k or aic as much as possible to have a best fit 

fitted(bank_fit)
##we ft the model and plot it 
plot(forecast(bank_fit,h=36))
