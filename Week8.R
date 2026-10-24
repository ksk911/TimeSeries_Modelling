library(urca)
library(vars)

data("Canada")
Canada_ts<-ts(Canada,start = c(1980,1),frequency = 4)
plot(Canada_ts,main="canada Macroeconomic Data",col=1:4)

#adf test is done to check if series is stationary or not
adf_test<-ur.df(Canada[,1],type='drift',selectlags='AIC')
summary(adf_test)
#since p value is small we cannot reject the null and null hypo is that series is non stationary 

##cointegration test
#johansen cointegration test
#H0 : no cointegration 

jo<-ca.jo(Canada_ts,type = "trace",K=2,ecdet="const",spec = 'transitory')
summary(jo)
#r is rank of co integration matrix
#suggests that r>1 but r<=2 which means that one needs a linear combination of 2 series to achieve stationary 

#convert Johnsen test result to vecm representation 

vecm_model<-cajorls(jo,r=1)
summary(vecm_model)
#r is cointegration rank derived from Johansen test results 

#forcasting with ECM
var_model<-vec2var(jo,r=1)
forecast<-predict(var_model,n.ahead = 10)
plot(forecast)

#example on other tests
data("Canada")
y<-Canada[,'prod']#example variable 1 
x <- Canada[,'e']#example variable 2

#1 engle granger test
eg_test<-ca.po(cbind(y,x),type = "Pz")#phillips ouliaris 
summary(eg_test)

#residuals are non stationary. series are not cointegrated 

#granger causality 
library(vars)
data("Canada")
canada_diff<-diff(Canada)
#difference to achieve stationarity 
#choose lag length 
lag_selection <-VARselect(canada_diff,lag.max = 5,type = "const")
p<-lag_selection$selection["AIC(n)"]

#estimate VAR Model
var_model<-VAR(canada_diff,p=p,type = "const")

causality(var_model,cause = "e")
#test if above e causes other variables