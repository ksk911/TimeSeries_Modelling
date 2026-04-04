library(tseries)
library(forecast)
#simulation of gausssian white noise process

x<-rnorm(1000,0,1)
#in the above line of code we are taking a sample of random 100 values and in that the mean is zero and variance is 1
hist(x)
plot(x,type = 'l')

abline(h=mean(x),col='red')
abline(h=0,col='blue')
#loading dta
bank_case<-as.ts(scan("bank_case.txt"))
bank_case


bank_case_d1<-diff(bank_case)
adf.test(bank_case_d1)
#in the above the p value is larger than apha so we fail to reject the null and accept it as a non stationary process

bank_case_d2<-diff(bank_case_d1)
adf.test(bank_case_d2)
#now above p val is smaller than alpha so we reject null and we go with alternative 

bank_fit<-arima(x=bank_case,order=c(0,2,1))
bank_fit

res<-bank_fit$residuals
res
plot(res,type="l")
abline(h=0)


#normailty check 
hist(res)
shapiro.test(res)
#in shapiro test we get a p vlue grteater than alpha, anytime we get a p vlue bigger than alpha we fail to reject the null 
#above p value is greater so the null is distribution is normal so we can conclude its normally distributed

jarque.bera.test(res)
qqnorm(res)
qqline(res,col='red')

sam<-rchisq(100,4)
qqnorm(sam)
qqline(sam,col='red')

#serial autocorelation check or are residuals independent or dependent 
Box.test(res,lag=10,type = "Box-Pierce")
Box.test(res,lag=10,type = "Ljung-Box")
#in both the above test we have got a p vlaue greater than alpha so we fail to reject the null hence there is no serial autocorelation in the fittd modelin residuals

#hetroscdasticity
acf(res)
pacf(res)
acf(res^2)
pacf(res^2)
#in all the above plots all the residuals are in betweeen the confidence lines so we can say that there is no corelation and are insignificant 

white.test(res)
#in the above test we can conclude that variance isnt changing since p value is greater than alpha we go with null hypothsis that variance is stationary 

checkresiduals(bank_fit)
