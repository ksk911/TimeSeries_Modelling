library(tseries)
load("C:/Users/athar/OneDrive/Desktop/R/timeseries modelling/week2.R")
getwd()
# Read the file (use header=TRUE if your txt file has column names like 'Year' or 'Pop')
uspop
# View the first few rows to make sure it loaded correctly
head(uspop)
plot(uspop,type='o',xlab='year')

#calculatre the linear trend and try to fit a line through it 

nt=length(uspop)
print(nt)
fit=lm(uspop~as.numeric(1:nt))
summary(fit)
#once we run the summary command we can see tht the p value is so small that we can reject the null hypothesis and ## pvalue=(7.29e-11 ***)
#conclude we have relationship between independent variable and population values 


abline(fit)
#below command removes the fitted line and final output doesnt have a trend
y<-uspop-fitted(fit)
ts.plot(y)
