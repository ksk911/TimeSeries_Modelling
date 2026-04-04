install.packages("vars")
install.packages("MTS")

library(vars)
library(MTS)
#simulate the data
set.seed(123)
n<-200
e1=rnorm(n)
e2=rnorm(n)
y1=filter(e1,filter = c(0.5,-0.3),method = "recursive")
y2=filter(e2,filter = c(0.4,0.2),method = "recursive")
#The filter() function and the c() coefficientsThe filter function with method = "recursive" transforms 
#that random noise into a time series with a specific structure.The job of c(0.5, -0.3) and c(0.4, 0.2):
#These are the AR coefficients. They determine how much "weight" or "influence" the past values have on 
#the current value.The first value (e.g., 0.5): This is the weight of the value from 1 step ago ($y_{t-1}$).
#The second value (e.g., -0.3): This is the weight of the value from 2 steps ago ($y_{t-2}$).

#combine into matrix
sim_data<-cbind(y1,y2)
#cbind converts them into matrix sort of structure

colnames(sim_data)<-c("Series1","Series2")

#convert to ts
sim_data_ts<-ts(sim_data)
plot.ts(sim_data_ts)

#var model 
var_model<-VAR(sim_data_ts,p=2)#p=lag order 
summary(var_model)
var_forecast<-VARpred(var_model,h=30)#h=time steps ahead so iin this case 30 steps ahead 
print(var_forecast)
par(mar=c(1,1,1,1))
plot(1:200,sim_data_ts[,-1],type='l',xlim=c(0,235))
lines(201:230,var_forecast$pred[,-1],col='red')
#VMA model 
vma_model<-VMA(sim_data_ts,q=2)
summary(vma_model)
#varma model 
varma_model<-VARMA(sim_data_ts,p=1,q=1)
#p is ar order and q is ma
summary(varma_model)

varma_forecast<-VARMApred(varma_model,h=30)#30 steps ahead
print(varma_forecast$pred)
plot(1:200,sim_data_ts[,-1],type='l',xlim=c(0,235))
lines(201:230,varma_forecast$pred[,-1],col='red')

#diagnostics for VAR and VARMA
MTSdiag(var_model,gof = 24,adj=0,level=F)
#in all the residual plots everything is below the bands hence residuals act as stationary which is what we want
#in the p value plt all the values are above the threshold so we fail to reject the null hypo hence underlying residuals are stationary 
