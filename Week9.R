#spectral analysis 

T<- 20 #len of time series
f1<- 0.1 #frq of first sinusoidal component
f2<- 0.3 #freq of second sinusoidal compoent 

#time points 
t <- 0 :(T-1)
#generate a signal 
signal<- sin(2*pi*f1*t)+0.5*sin(2*pi*f2*t)
#add some random noise to dataset in order to create some realness
set.seed(42)
noise<-0.2*rnorm(T)
time_series<-signal+noise

#plt time series 
plot(t,time_series,type = "o",col='red',xlab = "time",ylab = "value")

#compute the periodogram 
#using the fast fourier transform to compute the periodogram 
fft_values<-fft(time_series)
#fix some freqncies of interest (up to Nyquist Freq)
frequencies<- (0:(T/2))/T#takes only half the freq length a fixed formula 
#power is actual peridogram values 
power<-Mod(fft_values[1:(T/2+1)])^2/T #this is exact formula to get power 
plot(frequencies,power,type="h",col="blue",xlab="freq(Herz)",ylab="power")
#periodogram is nothing but estimate of actual spectral density so whatever freq that we fixed earlier have to be replicated 
#highlight the dominant frq
abline(v=f1,col="pink",lty=2)
abline(v=f2,col="red",lty=2)

#smoothed periodgram (optional) to reduce noise 
library(stats)
smoothed_power<-filter(power,rep(1/3,3),sides=2)
#plot the smooth periodogram 
plot(frequencies,smoothed_power,type = 'l',xlab = "Freq Hz",ylab = "Smoothed Power")

#Welchs method (segmented periodogram)
#divide periodogram into overlapping segments and average periodogram
install.packages("bspec")
library(bspec)
psd_welch<-welchPSD(ts(time_series),seglength = 10)

#extract freq and power from welchs method
welch_freq<-psd_welch$freq
welch_power<-psd_welch$power

plot(welch_freq,welch_power,type = 'l',xlab = "freq Hz",ylab = 'Power')

# ex 2 is we genrate ts with sinusoidal comp and exponential decay 

T<-100
f1<-0.15
decay_rate<-0.05

t<-0:(T-1)

#generate signal with decay and sinosidal pattrm
set.seed(123)#for reproucibility 
noise<-0.3*rnorm(T)
time_series<-signal+noise
plot(t,time_series,type="o",xlab='Time',ylab="Value")

#Compute Periodogram 

#use the fft to compute the periodogram 
fft_values<-fft(time_series)
frequencies<-(0:(T/2))/T
power<-Mod(fft_values[1:(T/2+1)])^2/T
plot(frequencies,power,type="h",col="blue",xlab="freq(Herz)",ylab="power")


smoothed_power<-stats::filter(power,rep(1/5,5),sides=2)
plot(frequencies,smoothed_power,type = 'l',xlab='Hz',ylab = 'smoothed Power')

#spectral density estimation using spectrum 
#spectrum function in R performs spectral density estimation 
density_estimation<-spectrum(time_series,col='red',spans=c(3,3))

