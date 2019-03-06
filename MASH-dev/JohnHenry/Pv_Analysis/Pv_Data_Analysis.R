library(readxl)
library(fitdistrplus)

Vivax = read.csv("~/vivax_processed_2017-02-14.csv")

## 335 patients
## 35200 person-days of observations

split = rep(0,335)
j = 1
for(i in 1:35199){
  if(Vivax$meta_patient_id[i]!=Vivax$meta_patient_id[i+1]){
    split[j] = i
    j = j+1
  }
}


##################### Duration of Infection ##################



## determines the longest length of time a patient was observed
M = max(abs(diff(split)))

## matrix which will contain asexual parasite counts
VP = matrix(nrow=336,ncol=M)

split = c(0,split)
for(i in 1:335){
  temp = Vivax$asexual[(split[i]+1):split[i+1]]
  pad = rep(NaN,M-length(temp))
  temp = c(temp,pad)
  VP[i,] = temp
}

PvDur = rep(NaN,336)
for(i in 1:336){
  PvDur[i] = max(which(!is.na(VP[i,])))
  PvDur[which(is.infinite(PvDur))] = 400
}

## histogram of duration of infection; appears to have multiple modes
hist(PvDur,breaks=25,freq=F)


gamma_fit = fitdist(PvDur,distr="gamma", method="mle")
shape = 1.924
rate = .0286
x = seq(0,400,.1)
lines(x,dgamma(x,shape=shape,rate=rate))

exp_fit = fitdist(PvDur,distr="exp",method="mle")
lines(x,dexp(x,.01485))

## this should be a straight line if exponentially distributed with slope = 1/mean
## not enough infections end very early on
#plot(-log(1-cumsum(rev(sort(PvDur)))/sum(PvDur)))

hist(PvDur,breaks=25,freq=F,xlab="Days",main="Duration of Pv Infection",ylim=c(0,.02))
#hist(PvDur,breaks=25,freq=F,ylim=c(0,.02))
shift = 10
## maybe try sinusoidally varying intensity with period of ~90 days, shifted slightly
lines(x,.015*(sin(pi*(x-shift)/45)+1)*exp(-.015*(45/pi*cos(pi*(x-shift)/45)+x)),type="l")

Durcdf = function(x){
  y = 0*x
  for(i in 1:length(x)){
    y[i] = sum(PvDur<=x[i])/336
  }
  return(y)
}

### justification for using 90 days as a frequency
### mu is the mean, .015 (used above)
### no justification for the shift, just eyeball it so far
x = seq(0,155)
plot(x,Durcdf(x),type="l")
pdf = diff(Durcdf(x))
plot(seq(1,155),pdf,type="l")
cdf = Durcdf(1:155)
mu = mean(pdf/(1-cdf))
plot(pdf/(1-cdf)-mu,ylim=c(-.1,.14),type="l")
xf = seq(0,154)/155
plot(xf,abs(fft(pdf/(1-cdf)-mu)),xlim=c(0,.2))
plot(pdf/(1-cdf)-mu,ylim=c(-.1,.14),type="l")
lines(x,.015*sin(pi*(x-20)/45))

################### Asexual Parasitemia ##########################


MVP = log10(rowMeans(t(VP),na.rm=T))
plot(MVP,type="l",,xlab="Days",ylab="Mean Pv Density",main="Daily Mean Pv Density Conditioned on Persistent Infection")

rowVars = function (x,na.rm = TRUE) {
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}
VVP = log10(rowVars(t(VP)))

lm(VVP[is.finite(VVP)]~MVP[is.finite(VVP)])
m = 1.993
b = .454

plot(MVP,VVP,xlab="Mean Pv Parasite Density",ylab="Variance of Pv Parasite Density",main="Pv Parasite Density Mean-Variance Power Law")
lines(seq(0,5,.1),seq(0,5,.1)*m+b)



######################## Gametocytemia #############################

