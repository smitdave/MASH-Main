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
x = seq(0,400)
hist(PvDur,breaks=25,freq=F,xlab="Days",main="Duration of Pv Infection",ylim=c(0,.02))
#hist(PvDur,breaks=25,freq=F,ylim=c(0,.02))
shift = 14
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
plot(pdf/(1-cdf)-mu,ylim=c(-.1,.14))
xf = seq(0,154)/155
plot(xf,abs(fft(pdf/(1-cdf)-mu)),xlim=c(0,.2))
plot(pdf/(1-cdf)-mu,ylim=c(-.1,.14))
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


split = rep(0,335)
j = 1
for(i in 1:35199){
  if(Vivax$meta_patient_id[i]!=Vivax$meta_patient_id[i+1]){
    split[j] = i
    j = j+1
  }
}
split = c(0,split)
M = max(abs(diff(split)))
VG = matrix(nrow=336,ncol=M)

for(i in 1:335){
  temp = Vivax$gametocytes_f[(split[i]+1):split[i+1]]+Vivax$gametocytes_m[(split[i]+1):split[i+1]]
  pad = rep(NaN,M-length(temp))
  temp = c(temp,pad)
  VG[i,] = temp
}

MVG = log10(rowMeans(t(VG),na.rm=T))
plot(MVP,type="l")
lines(MVG,lty=2)

lag = 7
plot(MVP[(lag+1):400],MVG[1:(400-lag)])

ccf = rep(0,15)
for(i in 1:15){
  
  dh = i-1
  both = which((!is.na(c(rep(0,dh),MVP)))&is.finite(c(rep(0,dh),MVP))&is.finite(c(MVG,rep(NaN,dh)))&(!is.na(c(MVG,rep(NaN,dh)))))
  MVPb = MVP[both]
  MVGb = MVG[both]
  ccf[i] = cor(MVPb,MVGb)
  
}

plot(0:14,ccf,xlab="Delay (in Days)",ylab="correlation")

VVG = log10(rowVars(t(VG)))
plot(MVG,VVG)

MVG[which(is.infinite(MVG))] = NaN
MVP[which(is.infinite(MVP))] = NaN
ccf(MVP, MVG, na.action = na.contiguous)
acf(MVP, na.action=na.contiguous,main="ACF for Mean Asexual P. vivax Densities")
pacf(MVP,na.action=na.contiguous,main="PACF for Mean Asexual P. vivax Densities")


#################### Transmission Efficiency


split = rep(0,335)
j = 1
for(i in 1:35199){
  if(Vivax$meta_patient_id[i]!=Vivax$meta_patient_id[i+1]){
    split[j] = i
    j = j+1
  }
}
split = c(0,split)
M = max(abs(diff(split)))
VTE = matrix(nrow=336,ncol=M)

for(i in 1:335){
  temp = Vivax$mosquitoes[(split[i]+1):split[i+1]]
  pad = rep(NaN,M-length(temp))
  temp = c(temp,pad)
  VTE[i,] = temp
}
VTE[which(VTE==max(Vivax$mosquitoes,na.rm=T))] = NaN
MVTE = rowMeans(t(VTE),na.rm=T)/100
plot(MVG,type="l")
lines(MVTE,lty=2)

plot(MVG,MVTE)

smoothedTE = function(x){
  y = rep(0,length(x))
  for(i in 1:length(x)){
    y[i] = mean(MVTE[which(MVG>=x[i] & MVG < (x[i]+.5))],na.rm=T)
  }
  return(y)
}

z = seq(0,2.5,.1)
y = smoothedTE(z)
plot(z+.25,y,xlab="log10 Gametocyte Density",ylab="Transmission Efficiency",ylim=c(0,1))

sigfit = nls(y~p1*exp(p2*(z+.25))/(p3+exp(p2*(z+.25))),start=list(p1=1,p2=1,p3=.5))
p1=.5003
p2=.8535
p3=.9324
lines(z+.25, p1*exp(p2*(z+.25))/(p3+exp(p2*(z+.25))))

VVTE = rowVars(t(VTE),na.rm=T)
plot(MVTE,VVTE)
