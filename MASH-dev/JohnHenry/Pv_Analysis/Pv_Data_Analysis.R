library(readxl)
library(fitdistrplus)
library(ggplot2)

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
hist(PvDur,breaks=25,freq=F,xlab="Days Since First Patency",main="Histogram of the Duration of Pv Infection")

gamma_fit = fitdist(PvDur,distr="gamma", method="mle")
shape = 1.924
rate = .0286
x = seq(0,400,.1)
lines(x,dgamma(x,shape=shape,rate=rate),lty=2)

exp_fit = fitdist(PvDur,distr="exp",method="mle")
lines(x,dexp(x,.01485),lty=2)

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
### no formal justification for the shift, just eyeball it so far
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

x = seq(0,400)
hist(PvDur,breaks=25,freq=F,xlab="Days",main="Duration of Pv Infection",ylim=c(0,.02))
shift = 14
lines(x,.015*(sin(pi*(x-shift)/45)+1)*exp(-.015*(45/pi*cos(pi*(x-shift)/45)+x)),type="l")

################### Asexual Parasitemia ##########################


MVP = log10(colMeans(VP,na.rm=T))
plot(MVP,type="l",xlab="Days Since First Pantency",ylab="log10 Mean Pv Density per cmm",main="Daily Mean Pv Density Conditioned on Patent Infection",xlim=c(0,300),ylim=c(0,5))
abline(v=c(0,80,160,240,320))
abline(v=c(10,90,170,250,330),lty=2)


rowVars = function (x,na.rm = TRUE) {
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}
VVP = log10(rowVars(t(VP)))

lm(VVP[is.finite(VVP)]~MVP[is.finite(VVP)])
m = 1.992
b = .45

plot(MVP,VVP,xlab="Mean Pv Parasite Density",ylab="Variance of Pv Parasite Density",main="Pv Parasite Density Mean-Variance Power Law")
lines(seq(0,5,.1),seq(0,5,.1)*m+b)

plot(log10(Vivax$asexual[1:PvDur[3]]),type="l",xlab="Days Since Innoculation",ylab="log10 Parasite Density per cmm",main="Example Individual Infection")
abline(v=c(32:34),lty=2)
abline(v=c(90),lty=2)

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
plot(MVP,type="l",xlab="Days Since First Patency",ylab="log10 Parasite Density per cmm",main="Average Asexual and Gametocyte Densities Conditioned on Infection",xlim=c(0,300),ylim=c(0,5))
lines(MVG,col="red")

#lag = 7
#plot(MVP[(lag+1):400],MVG[1:(400-lag)])

ccf = rep(0,15)
for(i in 1:15){
  
  dh = i-1
  both = which((!is.na(c(rep(0,dh),MVP)))&is.finite(c(rep(0,dh),MVP))&is.finite(c(MVG,rep(NaN,dh)))&(!is.na(c(MVG,rep(NaN,dh)))))
  MVPb = MVP[both]
  MVGb = MVG[both]
  ccf[i] = cor(MVPb,MVGb)
  
}

plot(0:14,ccf,xlab="Delay (in Days)",ylab="correlation",type="l")


VVG = log10(rowVars(t(VG)))
plot(MVG,VVG,xlab="log10 Mean P. vivax Gametocytemia",ylab="log10 Variance P. vivax Gametocytemia",main="Mean-Variance Powerlaw for P. vivax Gametocytes")

MVG[which(is.infinite(MVG))] = NaN
MVP[which(is.infinite(MVP))] = NaN
ccf(MVP[1:80], MVG[1:80], na.action = na.contiguous,main="CCF for Asexuals to Gametocytes for P vivax")
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

for(i in 1:336){
  temp = Vivax$mosquitoes[(split[i]+1):split[i+1]]
  pad = rep(NaN,M-length(temp))
  temp = c(temp,pad)
  VTE[i,] = temp
}
VTE[which(VTE==max(Vivax$mosquitoes,na.rm=T))] = NaN
MVTE = rowMeans(t(VTE),na.rm=T)/100
plot(MVG/max(MVG,na.rm=T),type="l",ylim=c(0,1))
lines(MVTE,lty=2,col="green")

plot(MVG,MVTE,xlab="log10 Mean Gametocyte Density",ylab="Proportion of Infected Mosquitoes",main="Transmission Efficiency as a Function of Gametocyte Density")

smoothedTE = function(x){
  y = rep(0,length(x))
  for(i in 1:length(x)){
    y[i] = mean(MVTE[which(MVG>=x[i] & MVG < (x[i]+.5))],na.rm=T)
  }
  return(y)
}

z = seq(0,2.5,.1)
y = smoothedTE(z)
plot(z+.25,y,xlab="log10 Mean Gametocyte Density",ylab="Fraction of Bloodfed Mosquitoes Infected",ylim=c(0,1),main="Mean Transmission Efficiency for Given Gametocytemia")

sigfit = nls(y~p1*exp(p2*(z+.25))/(p3+exp(p2*(z+.25))),start=list(p1=1,p2=1,p3=.5))
p1=.5003
p2=.8535
p3=.9324
lines(z+.25, p1*exp(p2*(z+.25))/(p3+exp(p2*(z+.25))))
abline(h=p1,lty=2)

VVTE = rowVars(t(VTE),na.rm=T)
#plot(MVTE,VVTE)


############## Fever from Asexual Parasitemia


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
VFever = matrix(nrow=336,ncol=M)

for(i in 1:335){
  temp = Vivax$fever[(split[i]+1):split[i+1]]
  ### remove unreasonably high temps (>115 F, current world record)
  temp[which(temp>115)] = NaN
  ### remove one measurement of fever below 90 F
  temp[which(temp<90)] = NaN
  pad = rep(NaN,M-length(temp))
  temp = c(temp,pad)
  VFever[i,] = temp
}

MVFever = rowMeans(t(VFever),na.rm=T)
plot(MVFever,type="l",xlab="Days",ylab="Mean Temperature Given Fever",main="Average Fever by Day")

#plot(MVP,MVFever)

feverData = which(!is.na(MVP) & !is.na(MVFever))
MVPComp = MVP[feverData]
MVFeverComp = MVFever[feverData]

plot(MVPComp,MVFeverComp,xlab="Mean P. vivax Asexual Density",ylab="Mean Recorded Temperature",main="Fever Risk for a Given Parasite Density")
feverfit = lm(MVFeverComp ~ MVPComp)
x = seq(1.5,4.5,.1)
m = feverfit$coefficients[2]
b = feverfit$coefficients[1]
lines(x,m*x+b)

smoothedFever = function(x){
  y = rep(0,length(x))
  for(i in 1:length(x)){
    y[i] = mean(MVFever[which(MVPComp>=x[i] & MVPComp < (x[i]+.2))],na.rm=T)
  }
  return(y)
}

## granger causality
x = seq(.9,3.6,.1)+.1
y = (smoothedFever(x)-min(smoothedFever(x),na.rm=T))/(max(smoothedFever(x),na.rm=T)-min(smoothedFever(x),na.rm=T))
plot(x,y,xlab="log10 Asexual Parasite Density",ylab="Average Fever",main="Averaged Fever Given Parasitemia",ylim=c(0,1))
sigfit = nls(y~p1*exp(p2*(x))/(p3+exp(p2*(x))),start=list(p1=1.1,p2=.8,p3=2))
p1=1.116
p2=2.065
p3=308.913
SigmaFever = p1*exp(p2*(x))/(p3+exp(p2*(x)))*(max(smoothedFever(x),na.rm=T)-min(smoothedFever(x),na.rm=T))+min(smoothedFever(x),na.rm=T)

plot(x,smoothedFever(x))
lines(x, SigmaFever)

plot(MVPComp,MVFeverComp)
lines(x,SigmaFever)

FeverDays = 0*VFever
for(i in 1:335){
  FeverDays[i,] = as.numeric(!is.na(VFever[i,]))
}

PFever = rowMeans(t(FeverDays),na.rm=T)
plot(MVP/max(MVP,na.rm=T),type="l",ylim=c(0,1))
PFeverPrime = PFever/pDet
plot(PFeverPrime,type="l",xlim=c(1,200))
lines(PFever[1:200])

lines(PFever,lty=2)

plot(MVP,log10(PFever/(1-PFever)),xlab="log10 Parasite Density",ylab="log10 Odds Ratio of Fever",xlim=c(1.5,4.5))

plot(MVP[8:80],PFeverPrime[8:80],xlab="log10 Parasite Density",ylab="Probability of Fever")

xxpd = MVP[8:80]
yyfev = PFeverPrime[8:80]
#xxpd = xxpd[which(yyfev>0)]
#yyfev = yyfev[which(yyfev>0)]

fitmodel <- nls(yyfev~a/(1 + exp(-b * (xxpd-c))), start=list(a=.6,b=1.5,c=3.5))
xx = seq(0,4.5,.01)
a = .7488
b = 3.9322
c = 3.4620
plot(MVP[8:80],PFeverPrime[8:80],xlab="log10 Parasite Density",ylab="Probability of Fever",xlim=c(2,4.5),main="Probability of Fever Given log10 P. vivax Density",ylim=c(0,1))
lines(xx,a/(1+exp(-b*(xx-c))))

plot(PFeverPrime[1:200],type="l",xlab="Days Since Inoculation",ylab="Fraction Presenting with Fever",main="Fever Prevalence Given Active Infection")
lines(a/(1+exp(-b*(MVP[1:200]-c))),lty=2,col="red")
legend(100,.65,legend=c("Measured Fraction","Predicted from Pv Density"),col=c("black","red"),lty=c(1,2))


plot(MVP[8:80],log10(PFever[8:80]/(1-PFever[8:80])),xlim=c(2.5,4.1))

##### Daily fraction with patent parasitemia


pDet = rep(0,300)
VP[which(is.na(VP))] = 0
pDet = rowMeans(t(VP>0))
plot(pDet*336,type="l",xlim=c(0,300),xlab="Days Since First Patency",ylab="Number of Patients with Patent Infections",main="Daily Fraction of Patent Infections")
abline(v=80,lty=2)
abline(h=.5,lty=2)

plot(MVP[1:80],type="l",xlab="Days Since First Patency",ylab="log10 Mean Parasitemia per cmm",main="Average Parasitemia Conditioned on Infection",ylim=c(0,5),xlim=c(0,200))
xx = 10:50
yy = MVP[10:50]
lmpd = lm(yy~xx)
lines(seq(0,200),seq(0,200)*lmpd$coefficients[2]+lmpd$coefficients[1])
abline(h=log10(88),lty=2)
abline(v=103,lty=2)

lines(pDet)
############# Aggregate by week/fortnight to get violin plots for first several months? 

VivaxPD = list()
for(i in 1:335){
  VivaxPD[[i]] = Vivax$asexual[(split[i]+1):split[i+1]]
}

PvPD = matrix(0,nrow=80,ncol=336)
for(i in 1:80){
  for(j in 1:335){
    PvPD[i,j] = VivaxPD[[j]][i]
  }
}

PvPD[which(PvPD==0)] = NaN

MPViol = rowMeans(PvPD,na.rm=T)
plot(log10(MPViol)[1:80],type="l")

vioplot(log10(PvPD[1:14]),log10(MPViol[15:28]),log10(MPViol[29:42]),log10(MPViol[43:55]))

