########### run PfPDG_Fitting.R, MT_Fever.R, MT_TE.R first
########### 10 Day Timestep PDG callibration



######################################## Time to Subpatency (Duration of Detectible Infection)



N = rep(0,334)
for(i in 1:334){
  position = which(!is.na(M[,i]))
  nonzero = which(M[,i]>0)
  N[i] = max(intersect(position,nonzero))
}

hist(N/10,xlab="10 Day Timesteps",main="Histogram of Duration of Patent Infection",freq=F)

N = sort(N)
NF = rep(0,36)
for(i in 1:36){
  NF[i] = tail(cumsum(N<(10*i)),1)
}
NF = NF/max(NF)
plot(-log(1-NF[1:20]),xlab="10 Day Increments",ylab="Lambda * Time")

plot(diff(-log(1-NF[1:20])),xlab="10 Day Increments",ylab="Lambda",main="Approximate Derivative of Previous Line")
abline(h=mean(diff(-log(1-NF[1:20]))))

## fitting to -log(1-Surv), including first 20 10 Day Increments which has majority of data
plot(-log(1-NF[1:20]),xlab="10 Day Increments",ylab="Lambda * Time")
y = -log(1-NF)
x = seq(1,26)
lamfit = lm(y[1:20]~x[1:20]+0)
lambdaF = lamfit$coefficients
lines(x,x*lambdaF)

hist(N/10,freq=F,main="Histogram of Surviving Patent Infections",xlab="Fortnights",breaks=15)
n = seq(0,30)
lines(dexp(n,lambdaF))



########################## Parasite Densities, tracked fortnightly with final density around 2 years (52 fortnights)



WP = matrix(0,nrow=36,ncol=334)
MP = rep(0,36)
VP = rep(0,36)

for(j in 1:334){
  for(i in 1:36){
    WP[i,j] = mean(M[((i-1)*10+1):(i*10),j],na.rm=T)
  }
}

for(i in 1:36){
  MP[i] = mean(WP[i,],na.rm=T)
  VP[i] = var(WP[i,],na.rm=T)
}

plot(log10(MP),ylim=c(0,5))

plot(1:36,log10(MP),type="l",xlab="Fortnights Since Infection",ylab="Mean log10 Parasite Density",main="Mean log10 Density Averged over Fortnights",xlim=c(0,30))
plot(log10(MP),log10(VP),xlab="log10 Mean Parasite Density",ylab="log10 Variance Parasite Density",main="Mean-Variance Power Law")
mvfit = lm(log10(VP) ~ log10(MP))
b = mvfit$coefficients[1]
m = mvfit$coefficients[2]
x = seq(-2,5,.1)
lines(x,m*x+b)


#################################### Shape of Dist'n of Asexuals


LWP = log10(WP)
LWP[which(is.infinite(LWP))] = NaN
vioplot(LWP[1,],LWP[2,],LWP[3,],LWP[4,],LWP[5,],LWP[6,],LWP[7,],LWP[8,],LWP[9,],LWP[10,])
vioplot(LWP[11,],LWP[12,],LWP[13,],LWP[14,],LWP[15,],LWP[16,],LWP[17,],LWP[18,],LWP[19,],LWP[20,])

maxp = seq(1,36)
for(i in 1:36){
  maxp[i] = max(LWP[i,],na.rm=T)
}
rLWP = maxp-LWP[1:36,]
hist(rLWP[1,],freq=F,ylim=c(0,1))

shape = rep(0,36)
rate = rep(0,36)
for(i in 1:36){
  temp = rLWP[i,]
  data = temp[which(!is.na(temp))]
  gam = fitdist(data,distr="gamma",method="mme")
  shape[i] = gam$estimate[1]
  rate[i] = gam$estimate[2]
}

plot(shape,type="l")
lines(rate,lty=2)

## this shows the mean and variance of the fitted distributions over time - this is mean and variance of log-transformed data
plot(maxp-shape/rate,xlab="fortnights",ylab="log10 Asexual Parasite Density",ylim=c(0,4),type="l",main="Mean and Variance of Fitted Gammas")
lines(shape/rate^2,lty=2)

var = mean(shape[2:36]/rate[2:36]^2)
abline(h=var)

##################### SMOOTHED Max and Mean of Dist'n of Asexuals ##########################
##################### Note: This fitting provided worse output... ##########################

plot(maxp[2:36],maxp[2:36]-shape[2:36]/rate[2:36],main="Max vs Mean of Asexual Densities")
maxplow = maxp[which(maxp<3)]
maxphigh = maxp[which(maxp>=3)]
meanlow = maxp[which(maxp<3)]-shape[which(maxp<3)]/rate[which(maxp<3)]
meanhigh = maxp[which(maxp>=3)]-shape[which(maxp>=3)]/rate[which(maxp>=3)]

max2meanlow = lm(meanlow[2:length(meanlow)]~maxplow[2:length(meanlow)])
max2meanhigh = lm(meanhigh[2:length(meanlow)]~maxphigh[2:length(meanlow)])
blow = max2meanlow$coefficients[1]
mlow = max2meanlow$coefficients[2]
bhigh = max2meanhigh$coefficients[1]
mhigh = max2meanhigh$coefficients[2]

max2mean = function(maxp){
  meanp = rep(0,length(maxp))
  #meanp = pmax(blow+mlow*maxp,bhigh+mhigh*maxp)
  meanp = bhigh+mhigh*maxp
}
x = seq(0,6,.1)
lines(x,max2mean(x))

plot(maxp,ylim=c(0,5.5),xlab="Fortnights",ylab="Max and Mean of Gamma Distributed Asexual Densities")
maxPt = seq(1,36)
maxPt[1] = maxp[1]
weights = exp(-(2:36)*.138)/exp(-2*.138)
lm(maxp[2:36] ~ seq(2,36),weights = weights)
maxb = 4.87997
maxm = -.08802
maxPt[2:36] = maxb+maxm*seq(2,36)
lines(1:36,maxPt)
points(maxp-shape/rate,pch=10)
lines(c(maxp[1]-shape[1]/rate[1],max2mean(maxPt[2:26])))

abline(h=log10(10),lty=2)
abline(h=log10(50),lty=2)


mupt = c(maxp[1]-shape[1]/rate[1],max2mean(maxPt[2:36]))
plot(1:36,mupt,type="l",main="Mean & Variance Over Duration of Infection")
varpt = shape/rate^2
vars = c(varpt[1],rep(mean(varpt[2:36]),35))
lines(1:36,vars,lty=2)

alpha = (maxPt-mupt)^2/vars
beta = (maxPt-mupt)/vars
plot(1:36,alpha,type="l",ylim=c(0,5),main="Shape and Rate Estimates")
lines(1:36,beta,lty=2)






plot(shape[2:36]/rate[2:36]^2)
hist(shape[2:36]/rate[2:36]^2,breaks=10,freq=F)
fitdist(shape[2:36]/rate[2:36]^2,dist="gamma")
eshape = 4.148
erate = 5.807
s = seq(0,2,.01)
lines(s,dgamma(s,shape=eshape,rate=erate))


plot(maxp-shape/rate,shape/rate^2) ## no strong sign of power law

## this shows the mean and variance of the data - this is log10 of the mean and variance of the data directly (exchange log and expectation)
plot(log10(MP[1:36]),type="l",ylim=c(0,10))
lines(log10(VP[1:36]),lty=2)

plot(log10(MP[1:36]),log10(VP[1:36])) ## clear power law

## this shows fitted distributions to histograms of parasite density for first 6 fortnights
par(mfrow=c(2,3))
for(i in 1:6){
  is = as.character(i)
  title = c("Density at fortnight ", is)
  hist(LWP[i,],freq=F,main=title,xlab="log10 Parasite Density",ylab="Probability Density",xlim=c(-1,5.5),ylim=c(0,.85))
  x = seq(0,5,.1)
  lines(maxp[i]-x,dgamma(x,shape=shape[i],rate=rate[i]))
}
par(mfrow=c(1,1))

## "" "" for fortnights 7-12
par(mfrow=c(2,3))
for(i in 7:12){
  is = as.character(i)
  title = c("Density at fortnight ", is)
  hist(LWP[i,],freq=F,main=title,xlab="log10 Density",ylab="Probability Density",xlim=c(-1,5.5),ylim=c(0,.85))
  x = seq(0,5,.1)
  lines(maxp[i]-x,dgamma(x,shape=shape[i],rate=rate[i]))
}
par(mfrow=c(1,1))

plot(shape/rate^2,type="l")
plot(diff(shape/rate^2,1),type="l")
acf(diff(shape/rate^2,1))

hist(diff(shape/rate^2,1),freq=F,breaks=10)

#gam1 = fitdist(rLWP[1,],distr="gamma",method="mme")
#x=seq(0,5,.1)
#lines(x,dgamma(x,4.64,4.01))

#################################### Fever Signals from Asexuals



FeverFNM = rep(0,36)
FeverFNP = rep(0,36)
FeverFN = matrix(0,nrow=36,ncol=334)
Fever[which(Fever<30)]=NaN
for(j in 1:334){
  for(i in 1:36){
    FeverFN[i,j] = mean(Fever[((i-1)*10+1):(i*10),j],na.rm=T)
  }
}

FeverFNBin = FeverFN
FeverFNBin[which(!is.na(FeverFNBin))]=1
FeverFNBin[which(is.na(FeverFNBin))]=0
for(i in 1:36){
  FeverFNM[i] = mean(FeverFN[i,],na.rm=T)
  FeverFNP[i] = mean(FeverFNBin[i,],na.rm=T)
}

LMP = log10(MP)
plot(LMP,FeverFNP,xlab="log10 Asexual Parasite Densities",ylab="Proportion with Fever",main="Fever Probability for Given Parasite Density",xlim=c(0,5),ylim=c(0,1))
fevfit = nls(FeverFNP~p1*exp(p2*(LMP))/(exp(p2*p3)+exp(p2*LMP)),start=list(p1=.9,p2=3,p3=3))
p1 = .911
p2 = 2.615
p3 = 3.696
x = seq(-2,5,.1)
lines(x,p1*exp(p2*x)/(exp(p2*p3)+exp(p2*x)))


plot(log10(MP),FeverFNM,xlab="log10 Asexual Parasite Densities",ylab="Body Temperature (Degrees Celsius)",main="Degree of Fever for Given Parasite Density")
hist(FeverFNM)
## compare to full dataset, is normally distributed:
hist(5/9*(MFeverF-32),freq=F,xlab="Temperature (Degrees Celsius)",main="Temperature Given Fever")
qqnorm(5/9*(MFeverF-32))
## for a given parasite density, calculate probability of fever; if fever present, draw from normal dist'n



#################################### Gametocytes



WG = matrix(0,nrow=36,ncol=334)
MG = rep(0,36)
VG = rep(0,36)

WTE = matrix(0,nrow=36,ncol=334)
MTE = rep(0,36)
VTE = rep(0,36)

for(j in 1:334){
  for(i in 1:36){
    WG[i,j] = mean(MGt[((i-1)*10+1):(i*10),j],na.rm=T)
  }
}

for(i in 1:36){
  MG[i] = mean(WG[i,],na.rm=T)
  VG[i] = var(WG[i,],na.rm=T)
  MTE[i] = mean(WTE[i,],na.rm=T)
  VTE[i] = var(WTE[i,],na.rm=T)
}

plot(log10(MG),log10(VG),xlab="log10 Mean Gametocyte Density",ylab="log10 Variance of Gametocyte Density",main="Gametocyte Mean-Variance Power Law")
lmG = lm(log10(VG[1:36])~log10(MG[1:36]))
b = lmG$coefficients[1]
m = lmG$coefficients[2]
x = seq(-3,3,.1)
lines(x,m*x+b)

plot(log10(MG),xlab="Fortnights Since Infection",ylab="log10 Parasite Density",xlim=c(0,30),type="l",ylim=c(-3,5),col="red",main="Fortnightly Average Parasite Densities")
lines(log10(MP))
## asexual-gametocyte relationship
plot(log10(MP[4:30]),log10(MG[3:29]))
plot(log10(MP),log10(MG),xlab="log10 Asexual Parasite Density",ylab="log10 Gametocyte Density",main="Mean Asexual and Gametocyte Densities Power Law")
ccf(MP[4:26],MG[4:26],lag.max=5)
ccf(log10(MP[4:28]),log10(MG[4:28]),lag.max=5)
ccf(MP,MG,lag.max=5)
ccf(log10(MP[1:26]),log10(MG[1:26]),lag.max=5)
## note here the ccf shows a lag-dependence of one 10 day increment for original data, but not log-transformed data...
## this is due to the first couple fortnights of infection where there does seem to be a lag and parasite densities
## are very high, so they dominate the correlation calculation for the non-transformed data. This is confirmed
## when omitting the first couple fortnights of data and any lag dependence is lost; 
## however, this may still suggest that there is a lagged dependence
## but the lag itself follows a distribution and causes the lag dependence to become diffuse.

lmPG = lm(log10(MG[2:26])~log10(MP[2:26]))
b = -2.224
m = 1.318
plot(log10(MP),log10(MG))
x = seq(-1,5,.1)
lines(x,m*x+b)

## outlier in first fortnight - ignore****

P2GVar = var(lmPG$residuals)
#################################### TE Signals from Gametocytes



WTE = matrix(0,nrow=36,ncol=334)
MTE = rep(0,36)
VTE = rep(0,36)

for(j in 1:334){
  for(i in 1:36){
    WTE[i,j] = mean(TE[((i-1)*10+1):(i*10),j],na.rm=T)
  }
}

WTE = WTE/100

for(i in 1:36){
  MTE[i] = mean(WTE[i,],na.rm=T)
  VTE[i] = var(WTE[i,],na.rm=T)
}

LMG = log10(MG)
plot(LMG,MTE,xlim=c(-2,4),ylim=c(0,1),xlab="log10 Gametocyte Density",ylab="Transmission Efficiency",main="Transmission Efficiency for Given Gametocyte Density")
TEfit = nls(MTE~p1*exp(p2*(LMG))/(exp(p2*p3)+exp(p2*LMG)),start=list(p1=.9,p2=3,p3=3))
TEmax = .373
TEslope = 6.18
TEhalf = 2.209
x = seq(-2,4,.1)
lines(x,TEmax*exp(TEslope*x)/(exp(TEslope*TEhalf)+exp(TEslope*x)))

