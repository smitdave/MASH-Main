library(readxl)
library(fitdistrplus)
library(zoib)
library(gamlss)
Mosquito_Transmission <- read_excel("~/Malaria Data Files/Mosquito_Transmission.xlsx")
MT_Days <- read_excel("~/Malaria Data Files/MT_Days.xlsx")
Gt_Col <- read_excel("~/Malaria Data Files/Gt_Col.xlsx")

#MT_GT_NP <- read_excel("~/Malaria Data Files/MT_GT_NP.xlsx")
#G = as.matrix(MT_GT_NP)

Days = as.matrix(MT_Days)
## which rows denote beginning of individual
begin = which(Days==1)

## which rows denote end of individual
end = which(Days==1)-1
end = c(end,length(Days))
end = end[2:length(end)]

Mosq = as.matrix(Mosquito_Transmission)
Mosq[which(Mosq==".")]=NaN
Mosq = as.numeric(Mosq)

Gt = as.matrix(Gt_Col)
Gt[which(Gt==".")]=NaN
Gt = as.numeric(Gt)

Pt = as.matrix(MT_PT_NP)

TE = matrix(NaN,nrow=1400,ncol=334)
MGt = TE
for(i in 1:334){
  TE[1:(end[i]-begin[i]+1),i] = Mosq[begin[i]:end[i]]
  MGt[1:(end[i]-begin[i]+1),i] = Gt[begin[i]:end[i]]
}


### compare average gametocyte levels to average proportion of infected mosquitoes
TEmu = rowMeans(TE,na.rm=T)
GTmu = rowMeans(G,na.rm=T)
PTmu = rowMeans(Pt,na.rm=T)

plot(TEmu,type="l",xlim=c(0,200))

rowVar = function(m){
  n = dim(m)[1]
  m[which(is.na(m))]=0
  temp = rowSums((m-rowMeans(m,na.rm=T))^2)/(n-1)
  return(temp)
}

TEvar = rowVar(TE)
GTvar = rowVar(MGt)
PTvar = rowVar(Pt)

plot(TEmu/100,type="l",ylim=c(0,5),xlim=c(0,365),xlab="days",ylab="log10 Parasite Density per microliter")
#lines(sqrt(TEvar/10^4)+TEmu/100,lty=2)
#lines(pmax(-sqrt(TEvar/10^4)+TEmu/100,0),lty=2)
lines(log10(GTmu),lty=2,col="red")
lines(log10(PTmu))
abline(h=log10(10))
title(main="Transmission Efficiency in Mean Infection Profile")
###
ccf(PTmu,GTmu,type="correlation",lag.max=20)
TEmu[which(is.na(TEmu))]=0

Mprime = Mosq[which(is.na(Mosq)==F)]
Gtprime = as.numeric(Gt[which(is.na(Mosq)==F)])
##log10 gametocyte count vs % of mosquitoes infected from bitting from them
plot(log10(Gtprime),Mprime/100,ylab="Proportion of Feeding Mosquitoes Infected",xlab="log10 Gametocyte Density per cmm")
title(main="Transmission Efficiency as a Function of Gametocyte Density")

beta1 = Mprime[which(log10(Gtprime) <= 1)]/100
logGT1 = pmax(log10(Gtprime)[which(log10(Gtprime) <= 1)],0)
logGT1mu = log10(mean(10^logGT1[which(logGT1>0)]))
beta2 = Mprime[which(log10(Gtprime) > 1 & log10(Gtprime) <= 1.5)]/100
logGT2 = pmax(log10(Gtprime)[which(log10(Gtprime) > 1 & log10(Gtprime) <= 1.5)],0)
logGT2mu = log10(mean(10^logGT2[which(logGT2>0)]))
beta3 = Mprime[which(log10(Gtprime) > 1.5 & log10(Gtprime) <= 2)]/100
logGT3 = pmax(log10(Gtprime)[which(log10(Gtprime) > 1.5 & log10(Gtprime) <= 2)],0)
logGT3mu = log10(mean(10^logGT3[which(logGT3>0)]))
beta4 = Mprime[which(log10(Gtprime) > 2 & log10(Gtprime) <= 2.5)]/100
logGT4 = pmax(log10(Gtprime)[which(log10(Gtprime) > 2 & log10(Gtprime) <= 2.5)],0)
logGT4mu = log10(mean(10^logGT4[which(logGT4>0)]))
beta5 = Mprime[which(log10(Gtprime) > 2.5 & log10(Gtprime) <= 3)]/100
logGT5 = pmax(log10(Gtprime)[which(log10(Gtprime) > 2.5 & log10(Gtprime) <= 3)],0)
logGT5mu = log10(mean(10^logGT5[which(logGT5>0)]))
beta6 = Mprime[which(log10(Gtprime) > 3 & log10(Gtprime) <= 3.5)]/100
logGT6 = pmax(log10(Gtprime)[which(log10(Gtprime) > 3 & log10(Gtprime) <= 3.5)],0)
logGT6mu = log10(mean(10^logGT6[which(logGT6>0)]))
beta7 = Mprime[which(log10(Gtprime) > 3.5)]/100
logGT7 = pmax(log10(Gtprime)[which(log10(Gtprime) > 3.5)],0)
logGT7mu = log10(mean(10^logGT7[which(logGT7>0)]))

betaRange = function(xmin){
  Mprime[which(log10(Gtprime) > xmin & log10(Gtprime) <= xmin+1)]/100
}

x = seq(0,1,.01)

beta1fit = fitdist(beta1,distr="beta",method="mge")
a1 = beta1fit$estimate[1]
b1 = beta1fit$estimate[2]
hist(beta1,breaks=50,freq=F)
lines(x,dbeta(x,a1,b1))
beta1var = a1*b1/((a1+b1)^2*(a1+b1+1))

beta2fit = fitdist(beta2,distr="beta",method="mge")
a2 = beta2fit$estimate[1]
b2 = beta2fit$estimate[2]
hist(beta2,breaks=50,freq=F)
lines(x,dbeta(x,a2,b2))
beta2var = a2*b2/((a2+b2)^2*(a2+b2+1))

beta3fit = fitdist(beta3,distr="beta",method="mge")
a3 = beta3fit$estimate[1]
b3 = beta3fit$estimate[2]
hist(beta3,breaks=50,freq=F)
lines(x,dbeta(x,a3,b3))
beta3var = a3*b3/((a3+b3)^2*(a3+b3+1))

beta4fit = fitdist(beta4,distr="beta",method="mge")
a4 = beta4fit$estimate[1]
b4 = beta4fit$estimate[2]
hist(beta4,breaks=50,freq=F)
lines(x,dbeta(x,a4,b4))
beta4var = a4*b4/((a4+b4)^2*(a4+b4+1))

beta5fit = fitdist(beta5,distr="beta",method="mge")
a5 = beta5fit$estimate[1]
b5 = beta5fit$estimate[2]
hist(beta5,breaks=50,freq=F)
lines(x,dbeta(x,a5,b5))
beta5var = a5*b5/((a5+b5)^2*(a5+b5+1))

beta6fit = fitdist(beta6,distr="beta",method="mge")
a6 = beta6fit$estimate[1]
b6 = beta6fit$estimate[2]
hist(beta6,breaks=50,freq=F)
lines(x,dbeta(x,a6,b6))
beta6var = a6*b6/((a6+b6)^2*(a6+b6+1))

beta7fit = fitdist(beta7,distr="beta",method="mge")
a7 = beta7fit$estimate[1]
b7 = beta7fit$estimate[2]
hist(beta7,breaks=50,freq=F)
lines(x,dbeta(x,a7,b7))
beta7var = a7*b7/((a7+b7)^2*(a7+b7+1))

betaMeans = c(a1/(a1+b1),a2/(a2+b2),a3/(a3+b3),a4/(a4+b4),a5/(a5+b5),a6/(a6+b6),a7/(a7+b7))
betaVars = c(beta1var,beta2var,beta3var,beta4var,beta5var,beta6var,beta7var)
logGT = c(logGT1mu,logGT2mu,logGT3mu,logGT4mu,logGT5mu,logGT6mu,logGT7mu)
plot(logGT,betaMeans,ylim=c(0,1),xlim=c(.5,4.25),xlab="Log10 Gametocyte Density per Microliter",ylab="Proportion of Mosquitoes Infected",main="Transmission Efficiency")
sigfit = nls(betaMeans~p1*exp(p2*(logGT))/(p3+exp(p2*(logGT))),start=list(p1=.6,p2=.5,p3=2))
p1=.689
p2= 2.145
p3 = 144.351

#gompfit = nls(betaMeans~g1*exp(-g2*exp(-g3*logGT)),start=list(g1=1-.7,g2=.5,g3=.1))
#g1 = .7725
#g2 = 12.1837
#g3 = 1.1818

#gompTE = function(x,g1,g2,g3){
#  g1*exp(-g2*exp(-g3*x))
#}

sigmoidTE = function(x,p1,p2,p3){
  p1*exp(p2*x)/(p3+exp(p2*x))
}
x = seq(.8,4.1,.01)
plot(x,sigmoidTE(x,p1,p2,p3))
title(main="Mean log10 Gametocyte Density vs Mean Transmission Efficiency")
#lines(x,gompTE(x,g1,g2,g3),lty=2)


midpoint = log(p3)/p2
saturation = p1
slope = p2

#par(mfrow=c(1,2))
x = seq(-1,8,.01)
plot(x,sigmoidTE(x,p1,p2,p3),type="l",ylim=c(0,1),xlab="log10 Gametocyte Density per Microliter")
abline(v = midpoint)
abline(h=saturation)
abline(h=0)
#plot(x,gompTE(x,g1,g2,g3),type="l",ylim=c(0,1),xlab="log10 Gametocyte Density (PRBC per cmm)")
#abline(h=.7725)
#abline(h=0)
#gomphalf = x[min(which(gompTE(x,g1,g2,g3)>(.7725/2)))]
#abline(v = gomphalf)
#par(mfrow=c(1,1))




plot(logGT,betaVars,type="l")
plot(logGT,betaMeans/betaVars,type="l")
plot(logGT,betaMeans/sqrt(betaVars),type="l")



############################ zero inflation fit

pz1 = sum(beta1==0)/length(beta1)
pz2 = sum(beta2==0)/length(beta2)
pz3 = sum(beta3==0)/length(beta3)
pz4 = sum(beta4==0)/length(beta4)
pz5 = sum(beta5==0)/length(beta5)
pz6 = sum(beta6==0)/length(beta6)
pz7 = sum(beta7==0)/length(beta7)
pz = c(pz1,pz2,pz3,pz4,pz5,pz6,pz7)

po1 = sum(beta1==1)/length(beta1)
po2 = sum(beta2==1)/length(beta2)
po3 = sum(beta3==1)/length(beta3)
po4 = sum(beta4==1)/length(beta4)
po5 = sum(beta5==1)/length(beta5)
po6 = sum(beta6==1)/length(beta6)
po7 = sum(beta7==1)/length(beta7)
po = c(po1,po2,po3,po4,po5,po6,po7)

s = 1:7
plot(s,pz,type="l",ylim=c(0,1))
lines(s,po)


#################### sliding window for measuring TE from Gt ####################


betazfit = function(a){
  ## a should be between .5 and 4
  betaz = Mprime[which(log10(Gtprime) > a & log10(Gtprime) <= a+.5)]/100
  logGTz = pmax(log10(Gtprime)[which(log10(Gtprime) > a & log10(Gtprime) <= a+.5)],0)
  fit = fitdist(betaz,distr="beta",method="mge")
  az = fit$estimate[1]
  bz = fit$estimate[2]
  return(c(az,bz))
}

z = seq(.5,4,.1)
muz = 0*z
params = matrix(0,nrow=2,ncol=length(z))
for(i in 1:length(z)){
  params[,i] = betazfit(z[i])
  muz[i] = params[1,i]/(params[1,i]+params[2,i])
}

sigfit = nls(muz~p1*exp(p2*(z))/(p3+exp(p2*(z))),start=list(p1=.5,p2=.5,p3=10))
p1 = .6753
p2 = 2.1801
p3 = 79.8746

sigmoidTE = function(x,p1,p2,p3){
  p1*exp(p2*x)/(p3+exp(p2*x))
}

w = seq(0,5,.01)
plot(z,muz,xlim=c(0,4.5),ylim=c(0,.9),ylab="Proportion of Mosquitoes Infected", xlab="log10 Gametocyte Density per Microliter", main="Transmission Efficiency")
lines(w,sigmoidTE(w,p1,p2,p3))

abline(h = p1)

resid = muz - sigmoidTE(z,p1,p2,p3)
hist(resid,breaks = 10)
qqnorm(resid)
lines(seq(-2,2,.01),.02*seq(-2,2,.01))
plot(resid)

plot(z,params[1,],col="red",type="l",ylim=c(0,3),xlab="log10 Gametocyte Density per microL",ylab="Parameter Value",main="Beta Parameterization given Gametocytemia")
lines(z,params[2,],col="blue")
legend(3,2.6,legend=c("alpha","beta"),col=c("red","blue"),lty=c(1,1))

a = params[1,]
b = params[2,]
mu = a/(a+b)
var = a*b/((a+b)^2*(a+b+1))
plot(mu,var,xlab="Mean of Fitted Beta", ylab="Variance of Fitted Beta",main="Mean-Variance Relationship for Fitted Beta Distributions",xlim=c(0,1),ylim=c(0,.2))

mu2 = mu^2
parfit = lm(var~mu+mu2)
parab = function(mu){
  pmax(parfit$coefficients[1]+parfit$coefficients[2]*mu+parfit$coefficients[3]*mu^2,0)
}
lines(seq(0,1,.01),parab(seq(0,1,.01)))


## tried fitting beta-like function ( y = ax^b(1-x)^c ), seems more (locally) parabolic
## (and mean of beta will ALWAYS be below .7)
#muc = 1-mu
#lvar = log(var)
#lmu = log(mu)
#lmuc = log(1-mu)
#mvfit = lm(lvar~lmu+lmuc)
#c1 = mvfit$coefficients[1]
#c2 = mvfit$coefficients[2]
#c3 = mvfit$coefficients[3]

#mvfitcurve = function(mu){
#  exp(c1)*mu^c2*(1-mu)^c3
#}

#r = seq(0,1,.01)
#lines(r,mvfitcurve(r))

