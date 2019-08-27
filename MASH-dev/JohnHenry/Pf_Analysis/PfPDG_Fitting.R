library(readxl)
library(vioplot)
library(matrixStats)
library(stinepack)

MT_PT_NP <- read_xlsx("~/GitHub/MASH-Main/MASH-Main/MASH-dev/JohnHenry/PDG/MT_PT_NP.xlsx")
M = as.matrix(MT_PT_NP)

MT_GT_NP <- read_excel("~/Malaria Data Files/MT_GT_NP.xlsx")
G = as.matrix(MT_GT_NP)

n = sample(1:334,1)
plot(log10(M[,n]),type="l",xlim=c(0,300),ylim=c(0,5),xlab="Days",ylab="log10 Parasite Density per microliter")
lines(log10(G[,n]),type="l",xlim=c(0,300),ylim=c(0,5),col="red")
title(main="Example Infection")

MP = M
MP[which(MP==0)] = NaN
rm = rowMeans(MP,na.rm=T)
rv = rowVars(MP,na.rm=T)

#Power Law, Removing the subpatent infections
plot(log10(rm),xlim=c(0,300),type="l",xlab="Days Since First Pantency",ylab="Log10 Mean Asexual Parasite Densities",main="Daily Mean Pf Density Conditioned on Patent Parasitemia",ylim=c(0,5))
plot(log10(rv),xlim=c(0,300))
plot(log10(rm)[1:125],log10(rv[1:125]),xlab="log10 Mean of Asexual Densities",ylab="log10 Variance of Asexual Densities",main="Variance-to-Mean Power Law for Asexual Parasitemia")
rmrvfit = lm(log10(rv[1:125])~log10(rm[1:125]))
lines(seq(-1,6),rmrvfit$coefficients[1]+rmrvfit$coefficients[2]*seq(-1,6))
slope = 1.88
legend(c(2,5,9.5),legend=c("Slope: " + slope))
#Power Law, With recorded subpatent infections as zeros
plot(log10(rowMeans(M,na.rm=T)[1:125]),log10(rowVars(M,na.rm=T)[1:125]),xlab="log10 Mean of Asexual Densities",ylab="log10 Variance of Asexual Densities",main="Variance-to-Mean Power Law for Asexual Parasitemia")
rmrvfit = lm(log10(rowVars(M,na.rm=T)[1:125])~log10(rowMeans(M,na.rm=T)[1:125]))
lines(seq(-1,6),rmrvfit$coefficients[1]+rmrvfit$coefficients[2]*seq(-1,6))
#

##
##
##
####daily plot of parasite densities
plot(log10(rm),type="l",xlim=c(0,80),xlab="Days Since First Patency",ylab="log10 Parasites per cmm",ylim=c(0,5),main="Daily Mean P falciparum and P vivax Density Conditioned on Patent Parasitemia",lty=2)
#abline(h=log10(88))
lines(MVP)
legend(60,4.8,legend=c("Pv","Pf"),col=c("black","black"),lty=c(1,2))

title(main="Average Asexual and Gametocyte Densities Conditioned on Infection")
#abline(h=log10(5),lty=2)

GP = G
GP[which(GP==0)] = NaN
rmg = rowMeans(GP,na.rm=T)
lines(log10(rmg),col="red")

abline(h=log10(10),lty=2)

ccf(rm[4:225],rmg[4:225],lag.max=25,type="correlation",ylim=c(0,1),ylab="Correlation",xlab="Lag (Days)", main="CCF of Asexuals to Gametocytes for P falciparum")
abline(h=1)
plot(log10(rm[1:233]),type="l",ylim=c(0,5))
lines(log10(rmg[9:241]),col="red")
plot(log10(rm[4:200]),log10(rmg[12:208]),xlab="log10 Asexual Densities (Parasites per Microliter), 8 Days Lagged",ylab="log10 Gametocyte Densities (Parasites per Microliter)",main="Comparison of Lagged Asexxual to Gametocyte Densities")

rms = rm[1:233]
rmgs = rmg[9:241]
plot(log10(rms),log10(rmgs),xlab="Lagged Asexuals",ylab="Gametocytes")
#pgpl = lm(log10(rmgs)~log10(rms))
#ttt = seq(-2,5,.01)
#lines(ttt,pgpl$coefficients[1]+pgpl$coefficients[2]*ttt)

#hist(log10(M[100,]))
hist(log10(colMeans(MP[1:7,],na.rm=T)),breaks=15,xlim=c(0,5.5))
hist(log10(colMeans(MP[30:36,],na.rm=T)),breaks=10,xlim=c(0,5.5))
hist(log10(colMeans(MP[70:76,],na.rm=T)),breaks=10,xlim=c(0,5.5))
hist(log10(colMeans(MP[110:116,],na.rm=T)),breaks=10,xlim=c(0,5.5))
hist(log10(colMeans(MP[160:166,],na.rm=T)),breaks=5,xlim=c(0,5.5))
hist(log10(colMeans(MP[200:206,],na.rm=T)),breaks=5,xlim=c(0,5.5))

MV = matrix(0,nrow=30,ncol=length(M[1,]))
MV[which(MV==0)] = NaN
for(i in 1:30){
  MV[i,] = colMeans(MP[(30*(i-1)+1):(30*i),],na.rm=T)
}

MV[which(MV==0)] = NaN

vioplot(log10(na.omit(MV[1,])),log10(na.omit(MV[2,])),log10(na.omit(MV[3,])),log10(na.omit(MV[4,])),log10(na.omit(MV[5,])),log10(na.omit(MV[6,])),log10(na.omit(MV[7,])),
        ylim=c(0,5.2),col="lightblue")
title(main = "Violin Plot of Monthly Mean Patent Asexual Parasite Densities", xlab="Months Post-Infection",ylab="log10 Mean Parasite Density per microliter")
abline(h=log10(88),lty=2)
abline(h=log10(8),lty=2)

GV = matrix(0,nrow=30,ncol=length(G[1,]))
for(i in 1:30){
  GV[i,] = colMeans(GP[(30*(i-1)+1):(30*i),],na.rm=T)
}

GV[which(GV==0)] = NaN

vioplot(log10(na.omit(GV[1,])),log10(na.omit(GV[2,])),log10(na.omit(GV[3,])),log10(na.omit(GV[4,])),log10(na.omit(GV[5,])),log10(na.omit(GV[6,])),log10(na.omit(GV[7,])),
        ylim=c(0,5.2),col="lightblue")
title(main = "Violin Plot of Monthly Mean Gametocyte Densities", xlab="Months Post-Infection",ylab="Mean log10 Gametocyte Density per microliter")
abline(h=log10(88))
abline(h=log10(8),lty=2)

mu = rep(0,250)
SampleN = rep(0,250)
for(i in 1:250){
  mu[i] = log10(mean(M[i,],na.rm=T))
  SampleN[i] = sum(M[i,]>0,na.rm=T)
}
N = rep(0,334)
for(i in 1:334){
  position = which(!is.na(M[,i]))
  nonzero = which(M[,i]>0)
  N[i] = max(intersect(position,nonzero))
}
#mu[30] = log10(mean(colMeans(M[((29)+1):nrow(M),],na.rm=T),na.rm=T))
#N[30] = sum(colMeans(M[((29)*7+1):nrow(M),],na.rm=T)>0,na.rm=T)

N = sort(N)

SampleN = rep(0,250)

Ndays = rep(0,250)
for(i in 1:251){
  Ndays[i] = tail(cumsum(N<i),1)
}
Ndays = Ndays/334
plot(1-Ndays,main="Empirical Survival Function",xlab="Days Since First Patency",ylab="Proportion of Patients Infected",type="l")
plot(Ndays,log(1-Ndays),type="s")

y = log(1-Ndays)[1:250]
x = 1:length(y)
surv = lm(y~x+0)
summary(surv)
lambda = -surv$coefficients[[1]]

plot(1:length(y),y)
lines(seq(0,length(y),.1),-lambda*seq(0,length(y),.1))

plot(1:length(Ndays),1-Ndays,xlab="Weeks",ylab="Proportion of Persisting Infections")
lines(seq(0,length(Ndays),.1),exp(-lambda*seq(0,length(Ndays),.1)))

CDFsurv = function(x){
  1-exp(-lambda*x)
}

plot(1:length(Ndays),Ndays,type="s",xlab="Days",ylab="Probability",main="Duration of Infection, Empirical and Exponential CDFs")

t = 0:250
lines(t,CDFsurv(t))

ks.test(1-Ndays,pexp)

hist(N,breaks=30)

s = rep(0,30*7)
for(i in 1:250){
  s[i] = log10(var(M[i,],na.rm=T))
}
#s[30] = log10(var(colMeans(M[(29+1):nrow(M),],na.rm=T),na.rm=T))

f = splinefun(mu,method="fmm")
plot(1:250,mu,ylim=c(0,5),xlab="Days Since First Detection",ylab="log10 Parasites / microliter")
lines(seq(1,250,.01),f(seq(1,250,.01)),lty=2)
abline(h=log10(88))

var = s
lines(1:250,sqrt(var),lty=2)

fit = lm(var~mu)
fitWeighted = lm(var~mu,weights=sqrt(SampleN))
lin = function(x){
  fit$coefficients[[1]] + fit$coefficients[[2]]*x
}
linWeighted = function(x){
  fitWeighted$coefficients[[1]] + fitWeighted$coefficients[[2]]*x
}

plot(mu,var,xlab="Mean of log10 Parasites per microliter",ylab="Variance of log10 Parasites per microliter")
x = seq(0,5,.01)
lines(x,lin(x))
lines(x,linWeighted(x),col="blue")
#abline(v=log10(88),lty=2)
#abline(v=log10(8))
title(main="Mean-Variance Power Law for Asexual Parasitemia")


## examination of residuals
resid = var-lin(mu)
plot(resid)

me = mean(resid)
se = var(resid)
residNorm = (resid-me)/sqrt(se)
hist(residNorm,breaks=50)


#### gametocytes

gmu = rep(0,250)
for(i in 1:250){
  gmu[i] = log10(mean(G[i,],na.rm=T))
}

g = splinefun(gmu,method="fmm")
plot(1:250,gmu,ylim=c(-.2,5),xlab="Weeks Since First Detection",ylab="Mean log10 Parasites / microliter")
lines(seq(1,250,.01),g(seq(1,250,.01)),lty=2,col="red")
points(1:250,mu)
lines(seq(1,250,.01),f(seq(1,250,.01)),lty=2)
title(main="Mean Parasitemia Profile, Conditioned on Persistence")


gs = rep(0,250)
for(i in 1:250){
  gs[i] = log10(var(G[i,],na.rm=T))
  gs[which(is.infinite(gs))]=NaN
}

gfit = lm(gs[which(!is.na(gmu))]~gmu[which(!is.na(gmu))])
glin = function(x){
  gfit$coefficients[[1]]+gfit$coefficients[[2]]*x
}
plot(gmu,gs,xlab = "log10 Mean Gametocytemia", ylab = "log10 Variance of Gametocytemia")
lines(gmu,glin(gmu))
title(main="Mean-Variance Power Law for Gametocytemia")

plot(gfit$residuals)
gresidNorm = (gfit$residuals-mean(gfit$residuals))/(sqrt(var(gfit$residuals)))
hist(gfit$residuals,breaks=20)
qqnorm(gresidNorm)
lines(seq(-2,2,.1),seq(-2,2,.1))

## check for cross-correlation in mu, gmu - best correlation at a 1 week lag
ccf(mu[10:50],gmu[10:50],type="correlation",main="")
title(main="Cross-Correlation Between Asexual Parasitemia and Gametocytemia")

pg = lm(gmu[2:30]~mu[1:29])
pgline = function(x){
  pg$coefficients[1]+pg$coefficients[[2]]*x
}
plot(mu[1:29],gmu[2:30],xlab="Mean log10 Parasites per microliter", ylab="One Week Lagged Mean log10 Gametocytes per microliter")
lines(mu[1:29],pgline(mu[1:29]))
title(main="Mean Asexual Parasitemia vs Lagged Gametocytemia")

plot(10^mu[1:29],10^gmu[2:30],xlab="mean asexual densities",ylab="lagged mean gametocyte densities")
lines(10^seq(1,5,.01),.047*10^(1.048*seq(1,5,.01)))



############################################################ ACTUAL power laws for mean/variance



rv = rep(0,dim(M)[1])
for(i in 1:dim(M)[1]){
  rv[i] = var(M[i,],na.rm=T)
}
rm = rowMeans(M,na.rm=T)
plot(rv,type="l",xlim=c(0,500),xlab="Days Since First Detectable",ylab="log10 Parasites per microL")
title(main="Daily Variance of Parasite Densities")

rv[which(rv==0)]=NaN
rm[which(rm==0)]=NaN
rm = log10(rm)
rv = log10(rv)
mv = lm(rv~rm)
fmv = function(x){
  mv$coefficients[[1]]+mv$coefficients[[2]]*x
}
###
###
###
#### Mean/Variance relationship for Asexual parasitemia
plot(rm,rv,xlab="log10 Mean Parasite Density",ylab="log10 Variance of Parasite Density")
#lines(seq(-1,10,.01),fmv(seq(-1,10,.01)),col="blue")
title(main="Mean-Variance Power Law for Asexual Parasites")
lines(seq(1,5,.01),fmv(seq(1,5,.01)))

plot(mv$residuals,xlab="Days",ylab="Residual")
title(main="Plot of Residuals from Asexual Mean-Variance Power Law")
hist(mv$residuals,xlab="Residual",main="")
title("Histogram of Residuals from Asexual Mean-Variance Power Law")
qqnorm((mv$residuals-mean(mv$residuals))/sqrt(var(mv$residuals)))
lines(seq(-4,4,.1),seq(-4,4,.1))

grv = rep(0,dim(G)[1])
for(i in 1:dim(G)[1]){
  grv[i] = var(G[i,],na.rm=T)
}
grm = rowMeans(G,na.rm=T)
plot(log10(grv),type="l",xlim=c(0,500),xlab="Days Since First Detectable",ylab="log10 Gametocytes per microL")
abline(h=log10(88))
title(main="Daily Variance of Gametocyte Densities")
abline(h=log10(5),lty=2)

grv[which(grv==0)]=NaN
grm[which(grm==0)]=NaN
gmv = lm(log10(grv)~log10(grm))

ggmv = function(x){
  gmv$coefficients[[1]]+gmv$coefficients[[2]]*x
}
plot(log10(grm),log10(grv),xlab="log10 Mean Gametocyte Density",ylab="log10 Variance of Gametocyte Density")
lines(seq(-1,10,.01),ggmv(seq(-1,10,.01)),col="blue")
title(main="Mean-Variance Power Law for Gametocytes")

plot(gmv$residuals)
title(main="Plot of Residuals from Gametocyte Mean-Variance Power Law")
hist(gmv$residuals,main="")
title("Histogram of Residuals from Gametocyte Mean-Variance Power Law")
qqnorm((gmv$residuals-mean(gmv$residuals))/sqrt(var(gmv$residuals)))
lines(seq(-4,4,.1),seq(-4,4,.1))

ccf(rm,grm,na.action = na.contiguous,main="Cross-Correlation between Asexual and Gametocyte Densities",ylab="CCF")
plot(log10(rm[1:200]),log10(grm[9:208]),xlab="Lagged log10 Mean Asexual Parasite Densities per microL",ylab="log10 Mean Gametocyte Densities",main="Lagged Gametocyte Production")
rmd = log10(rm)[1:(length(rm)-8)]
rmd[which(is.infinite(rmd))] = NaN
grmd = log10(grm)[9:length(rm)]
grmd[which(is.infinite(grmd))] = NaN
ptgt = lm(grmd~rmd)
fg = function(x){
  ptgt$coefficients[[1]]+x*ptgt$coefficients[[2]]
}
lines(seq(-1,5,.01),fg(seq(-1,5,.01)))

##restrict to larger than ~88 parasites per microliter for both gametocytes and asexuals

rmdp = 10^rmd[which(grm>1.8)]
grmdp = grmd[which(grm>1.8)]
plot(log10(rmdp),grmdp,xlab="8 Day Lagged log10 Mean Asexual Parasite Densities",ylab="log10 Mean Gametocyte Densities")
title(main="Lagged Power Law Relationship")
ptgtp = lm(grmdp~log10(rmdp))
fgp = function(x){
  ptgtp$coefficients[[1]]+x*ptgtp$coefficients[[2]]
}
lines(seq(-1,5,.01),fgp(seq(-1,5,.01)))

plot(ptgtp$residuals)
hist(ptgtp$residuals,breaks=10)

qqnorm((ptgtp$residuals-mean(ptgtp$residuals))/sqrt(var(ptgtp$residuals)))
lines(seq(-5,5,.01),seq(-5,5,.01))

##uses restricted dataset
rmpp = c(0,0,0,0,0,0,0,0,rm)
plot(log10(rm),type="l",ylim=c(-1,5),xlim=c(0,230),xlab="Days",ylab="log10 Density per microliter")
lines(ptgtp$coefficients[[1]]+log10(rmpp)*ptgtp$coefficients[[2]],col="blue")
lines(log10(grm),col="red")
title(main="Measured vs Fitted Gametocytes with Asexual Predictor")

##uses full dataset
rmpp = c(0,0,0,0,0,0,0,0,rm)
plot(log10(rm),type="l",ylim=c(-1,5),xlim=c(0,230),xlab="Days",ylab="log10 Density per microliter")
lines(ptgt$coefficients[[1]]+log10(rmpp)*ptgt$coefficients[[2]],col="blue")
lines(log10(grm),col="red")
title(main="Measured vs Fitted Gametocytes with Asexual Predictor")






################## piecewise-linear fitting of the two previous models

fgpl = function(x){
    x1 = x[which(x>=3.3)]
    x2 = x[which(x<3.3)]
    y1 = ptgtp$coefficients[[1]]+x1*ptgtp$coefficients[[2]]
    y2 = ptgt$coefficients[[1]]+x2*ptgt$coefficients[[2]]
    y = rep(0,length(x))
    y[which(x>=3.3)] = y1
    y[which(x<3.3)] = y2
    return(y)
}

plot(log10(rm[9:208]),log10(grm[1:200]),xlab="9 Day Lagged log10 Mean Asexual Parasite Densities",ylab="log10 Mean Gametocyte Densities")
title(main="Lagged Power Law Relationship")
lines(seq(-1,5,.01),fgpl(seq(-1,5,.01)))

##see how it all fits together
plot(log10(rm),type="l",ylim=c(0,5),xlim=c(0,250),xlab="Days",ylab="log10 Density per microliter")
lines(fgpl(log10(rmpp)),col="blue",xlab="Days",ylab="log10 Density per microliter",type="l",ylim=c(-1,3.5),xlim=c(0,240))
lines(log10(grm),col="red")
title(main="Measured vs Fitted Gametocytes with Asexual Predictor")

presid = log10(grm[9:length(grm)])-fgpl(log10(rm[1:(length(rm)-8)]))
presid = presid[!is.na(presid)]
plot(presid)
hist(presid,freq=F,breaks=20)
qqnorm((presid-mean(presid))/sqrt(var(presid)))
lines(seq(-4,4,.01),seq(-4,4,.01))

plot(10^fgpl(log10(rmpp)),col="blue",xlab="Days",ylab="Density per microliter",type="l",xlim=c(0,240),ylim=c(0,2500))
lines(grm,col="red")
title(main="Measured vs Fitted Gametocytes with Asexual Predictor")



####################################### Tent Parameters ######################################

# these two are calculated already
firstPatent
lastPatent
#these two need declaration + computation
peak = rep(0,333)
peakT = rep(0,333)

for(i in 1:333){
  peak[i] = max(M[,i],na.rm=T)
  peakT[i] = which(M[,i]==peak[i])[1]-firstPatent[i]
}

hist(log10(peak),breaks=30,freq=F,xlim=c(2,6),ylim=c(0,1.2))
z = seq(2,6,.01)
lines(z,dnorm(z,mean(log10(peak)),var(log10(peak))+.15))

hist(peakT,breaks=30,freq=F,ylim=c(0,.2))
z = seq(0,80)
lines(z,dpois(z,mean(peakT)-3))

plot(peakT,log10(peak))
plot(lastPatent,log10(peak))


################################## Test for Recrudescence ####################################


plot(log10(rm),xlim=c(0,300),type="l",ylim=c(-1,6))
upper = rm + 3*sqrt(rv)
lines(log10(upper))

old = rep(0,1326)
for(i in 1:333){
  for(j in 1:1326){
    if(M[j,i])
    if(M[j,i] > upper[j]){
      old[j] = old[j]+1
    }
  }
}

##################################### Individual Variation ###################################
######################## linear fit from peak to end, FFT on residuals #######################

## find maximum over every individual infection
Mmax = rep(NaN,333)
## find the position (day) maximum occurs
pMmax = Mmax
## day of first patency
firstPatent = Mmax
## day of last patency
lastPatent = Mmax

for(i in 1:333){
  Mmax[i] = max(M[,i],na.rm=T)
  firstPatent[i] = min(which(M[,i] > 0))
  pMmax[i] = min(which(M[,i] == Mmax[i]))-firstPatent[i]+1
  lastPatent[i] = max(which(M[,i]>0))-firstPatent[i]+1
}

hist(pMmax,breaks=50,freq=F,ylim=c(0,.18))
f = function(a,t){a/sqrt(2*pi*t^3)*exp(-(a-t)^2/(2*t))}
d = 1:80
lines(f(7,d))
hist(log10(Mmax),xlim=c(2,7),breaks=20)
hist(lastPatent,breaks=20,xlab="Duration of Infection (in Days)")

#### run linear fit from peak to end of every infection ####

intercept = rep(0,333)
slope = rep(0,333)
for(i in 1:333){
  x = pMmax[i]:lastPatent[i]
  y = log10(M[pMmax[i]:lastPatent[i],i])
  model = lm(y~x)
  intercept[i] = model$coefficients[[1]]
  slope[i] = model$coefficients[[2]]
}

## these 2 have undefined slope
which(is.na(slope))
## these 8 had a positive slope
which(slope>0)

LF = NaN*M
for(i in 1:333){
  for(j in pMmax[i]:lastPatent[i]){
    LF[j,i] = intercept[i]+j*slope[i]
  }
}

R = log10(M)-LF
plot(R[,2],xlim=c(1,60))




################ Average TE over the course of an infection ###############
p1=.689
p2= 2.145
p3 = 144.351
plot(sigmoidTE(log10(rmg),p1,p2,p3),ylim=c(0,1),type="l",xlim=c(0,300),xlab="Days since Patency",ylab="Transmission Efficiency",main="Estimated Transmission Efficiency as a Function of Time")
lines(sigmoidTE(fgpl(c(rep(0,8),log10(rm[1:300]))),p1,p2,p3),col="green")
legend(100,.9,legend=c("Estimated TE from Gt","Estimated TE from Pt through Gt Filter"),col=c("black","green"),lty=c(1,1))
## green here is using the asexuals to predict gametocytes, then gametocytes to predict TE

################ Probability of Fever over the course of an Infection ##############
p1 = .8953
p2 = 3.449
p3 = 5.819*10^4
plot(PropFever[1:365],type="l",ylim=c(0,1),xlab="Days Since Patency", ylab="Proportion with Fever",main="Daily Prevalence of Fever Conditioned on Infection",xlim=c(0,180))
lines(sigmoidFev(log10(rm),p1,p2,p3),col="red")
legend(100,.9,legend=c("Measured Prevalence","Parasite Predicted Prevalence"),col=c("black","red"),lty=c(1,1))
