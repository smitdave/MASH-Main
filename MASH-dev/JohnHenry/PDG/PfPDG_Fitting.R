library(readxl)
library(vioplot)
library(matrixStats)

MT_PT_NP <- read_excel("~/GitHub/MASH-Main/MASH-dev/JohnHenry/PDG/MT_PT_NP.xlsx")
M = as.matrix(MT_PT_NP)

MT_GT_NP <- read_excel("~/Malaria Data Files/MT_GT_NP.xlsx")
G = as.matrix(MT_GT_NP)

rm = rowMeans(M,na.rm=T)
plot(log10(rm),type="l",xlim=c(0,365),xlab="Days Since First Detectable",ylab="log10 Parasites per microL")
abline(h=log10(88))
title(main="Daily Mean Parasite Densities")
abline(h=log10(5),lty=2)

rmg = rowMeans(G,na.rm=T)
lines(log10(rmg),col="red")

ccf(rm,rmg,lag.max=20,type="correlation")

#hist(log10(M[100,]))
hist(log10(colMeans(M[1:7,],na.rm=T)),breaks=10,xlim=c(0,5.5))
hist(log10(colMeans(M[30:36,],na.rm=T)),breaks=10,xlim=c(0,5.5))
hist(log10(colMeans(M[70:76,],na.rm=T)),breaks=10,xlim=c(0,5.5))
hist(log10(colMeans(M[110:116,],na.rm=T)),breaks=10,xlim=c(0,5.5))
hist(log10(colMeans(M[160:166,],na.rm=T)),breaks=10,xlim=c(0,5.5))
hist(log10(colMeans(M[200:206,],na.rm=T)),breaks=8,xlim=c(0,5.5))

MV = matrix(0,nrow=30,ncol=length(M[1,]))
for(i in 1:30){
  MV[i,] = colMeans(M[(30*(i-1)+1):(30*i),],na.rm=T)
}

MV[which(MV==0)] = NaN

vioplot(log10(na.omit(MV[1,])),log10(na.omit(MV[2,])),log10(na.omit(MV[3,])),log10(na.omit(MV[4,])),log10(na.omit(MV[5,])),log10(na.omit(MV[6,])),log10(na.omit(MV[7,])),
        ylim=c(-1,5.2),col="lightblue")
title(main = "Violin Plot of Monthly Mean Asexual Parasite Densities", xlab="Months Post-Infection",ylab="Mean log10 Parasite Density per microliter")
abline(h=log10(88))
abline(h=log10(5),lty=2)

GV = matrix(0,nrow=30,ncol=length(G[1,]))
for(i in 1:30){
  GV[i,] = colMeans(G[(30*(i-1)+1):(30*i),],na.rm=T)
}

GV[which(GV==0)] = NaN

vioplot(log10(na.omit(GV[1,])),log10(na.omit(GV[2,])),log10(na.omit(GV[3,])),log10(na.omit(GV[4,])),log10(na.omit(GV[5,])),log10(na.omit(GV[6,])),log10(na.omit(GV[7,])),
        ylim=c(-1,5.2),col="lightblue")
title(main = "Violin Plot of Monthly Mean Gametocyte Densities", xlab="Months Post-Infection",ylab="Mean log10 Gametocyte Density per microliter")
abline(h=log10(88))
abline(h=log10(5),lty=2)

mu = rep(0,30)
N = rep(0,30)
for(i in 1:29){
  mu[i] = log10(mean(colMeans(M[((i-1)*7+1):(i*7),],na.rm=T),na.rm=T))
  N[i] = sum(colMeans(M[((i-1)*7+1):(i*7),],na.rm=T)>0,na.rm=T)
}
mu[30] = log10(mean(colMeans(M[((29)*7+1):nrow(M),],na.rm=T),na.rm=T))
N[30] = sum(colMeans(M[((29)*7+1):nrow(M),],na.rm=T)>0,na.rm=T)

plot(0:28,N[1:29]/333)
plot(0:28,log(N[1:29]/333))
y = log(N[1:29]/333)
x = 0:28
surv = lm(y~x+0)
summary(surv)
lambda = -surv$coefficients[[1]]

plot(0:28,log(N[1:29]/333))
lines(seq(0,28,.1),-lambda*seq(0,28,.1))

plot(0:28,N[1:29]/333,xlab="Weeks",ylab="Proportion of Persisting Infections")
lines(seq(0,28,.1),exp(-lambda*seq(0,28,.1)))

s = rep(0,30)
for(i in 1:29){
  s[i] = log10(var(colMeans(M[((i-1)*7+1):(i*7),],na.rm=T),na.rm=T))
}
s[30] = log10(var(colMeans(M[(29*7+1):nrow(M),],na.rm=T),na.rm=T))

f = splinefun(mu,method="fmm")
plot(0:29,mu,ylim=c(0,5),xlab="Weeks Since First Detection",ylab="log10 Parasites / microliter")
lines(seq(0,29,.01),f(seq(1,30,.01)),lty=2)
abline(h=log10(88))

var = s
lines(0:29,sqrt(var),lty=2)

fit = lm(var~mu)
lin = function(x){
  fit$coefficients[[1]] + fit$coefficients[[2]]*x
}

plot(mu,var,xlab="Mean of log10 Parasites per microliter",ylab="Variance of log10 Parasites per microliter")
x = seq(0,5,.01)
lines(x,lin(x))
abline(v=log10(88),lty=2)
title(main="Mean-Variance Power Law for Asexual Parasitemia")


## examination of residuals
resid = var-lin(mu)
plot(resid)

me = mean(resid)
se = var(resid)
residNorm = (resid-me)/sqrt(se)
hist(residNorm,breaks=50)
qqnorm(y=residNorm)
lines(seq(-2,2,.01),seq(-2,2,.01))


#### gametocytes

gmu = rep(0,30)
for(i in 1:29){
  gmu[i] = log10(mean(colMeans(G[((i-1)*7+1):(i*7),],na.rm=T),na.rm=T))
}
gmu[30] = log10(mean(colMeans(G[(29*7+1):nrow(G),],na.rm=T),na.rm=T))

g = splinefun(gmu,method="fmm")
plot(0:29,gmu,ylim=c(-.2,5),xlab="Weeks Since First Detection",ylab="Mean log10 Parasites / microliter")
lines(seq(0,29,.01),g(seq(1,30,.01)),lty=2,col="red")
points(0:29,mu)
lines(seq(0,29,.01),f(seq(1,30,.01)),lty=2)
title(main="Mean Parasitemia Profile, Conditioned on Persistence")


gs = rep(0,30)
for(i in 1:30){
  gs[i] = log10(var(colMeans(G[((i-1)*7+1):(i*7),],na.rm=T),na.rm=T))
}

gfit = lm(gs~gmu)
glin = function(x){
  gfit$coefficients[[1]]+gfit$coefficients[[2]]*x
}
plot(gmu,gs,xlab = "log10 Mean Gametocytemia", ylab = "log10 Variance of Gametocytemia")
lines(gmu,glin(gmu))
title(main="Mean-Variance Power Law for Gametocytemia")

plot(gfit$residuals)
gresidNorm = (gfit$residuals-mean(gfit$residuals))/(sqrt(var(gfit$residuals)))
qqnorm(gresidNorm)
lines(seq(-2,2,.1),seq(-2,2,.1))

## check for cross-correlation in mu, gmu - best correlation at a 1 week lag
ccf(mu,gmu,type="correlation",main="")
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


rv = rowVars(M,na.rm=T)
rm = rowMeans(M,na.rm=T)
plot(log10(rv),type="l",xlim=c(0,500),xlab="Days Since First Detectable",ylab="log10 Parasites per microL")
abline(h=log10(88))
title(main="Daily Variance of Parasite Densities")
abline(h=log10(5),lty=2)

rv[which(rv==0)]=NaN
rm[which(rm==0)]=NaN
mv = lm(log10(rv)~log10(rm))
fmv = function(x){
  mv$coefficients[[1]]+mv$coefficients[[2]]*x
}
plot(log10(rm),log10(rv),xlab="log10 Mean Parasite Density",ylab="log10 Variance of Parasite Density")
lines(seq(-1,10,.01),fmv(seq(-1,10,.01)),col="blue")
title(main="Mean-Variance Power Law for Asexual Parasites")

grv = rowVars(G,na.rm=T)
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

ccf(rm,grm,na.action = na.contiguous)
plot(log10(rm)[1:(length(rm)-9)],log10(grm)[10:length(rm)],xlab="Lagged log10 Mean Asexual Parasite Densities",ylab="log10 Mean Gametocyte Densities")
rmd = log10(rm)[1:(length(rm)-9)]
rmd[which(is.infinite(rmd))] = NaN
grmd = log10(grm)[10:length(rm)]
grmd[which(is.infinite(grmd))] = NaN
ptgt = lm(grmd~rmd)
fg = function(x){
  ptgt$coefficients[[1]]+x*ptgt$coefficients[[2]]
}
lines(seq(-1,5,.01),fg(seq(-1,5,.01)))

##restrict to larger than 10 parasites per microliter

rmdp = rmd[which(rmd>1)]
grmdp = grmd[which(rmd>1)]
plot(rmdp,type="l",ylim=c(-1,5))
lines(grmdp,col="red")
plot(rmdp,grmdp)
ptgtp = lm(grmdp~rmdp)
fgp = function(x){
  ptgtp$coefficients[[1]]+x*ptgtp$coefficients[[2]]
}
lines(seq(-1,5,.01),fgp(seq(-1,5,.01)))

plot(rmdp,type="l",ylim=c(-1,5))
lines(grmdp+ptgtp$coefficients[[2]],col="red")
