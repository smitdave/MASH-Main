library(readxl)
library(vioplot)
library(matrixStats)
library(stinepack)
library(viridis)
library(fitdistrplus)
library(zoib)
library(gamlss)

### Extracting Asexual Parasitemia Conditioned on Detection - recorded
### zeroes and non-recordings are both listed as NaN
MT = read.delim('MalariaTherapy.txt')
pID = unique(MT$patient)
UT = (MT$Tretmentfree.==1)

UTd = matrix(NaN,nrow=2000,ncol=length(pID))
for(i in 1:length(pID)){
  UTd[1:length(which(MT$patient==pID[i])),i] = UT[which(MT$patient==pID[i])]
}

MM = matrix(NaN,nrow=2000,ncol=length(pID))
UTM = MM
P = MT$Asexual
P[which(P=='.')]=0
P = as.numeric(as.character(P))
for(i in 1:length(pID)){
  MM[1:length(which(MT$patient==pID[i])),i] = P[which(MT$patient==pID[i])]
  UTM[1:length(which(MT$patient==pID[i])),i] = P[which(MT$patient==pID[i])]*UT[which(MT$patient==pID[i])]
}
MM[which(MM==0)]=NaN
UTM[which(UTM==0)]=NaN

### Repeat for Gametocytes
GG = matrix(NaN,nrow=2000,ncol=length(pID))
UTG = GG
G = MT$Gametocytes
G[which(G=='.')]=0
G = as.numeric(as.character(G))
for(i in 1:length(pID)){
  GG[1:length(which(MT$patient==pID[i])),i] = G[which(MT$patient==pID[i])]
  UTG[1:length(which(MT$patient==pID[i])),i] = G[which(MT$patient==pID[i])]*UT[which(MT$patient==pID[i])]
}
GG[which(GG==0)]=NaN
UTG[which(UTG==0)]=NaN




### Log of Means vs Mean of Logs
### Log of Means
plot(log10(rowMeans(UTM,na.rm=T)[1:300]),type="l")
lines(log10(rowMeans(UTG,na.rm=T)[1:300]),type="l",col="red")
##### MV power law
plot(log10(rowMeans(UTM,na.rm=T)),log10(rowVars(MM,na.rm=T)))

### Mean of Logs
plot(rowMeans(log10(UTM),na.rm=T)[1:200],type="l",ylim=c(1,4.2))
lines(rowMeans(log10(UTG),na.rm=T)[1:200],type="l",col="red")
##### MV power law
plot(rowMeans(log10(UTM),na.rm=T),rowVars(log10(MM),na.rm=T))


### Cross correlation between untreated mean asexual parasites and gametocytes
ccf(rowMeans(log10(UTM),na.rm=T)[5:100],rowMeans(log10(UTG),na.rm=T)[5:100],ylab="CCF",main="Cross-Correlation between Mean log10 Asexual Parasites and Gametocytes")



### Duration of Infection -
### include only untreated infections,
### determine the final day of patency
dur = c()
for(i in 1:length(pID)){
  UTdur = (max(which(MM[,i]>0))==max(which(UTM[,i]>0)))
  if(UTdur==1){
    dur = c(dur,max(which(MM[,i]>0)))
  }
}
hist(dur,freq=F,ylim=c(0,.007),xlab="Days",ylab="Density",main="Duration of Untreated Infections")
gammafit = fitdist(dur,distr="gamma")
lines(seq(0,500),dgamma(seq(0,500),shape=2.058,rate=.0158))
abline(v=mean(dur),col="red")
weibullfit = fitdist(dur,distr="weibull")
lines(seq(0,500),dweibull(seq(0,500),shape=1.51,scale=145.24),col="blue")
lognormalfit = fitdist(dur,distr="lnorm")
lines(seq(0,500),dlnorm(seq(0,500),4.61,.78),col="green")
legend(250,.007,legend=c("Gamma","Weibull","Lognormal"),lty=1,col=c("black","blue","green"))
### Note - aic and bic are both ambivalent to gamma and weibull distributions, and both show
### the two are superior to lognormal



### Patent Fraction
##### Patent fraction for untreated infections
UTID = rep(0,length(pID))
for(i in 1:length(pID)){
  UTID[i] = (max(which(MM[,i]>0))==max(which(UTM[,i]>0)))
}
pat = rep(0,300)
for(i in 1:300){
  pat[i] = sum(dur>i,na.rm=T)
}
plot(1-rowSums(UTM[,which(UTID==1)]>0,na.rm=T)[1:300]/pat[1:300],type="l",xlab="Day",ylab="Fraction Subpatent",main="Fraction of Untreated Infections Subpatent")
##### Patent fraction for all infections
durall = rep(0,length(pID))
for(i in 1:length(pID)){
  durall[i] = max(which(MM[,i]>0))
}
patall = rep(0,300)
for(i in 1:300){
  patall[i] = sum(durall>i,na.rm=T)
}
plot(1-rowSums(MM>0,na.rm=T)[1:300]/patall[1:300],type="l",xlab="Day",ylab="Fraction Subpatent",main="Fraction of All Infections Subpatent")


### Fever - look at fraction with a fever
Temperature = matrix(NaN,nrow=2000,ncol=length(pID))
Temp = MT$Temp
Temp[which(Temp=='.')]=NaN
Temp = as.numeric(as.character(Temp))
for(i in 1:length(pID)){
  Temperature[1:length(which(MT$patient==pID[i])),i] = Temp[which(MT$patient==pID[i])]
}
Fever = (Temperature>38)
UTFever = (UTTemperature>38)

plot(rowMeans(Fever,na.rm=T)[1:200])

plot(rowMeans(log10(UTM),na.rm=T)[1:150],rowMeans(Fever,na.rm=T)[1:150],ylim=c(0,1),xlab="Mean Log10 Asexual Parasite Densities",ylab="Fraction with Fever",main="Fever Fraction as a function of Parasitemia")
points(rowMeans(log10(UTM),na.rm=T)[1:5],rowMeans(Fever,na.rm=T)[1:5],col="red")

aa = rowMeans(log10(UTM),na.rm=T)[3:150]
bb = rowMeans(Fever,na.rm=T)[3:150]
sigfitfev = nls(bb~p1*exp(p2*aa)/(p3+exp(p2*aa)),start=list(p1=.9,p2=1,p3=100))
p1 = .895
p2 = 5.388
p3 = 1.307*10^6
sigmoidFev = function(x,p1,p2,p3){
  p1*exp(p2*x)/(p3+exp(p2*x))
}
lines(seq(1,6,.01),sigmoidFev(seq(1,6,.01),p1,p2,p3))
abline(h=p1,lty=2)

plot(rowMeans(Fever,na.rm=T)[1:200],ylim=c(0,1))
lines(1:180,sigmoidFev(rowMeans(log10(UTM),na.rm=T)[1:180],p1,p2,p3),col="red")



### Transmission Efficiency
Mosquito_Transmission = MT$Mosq.
Mosquito_Transmission[which(Mosquito_Transmission==".")]=NaN
Mosq = as.numeric(as.character(Mosquito_Transmission))
TE = matrix(NaN,2000,length(pID))
UTTE = TE

for(i in 1:length(pID)){
  TE[1:length(which(MT$patient==pID[i])),i] = Mosq[which(MT$patient==pID[i])]
  UTTE[1:length(which(MT$patient==pID[i])),i] = Mosq[which(MT$patient==pID[i])]*UT[which(MT$patient==pID[i])]
}

plot(rowMeans(TE,na.rm=T)[1:150],type="l")
plot(rowMeans(log10(UTG),na.rm=T)[1:150],rowMeans(TE,na.rm=T)[1:150])

plot(log10(G),Mosq,xlim=c(.5,4.5),xlab="Log10 Gametocytemia",ylab="Transmission Efficiency")

### restrict those who were treated
GUT = as.numeric(UT)*G
GUT[which(GUT==0)]=NaN
TEfit = function(x){
  temp = rep(0,length(x)-1)
  weight = temp
  for(i in 1:length(x)){
    temp[i] = mean(Mosq[which(log10(GUT)>=(x[i]-.5) & log10(GUT)<(x[i]+.5))],na.rm=T,weight=dnorm(log10(GUT)[which(log10(GUT)>=(x[i]-.5) & log10(GUT)<(x[i]+.5))]-x[i],x[i],.25))
    weight[i] = length(Mosq[which(log10(GUT)>=(x[i]-.5) & log10(GUT)<(x[i]+.5))])
  }
  output = list(TE = temp/100, weight = weight)
  return(output)
}

x = seq(.6,4.3,.1)
SmoothTE = TEfit(x)
sigfitTE = nls(SmoothTE$TE~p1*exp(p2*x)/(p3+exp(p2*x)),start=list(p1=.6,p2=2,p3=100),weights=SmoothTE$weight)
sigfitTE
p1TE = .6937
p2TE = 2.0981
p3TE = 80.5279
sigTE = function(x,p1TE,p2TE,p3TE){
  p1TE*exp(p2TE*x)/(p3TE+exp(p2TE*x))
}
plot(x,SmoothTE$TE,xlab="Smoothed Log10 Gametocytemia",ylab="Transmission Efficiency",ylim=c(0,1))
lines(x,sigTE(x,p1TE,p2TE,p3TE),col="green")

plot(log10(GUT),Mosq/100,xlim=c(.5,4.5),xlab="Log10 Gametocytemia",ylab="Transmission Efficiency",cex=.5,main="Transmission Efficiency Predicted by Gametocytemia")
points(x,SmoothTE$TE,xlab="Smoothed Log10 Gametocytemia",ylab="Transmission Efficiency",pch=19,col="blue")
lines(x,sigTE(x,p1TE,p2TE,p3TE),col="dark green",lwd=5)
abline(h=p1TE,lty=2,col="dark green",lwd=3)

plot(rowMeans(TE,na.rm=T)[1:250]/100)
lines(sigTE(rowMeans(log10(UTG),na.rm=T)[1:250],p1TE,p2TE,p3TE),col="dark green")



###### Organizing Figures


### Duration of Untreated Infections

#par(mfrow=c(1,2))
#hist(durall,xlab="Days",ylab="Density",freq=F,main="Duration of All Infections")
#expfitall = fitdist(durall,distr="exp")
#lines(seq(0,500),dexp(seq(0,500),rate=.0093),col="red")
#gammafitall = fitdist(durall,distr="gamma")
#lines(seq(0,500),dgamma(seq(0,500),rate=.0116,shape=1.2449))
#weibullfitall = fitdist(durall,distr="weibull")
#lines(seq(0,500),dweibull(seq(0,500),shape=1.148,scale=112.787),col="blue")

par(mfrow=c(1,1))
hist(dur,freq=F,ylim=c(0,.007),xlab="Days",ylab="Density",main="Duration of Untreated Infections")
gammafit = fitdist(dur,distr="gamma")
lines(seq(0,500),dgamma(seq(0,500),shape=2.058,rate=.0158))
abline(v=mean(dur),col="red")
weibullfit = fitdist(dur,distr="weibull")
lines(seq(0,500),dweibull(seq(0,500),shape=1.51,scale=145.24),col="blue")
lognormalfit = fitdist(dur,distr="lnorm")
lines(seq(0,500),dlnorm(seq(0,500),4.61,.78),col="green")
legend(250,.007,legend=c("Gamma","Weibull","Lognormal"),lty=1,col=c("black","blue","green"))


### Patent Fraction

par(mfrow=c(1,2))
plot(1-rowSums(UTM[,which(UTID==1)]>0,na.rm=T)[1:300]/pat[1:300],type="l",xlab="Day",ylab="Fraction Subpatent",main="Fraction of Untreated Infections Subpatent")
plot(1-rowSums(MM>0,na.rm=T)[1:300]/patall[1:300],type="l",xlab="Day",ylab="Fraction Subpatent",main="Fraction of All Infections Subpatent")


### Fever Probability and TE

par(mfrow=c(2,2))

plot(rowMeans(log10(UTM),na.rm=T)[1:150],rowMeans(Fever,na.rm=T)[1:150],ylim=c(0,1),xlab="Mean Log10 Asexual Parasite Densities",ylab="Fraction with Fever",main="Fever Fraction as a function of Parasitemia",cex=.5)
points(rowMeans(log10(UTM),na.rm=T)[1:5],rowMeans(Fever,na.rm=T)[1:5],col="red")
lines(seq(1,6,.01),sigmoidFev(seq(1,6,.01),p1,p2,p3),col="red",lwd=3)
abline(h=p1,lty=2)

plot(log10(G),Mosq/100,xlim=c(.5,4.5),xlab="Log10 Gametocytemia",ylab="Transmission Efficiency",cex=.5,main="Transmission Efficiency Predicted by Gametocytemia")
points(x,SmoothTE$TE,xlab="Smoothed Log10 Gametocytemia",ylab="Transmission Efficiency",pch=19,col="blue")
lines(x,sigTE(x,p1TE,p2TE,p3TE),col="dark green",lwd=3)
abline(h=p1TE,lty=2,col="dark green",lwd=3)

plot(rowMeans(Fever,na.rm=T)[1:200],ylim=c(0,1),xlab="Days Since First Patent",ylab="Probability of Fever",cex=.5,main="Predicted Average Probability of Fever")
lines(1:180,sigmoidFev(rowMeans(log10(UTM),na.rm=T)[1:180],p1,p2,p3),col="red")

plot(rowMeans(TE,na.rm=T)[1:250]/100,cex=.5,xlab="Days Since First Patent",ylab="Transmission Efficiency",main="Predicted Average Transmission Efficiency")
lines(sigTE(rowMeans(log10(GG),na.rm=T)[1:250],p1TE,p2TE,p3TE),col="dark green")


### Beta Binomial TE fits

## all data
par(mfrow=c(2,3))
TE1 = Mosq[which(log10(G)>=1 & log10(G)<1.5 & !is.na(Mosq))]/100
TE2 = Mosq[which(log10(G)>=1.5 & log10(G)<2 & !is.na(Mosq))]/100
TE3 = Mosq[which(log10(G)>=2 & log10(G)<2.5 & !is.na(Mosq))]/100
TE4 = Mosq[which(log10(G)>=2.5 & log10(G)<3 & !is.na(Mosq))]/100
TE5 = Mosq[which(log10(G)>=3 & log10(G)<3.5 & !is.na(Mosq))]/100
TE6 = Mosq[which(log10(G)>=3.5 & log10(G)<4 & !is.na(Mosq))]/100
TE1fit = fitdist(TE1,distr="beta","mme")
hist(TE1,freq=F,breaks=20,xlab="Transmission Efficiency",main="1 <= log10[Gametocyte] < 1.5")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE1fit$estimate[1],TE1fit$estimate[2]))
TE2fit = fitdist(TE2,distr="beta","mme")
hist(TE2,freq=F,breaks=20,xlab="Transmission Efficiency",main="1.5 <= log10[Gametocyte] < 2") 
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE2fit$estimate[1],TE2fit$estimate[2]))
TE3fit = fitdist(TE3,distr="beta","mme")
hist(TE3,freq=F,breaks=20,xlab="Transmission Efficiency",main="2 <= log10[Gametocyte] < 2.5")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE3fit$estimate[1],TE3fit$estimate[2]))
TE4fit = fitdist(TE4,distr="beta","mme")
hist(TE4,freq=F,breaks=20,xlab="Transmission Efficiency",main="2.5 <= log10[Gametocyte] < 3")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE4fit$estimate[1],TE4fit$estimate[2]))
TE5fit = fitdist(TE5,distr="beta","mme")
hist(TE5,freq=F,breaks=20,xlab="Transmission Efficiency",main="3 <= log10[Gametocyte] < 3.5")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE5fit$estimate[1],TE5fit$estimate[2]))
TE6fit = fitdist(TE6,distr="beta","mme")
hist(TE6,freq=F,breaks=20,xlab="Transmission Efficiency",main="3.5 <= log10[Gametocyte] < 4") 
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE6fit$estimate[1],TE6fit$estimate[2]))

## zeros modeled separately
par(mfrow=c(2,3))
hist(TE1[which(TE1>0)],freq=F,breaks=20,xlab="Transmission Efficiency",main="1 <= log10[Gametocyte] < 1.5",ylim=c(0,8),xlim=c(0,1))
TE1fitZI = fitdist(TE1[which(TE1>0)],distr="beta")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE1fitZI$estimate[1],TE1fitZI$estimate[2]))
hist(TE2[which(TE2>0)],freq=F,breaks=20,xlab="Transmission Efficiency",main="1.5 <= log10[Gametocyte] < 2",ylim=c(0,8))
TE2fitZI = fitdist(TE2[which(TE2>0)],distr="beta","mme")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE2fitZI$estimate[1],TE2fitZI$estimate[2]))
hist(TE3[which(TE3>0)],freq=F,breaks=20,xlab="Transmission Efficiency",main="2 <= log10[Gametocyte] < 2.5",ylim=c(0,8))
TE3fitZI = fitdist(TE3[which(TE3>0)],distr="beta","mme")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE3fitZI$estimate[1],TE3fitZI$estimate[2]))
hist(TE4[which(TE4>0)],freq=F,breaks=20,xlab="Transmission Efficiency",main="2.5 <= log10[Gametocyte] < 3",ylim=c(0,8))
TE4fitZI = fitdist(TE4[which(TE4>0)],distr="beta","mme")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE4fitZI$estimate[1],TE4fitZI$estimate[2]))
hist(TE5[which(TE5>0)],freq=F,breaks=20,xlab="Transmission Efficiency",main="3 <= log10[Gametocyte] < 3.5",ylim=c(0,8))
TE5fitZI = fitdist(TE5[which(TE5>0)],distr="beta","mme")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE5fitZI$estimate[1],TE5fitZI$estimate[2]))
hist(TE6[which(TE6>0)],freq=F,breaks=20,xlab="Transmission Efficiency",main="3.5 <= log10[Gametocyte] < 4",ylim=c(0,8)) 
TE6fitZI = fitdist(TE6[which(TE6>0)],distr="beta","mme")
lines(seq(0,1,.01),dbeta(seq(0,1,.01),TE6fitZI$estimate[1],TE6fitZI$estimate[2]))

alphas = c(TE1fitZI$estimate[1],TE2fitZI$estimate[1],TE3fitZI$estimate[1],TE4fitZI$estimate[1],TE5fitZI$estimate[1],TE6fitZI$estimate[1])
betas = c(TE1fitZI$estimate[2],TE2fitZI$estimate[2],TE3fitZI$estimate[2],TE4fitZI$estimate[2],TE5fitZI$estimate[2],TE6fitZI$estimate[2])
par(mfrow=c(1,1))
ZI = c(sum(TE1==0)/length(TE1),sum(TE2==0)/length(TE2),sum(TE3==0)/length(TE3),sum(TE4==0)/length(TE4),sum(TE5==0)/length(TE5),sum(TE6==0)/length(TE6))
xx = c(1.25,1.75,2.25,2.75,3.25,3.75)
sigfitZI = nls(ZI~p1*exp(p2*xx)/(p3+exp(p2*xx)),start=list(p1=.7,p2=-2,p3=.1))
plot(xx,ZI,xlab="Log10 Gametocytemia",ylab="Zero-Inflation in TE",ylim=c(0,.8))
tt = seq(1,4,.1)
lines(tt,sigmoidTE(tt,.8,-1.6,.029))

ZIsmooth = function(x,dx){
  output = rep(0,length(x))
  for(i in 1:length(x)){
    temp = Mosq[which(log10(G)>=x[i]-dx & log10(G)<x[i]+dx & !is.na(Mosq))]
    output[i] = sum(temp==0)/length(temp)
  }
  return(output)
}
plot(seq(1,4,.1),ZIsmooth(seq(1,4,.1),.2))

xx = seq(1.9,4,.1)
ZIy = ZIsmooth(xx,.2)
sigfitZI = nls(ZIy~p1*exp(p2*xx)/(p3+exp(p2*xx)),start=list(p1=.6,p2=-4,p3=.01))
lines(seq(1.8,4,.01),sigmoidTE(seq(1.8,4,.01),.591,-3.638,.0000134))
