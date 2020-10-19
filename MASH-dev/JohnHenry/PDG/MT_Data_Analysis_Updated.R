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

plot(rowMeans(TE,na.rm=T)[1:200],type="l")
plot(rowMeans(log10(UTG),na.rm=T)[1:200],rowMeans(TE,na.rm=T)[1:200])

plot(log10(G),Mosq,xlim=c(.5,4.5),xlab="Log10 Gametocytemia",ylab="Transmission Efficiency")


TEfit = function(x){
  temp = rep(0,length(x)-1)
  weight = temp
  for(i in 1:length(x)){
    temp[i] = mean(Mosq[which(log10(G)>=(x[i]-.25) & log10(G)<(x[i]+.25))],na.rm=T,weight=dnorm(log10(G)[which(log10(G)>=(x[i]-.25) & log10(G)<(x[i]+.25))]-x[i],x[i],.25))
    weight[i] = length(Mosq[which(log10(G)>=(x[i]-.25) & log10(G)<(x[i]+.25))])
  }
  output = list(TE = temp/100, weight = weight)
  return(output)
}

x = seq(.5,4.5,.1)
SmoothTE = TEfit(x)
sigfitTE = nls(SmoothTE$TE~p1*exp(p2*x)/(p3+exp(p2*x)),start=list(p1=.6,p2=2,p3=100),weights=SmoothTE$weight)
sigfitTE
p1 = .653
p2 = 2.228
p3=146.672
sigTE = function(x,p1,p2,p3){
  p1*exp(p2*x)/(p3+exp(p2*x))
}
plot(x,SmoothTE$TE,xlab="Smoothed Log10 Gametocytemia",ylab="Transmission Efficiency")
lines(x,sigTE(x,p1,p2,p3),col="green")

plot(log10(G),Mosq/100,xlim=c(.5,4.5),xlab="Log10 Gametocytemia",ylab="Transmission Efficiency",cex=.5,main="Transmission Efficiency Predicted by Gametocytemia")
points(x,SmoothTE$TE,xlab="Smoothed Log10 Gametocytemia",ylab="Transmission Efficiency",pch=19,col="blue")
lines(x,sigTE(x,p1,p2,p3),col="dark green",lwd=5)
abline(h=p1,lty=2,col="dark green",lwd=3)

plot(rowMeans(TE,na.rm=T)[1:250]/100)
lines(sigTE(log10(rowMeans(GG,na.rm=T))[1:250],p1,p2,p3),col="dark green")
lines(sigTE(rowMeans(log10(GG),na.rm=T)[1:250],p1,p2,p3),col="dark green")
