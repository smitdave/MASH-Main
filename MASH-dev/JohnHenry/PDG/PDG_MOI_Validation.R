########################## Testing PDG Superinfection
########################## To test that simulated superinfections are statistically consistent
########################## with the assumption of independent infection dynamics, we first add
########################## each independent trajectory pairwise to create data consistent with
########################## this assumption. Then, simulate MOI=2 infections. Compare histograms
########################## of output and related summary statistics


########################## Part 1: pairwise additive data.

MT_PT_NP = read_excel("~/GitHub/MASH-Main/MASH-Main/MASH-dev/JohnHenry/PDG/MT_PT_NP.xlsx")
M = as.matrix(MT_PT_NP)
PtData = matrix(0,nrow=52,ncol=334)

for(j in 1:334){
  for(i in 1:52){
    PtData[i,j] = mean(M[((i-1)*14+1):(i*14),j],na.rm=T)
  }
}

hist(log10(PtData[1,]))


PtPaired = matrix(0,nrow=52,ncol=334*335/2)
index = 1
for(i in 1:334){
  temp = 0
  for(j in i:334){
    PtPaired[,index+temp] = PtData[,i]+PtData[,j]
    temp = temp+1
  }
  index = index+temp
}

LogPtPaired = log10(PtPaired[,])
LogPtPaired[which(is.infinite(LogPtPaired))]=NaN
hist(LogPtPaired[1,],freq=F,ylim=c(0,1.5),xlim=c(0,6),breaks=10)

vioplot(LogPtPaired[1,],LogPtPaired[2,],LogPtPaired[3,],LogPtPaired[4,],LogPtPaired[5,],LogPtPaired[6,],LogPtPaired[7,],LogPtPaired[8,])


NData = rep(0,334)

for(i in 1:334){
  position = which(!is.na(M[,i]))
  nonzero = which(M[,i]>0)
  NData[i] = max(intersect(position,nonzero))
}
hist(NData,freq=F)
### Note here that the distribution is approximately exponentially distributed
NDataFortnight = NData/14
hist(ceiling(NDataFortnight),freq=F)
expfit = fitdist(ceiling(NDataFortnight),method="mle",distr="exp")
rate = .1285
t = seq(0,30,.1)
lines(t,dexp(t,rate=rate))


NPairedFortnight = rep(0,334*335/2)
index = 1
for(i in 1:334){
  temp = 0
  for(j in i:334){
    NPairedFortnight[index+temp] = max(NDataFortnight[i],NDataFortnight[j])
    temp = temp+1
  }
  index = index+temp
}

hist(NPairedFortnight,freq=F,main="Histogram of Duration of Patent Infection")
### Note here that the distribution is approximately gamma distributed, as expected from the sum of exponential distributions
gamfit = fitdist(ceiling(NPairedFortnight),method="mle",distr="gamma")
gshape = 2.443
grate = .2174
t = seq(0,30,.1)
lines(t,dgamma(t,shape=gshape,rate=grate))
#lines(t,dgamma(t,shape=2,rate=3/2*rate),lty=3)

######################### Part 2: simulate synthetic data of MOI=2

P = 334
Years = 20
Fortnights = Years*26
Pt = matrix(0,nrow=Fortnights,ncol=P)

for(j in 1:P){
  
  human = PDGHuman$new()
  human$infect_Human(2)
  
  for(i in 1:Fortnights){
    human$update_Human()
  }
  
  Pt[,j] = human$get_history()$Pt
  
}

hist(Pt[1,],freq=F)

Pt[which(Pt==0)]=NaN

############# Part 3: Compare Output to Pairwise Data

par(mfrow=c(1,2))
vioplot(LogPtPaired[1,],LogPtPaired[2,],LogPtPaired[3,],LogPtPaired[4,],LogPtPaired[5,],LogPtPaired[6,],LogPtPaired[7,],LogPtPaired[8,],LogPtPaired[9,],ylim=c(0,6))
vioplot(Pt[1,],Pt[2,],Pt[3,],Pt[4,],Pt[5,],Pt[6,],Pt[7,],Pt[8,],Pt[9,],ylim=c(0,6))
par(mfrow=c(1,1))


Active = Pt>2
Dur = rep(0,P)
for(i in 1:P){
  Dur[i] = max(which(Active[,i]==T))-1
}

Dur[which(Dur>30)]=NaN
par(mfrow=c(1,2))
hist(Dur,freq=F,breaks=15,xlab="Fortnights Since Infection",main="Histogram of the Duration of 2 Infections",xlim=c(0,30),ylim=c(0,.08))
DurDist = fitdist(Dur,method="mme",distr="gamma")
t = seq(0,30,.1)
dshape = DurDist$estimate[1]
drate = DurDist$estimate[2]
lines(t,dgamma(t,dshape,drate))
abline(v=mean(Dur))

hist(ceiling(NPairedFortnight),freq=F,main="Histogram of Duration of Patent Infection",ylim=c(0,.08))
lines(t,dgamma(t,shape=gshape,rate=grate))
abline(v=gshape/grate)
par(mfrow=c(1,1))

plot(t,dgamma(t,dshape,drate),type="l",ylim=c(0,.07))
lines(t,dgamma(t,gshape,grate))

