########################## Testing PDG Superinfection
########################## To test that simulated superinfections are statistically consistent
########################## with the assumption of independent infection dynamics, we first add
########################## each independent trajectory pairwise to create data consistent with
########################## this assumption. Then, simulate MOI=2 infections. Compare histograms
########################## of output and related summary statistics



########################## Part 1: pairwise additive data (MOI = 2)



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
hist(NData,freq=F,breaks=30)
expfitdaily = fitdist(NData,method="mle",distr="exp")
ratedaily = .0097
### Note here that the distribution is approximately exponentially distributed
NDataFortnight = NData/14
hist(ceiling(NDataFortnight),freq=F,breaks=30)
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



P = 5000
Years = 2
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



hist(Pt[1,],xlim=c(0,5.5))
par(mfrow=c(1,2))
vioplot(LogPtPaired[1,],LogPtPaired[2,],LogPtPaired[3,],LogPtPaired[4,],LogPtPaired[5,],LogPtPaired[6,],LogPtPaired[7,],LogPtPaired[8,],LogPtPaired[9,],ylim=c(0,6),main="Malaria Therapy")
vioplot(Pt[1,],Pt[2,],Pt[3,],Pt[4,],Pt[5,],Pt[6,],Pt[7,],Pt[8,],Pt[9,],ylim=c(0,6),main="PDG Sim")
par(mfrow=c(1,1))

Active = Pt>log10(150)## defining average point of "reliable" detectability by microscopy
Dur = rep(0,P)
for(i in 1:P){
  Dur[i] = max(which(Active[,i]==T))-1
}

Dur[which(Dur>30)]=NaN
par(mfrow=c(1,2))
hist(Dur,freq=F,breaks=15,xlab="Fortnights Since Infection",main="Histogram of the Duration of 2 Simulated Infections",xlim=c(0,30),ylim=c(0,.1))
DurDist = fitdist(Dur,method="mme",distr="gamma")
t = seq(0,30,.1)
dshape = DurDist$estimate[1]
drate = DurDist$estimate[2]
lines(t,dgamma(t,dshape,drate))
abline(v=mean(Dur,na.rm=T))

hist(ceiling(NPairedFortnight),freq=F,main="Histogram of Duration of 2 Patent Infections",ylim=c(0,.1),breaks=20)
lines(t,dgamma(t,shape=gshape,rate=grate))
abline(v=gshape/grate)
par(mfrow=c(1,1))

plot(t,dgamma(t,dshape,drate),type="l",ylim=c(0,.1),col="blue")
abline(v = mean(Dur,na.rm=T),col="blue")
lines(t,dgamma(t,gshape,grate))
abline(v = gshape/grate)



####################### ********* Repeat with MOI = 3 **********

####################### Part 4: Iteratively add infections, examine distribution (MOI = 3)



MT_PT_NP = read_excel("~/GitHub/MASH-Main/MASH-Main/MASH-dev/JohnHenry/PDG/MT_PT_NP.xlsx")
M = as.matrix(MT_PT_NP)
PtData = matrix(0,nrow=52,ncol=334)

for(j in 1:334){
  for(i in 1:52){
    PtData[i,j] = mean(M[((i-1)*14+1):(i*14),j],na.rm=T)
  }
}


PtTriplet = matrix(0,nrow=52,ncol=choose(334+3-1,3))
index = 1
for(i in 1:334){
  for(j in i:334){
    for(k in j:334){
      PtTriplet[,index] = PtData[,i]+PtData[,j]+PtData[,k]
      index = index+1
    }
  }
}

hist(log10(PtTriplet[10,]),freq=F)

LogPtTriplet = log10(PtTriplet)
LogPtTriplet[which(is.infinite(LogPtTriplet))]=NaN
hist(LogPtTriplet[1,],freq=F,ylim=c(0,1.5),xlim=c(0,6),breaks=15)

vioplot(LogPtTriplet[1,],LogPtTriplet[2,],LogPtTriplet[3,],LogPtTriplet[4,],LogPtTriplet[5,],LogPtTriplet[6,],LogPtTriplet[7,],LogPtTriplet[8,])


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


NTripletFortnight = rep(0,choose(334+3-1,3))
index = 1
for(i in 1:334){
  for(j in i:334){
    for(k in j:334){
      NTripletFortnight[index] = max(NDataFortnight[i],NDataFortnight[j],NDataFortnight[k])
      index = index+1
    }
  }
}

hist(NTripletFortnight,freq=F,main="Histogram of Duration of Patent Infection, MOI=3")
### Note here that the distribution is approximately gamma distributed, as expected from the sum of exponential distributions
gamfitTriplet = fitdist(ceiling(NTripletFortnight),method="mle",distr="gamma")
gshape3 = 3.6156
grate3 = .2695
t = seq(0,30,.1)
lines(t,dgamma(t,shape=gshape3,rate=grate3))




################# Part 5: create synthetic data, MOI=3



P = 5000
Years = 2
MOI = 3
Fortnights = Years*26
Pt = matrix(0,nrow=Fortnights,ncol=P)

for(j in 1:P){
  
  human = PDGHuman$new()
  human$infect_Human(MOI)
  
  for(i in 1:Fortnights){
    human$update_Human()
  }
  
  Pt[,j] = human$get_history()$Pt
  
}

hist(Pt[1,],freq=F)

Pt[which(Pt==0)]=NaN



################## Part 6: Comparison, Take 2 (...or should I say Take 3?)

hist(Pt[1,]/50*100,freq=F,main="Histogram of the Mean % Parasitemia in the First Fortnight",xlab="Percent Parasitemia",ylab="Probability Density")

hist(Pt[1,],xlim=c(0,5.5))
LPtSample = matrix(NaN,nrow = 52,ncol = 100000)
LPtSample = LogPtTriplet[,sample(1:choose(334+3-1,3),5000)]
par(mfrow=c(1,2))
vioplot(LPtSample[1,],LPtSample[2,],LPtSample[3,],LPtSample[4,],LPtSample[5,],LPtSample[6,],LPtSample[7,],LPtSample[8,],LPtSample[9,],ylim=c(0,6),main="Malaria Therapy, MOI=3")
vioplot(Pt[1,],Pt[2,],Pt[3,],Pt[4,],Pt[5,],Pt[6,],Pt[7,],Pt[8,],Pt[9,],ylim=c(0,6),main="PDG Sim, MOI=3")
par(mfrow=c(1,1))


par(mfrow=c(1,2))
hist(NTripletFortnight,freq=F,main="Histogram of Duration of Patent Infection, MOI=3")
gshape3 = 3.6156
grate3 = .2695
t = seq(0,30,.1)
lines(t,dgamma(t,shape=gshape3,rate=grate3))
Active = Pt>log10(80)
## active defines those which are **possibly** detectable by microscopy, using the empirical lower
## bound of ~about~ 80 per microliter. However, we need to continue to process this through
## a bernoulli filter with a probability of detection an increasing function of parasite density
sigmoid = function(Pt,Half,slope,max,min){
  (max-min)*(Pt/Half)^slope/(1+(Pt/Half)^slope)+min
}
threshDetec = log10(80)
pmicro = matrix(NaN,nrow=52,ncol=5000)
for(i in 1:52){
  pmicro[i,which(Pt[i,]>threshDetec)] = sigmoid(Pt[i,which(Pt[i,]>threshDetec)],Half=log10(150),slope=10,max=1,min=0)
}
plot(rowMeans(pmicro,na.rm=T),type="l",ylim=c(0,1),main="Probability of Detection By Microscopy, Given Theoretically Detectable Parasitemia",xlab="Fortnights Since Infection",ylab="Probability of Detection")

Dur = rep(0,P)
for(i in 1:P){
  Dur[i] = max(which(Active[,i]==T))-1
}
hist(Dur,freq=F,main="Histogram of Duration of Simulated Patent Infection, MOI=3",breaks=30)
gamDur = fitdist(Dur,distr="gamma",method="mme")
shape3sim = gamDur$estimate[1]
rate3sim = gamDur$estimate[2]
lines(t,dgamma(t,shape=shape3sim,rate=rate3sim))
par(mfrow=c(1,1))


plot(t,dgamma(t,shape=gshape3,rate=grate3),type="l",ylim=c(0,.1),xlab="Duration (Fortnights)",ylab="Probability Density",main="Fitted Distributions for Duration of Patent Infection, MOI=3")
abline(v=gshape3/grate3)
lines(t,dgamma(t,shape=shape3sim,rate=rate3sim),col="blue")
abline(v=shape3sim/rate3sim,col="blue")



