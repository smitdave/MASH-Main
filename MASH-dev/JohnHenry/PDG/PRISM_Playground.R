PRISM <- read.csv('PRISM_Data.csv',header=TRUE)

## Parasite Density Detected
PD = PRISM$parsdens
## Detection by Microscopy (Binary)
microDICH = PRISM$microDICH
BinMicro = rep(0,length(microDICH))
BinMicro[which(microDICH=='Positive')] = 1

min(PD[which(PD>10)],na.rm=T)

Hb = PRISM$hb
max(Hb,na.rm=T)
Hb[which(Hb>25)] = NaN

plot(log10(PD),Hb,ylim=c(0,20),xlab="Parasite Denisty",ylab="Hemoglobin Reading")

HbHealthy = Hb[intersect(which(Hb>0),which(BinMicro==0))]
HbInfected = Hb[intersect(which(Hb>0),which(BinMicro==1))]

par(mfrow=c(2,1))
t = seq(0,20,.01)
hist(HbHealthy,freq=F,xlim=c(5,15),breaks=30,ylim=c(0,.4))
healthyFit = fitdist(HbHealthy,distr="norm",method="mle")
healthyMu = healthyFit$estimate[1]
healthySd = healthyFit$estimate[2]
lines(t,dnorm(t,healthyMu,healthySd))
abline(v=12)
abline(v=10)
abline(v=7)
hist(HbInfected,freq=F,xlim=c(5,15),breaks=20,ylim=c(0,.4))
infectedFit = fitdist(HbInfected,distr="norm",method="mle")
infectedMu = infectedFit$estimate[1]
infectedSd = infectedFit$estimate[2]
lines(t,dnorm(t,infectedMu,infectedSd))
abline(v=12)
abline(v=10)
abline(v=7)
par(mfrow=c(1,1))

plot(t,dnorm(t,healthyMu,healthySd),type="l",xlim=c(4,16),ylim=c(0,.35),xlab="Measured Hb",ylab="Probability Density",main="Hb Measurements Stratified by Infection Status")
lines(t,dnorm(t,infectedMu,infectedSd),col="red")


################# Stratify by Sex

BinSex = as.numeric(PRISM$gender=="Male")
healthyMaleHb = HbHealthy[which(BinSex==1)]
healthyFemaleHb = HbHealthy[which(BinSex==0)]
infectedMaleHb = HbInfected[which(BinSex==1)]
infectedFemaleHb = HbInfected[which(BinSex==0)]

par(mfrow=c(2,2))
hist(healthyMaleHb,freq=F,xlim=c(5,15),ylim=c(0,.35),breaks=30)
abline(v=12)
abline(v=9.9)
abline(v=7)
hist(healthyFemaleHb,freq=F,xlim=c(5,15),ylim=c(0,.35),breaks=30)
abline(v=12)
abline(v=9.9)
abline(v=7)
hist(infectedMaleHb,freq=F,xlim=c(5,15),ylim=c(0,.35),breaks=30)
abline(v=12)
abline(v=9.9)
abline(v=7)
hist(infectedFemaleHb,freq=F,xlim=c(5,15),ylim=c(0,.35),breaks=30)
abline(v=12)
abline(v=9.9)
abline(v=7)
par(mfrow=c(1,1))

maleHb = Hb[which(BinSex==1)]
femaleHb = Hb[which(BinSex==0)]

##Difference by Infection Status in Males
t.test(maleHb~BinMicro[which(BinSex==1)])

##Difference by Infection Status in Females
t.test(femaleHb~BinMicro[which(BinSex==0)])

##Difference by Sex, Healthy
##SS diff, by (.1,.19)
t.test(HbHealthy~BinSex[intersect(which(Hb>0),which(BinMicro==0))])

##Difference by Sex, Infected
##No SS diff
t.test(HbInfected~BinSex[intersect(which(Hb>0),which(BinMicro==1))])
