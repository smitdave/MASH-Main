PRISM <- read.csv('PRISM_Data.csv',header=TRUE)

## Parasite Density Detected
PD = PRISM$parsdens
## Detection by Microscopy (Binary)
microDICH = PRISM$microDICH
BinMicro = rep(0,length(microDICH))
BinMicro[which(microDICH=='Positive')] = 1

Hb = PRISM$hb
max(Hb,na.rm=T)
Hb[which(Hb>25)] = NaN


plot(log10(PD),Hb,ylim=c(0,20),xlab="Parasite Denisty",ylab="Hemoglobin Reading")
abline(h=12)
abline(h=10)
abline(h=7)

muHb = rep(0,35)
for(i in 0:34){
  low = 2+.1*i
  high = low+.1
  Hbi = Hb[intersect(which(log10(PD)>=low),which(log10(PD)<high))]
  muHb[i+1] = mean(Hbi,na.rm=T)
}
ss = seq(2,5.4,.1)
plot(ss,muHb,xlab="Log10 Parasite Density",ylab="Hb Measurement",main="Hemoglobin as a Function of Parasitemia")
lm(muHb~ss)
lines(ss,-.1304*ss+10.3588)
abline(h=10,lty=2)




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

## % with some form of anemia
pnorm(12,mean=healthyMu,sd=healthySd)

## % with mild anemia
pnorm(12,mean=healthyMu,sd=healthySd)-pnorm(10,mean=healthyMu,sd=healthySd)
## % with moderate anemia
pnorm(10,mean=healthyMu,sd=healthySd)-pnorm(7,mean=healthyMu,sd=healthySd)
## % with severe anemia
pnorm(7,mean=healthyMu,sd=healthySd)


## % with some form of anemia
pnorm(12,mean=infectedMu,sd=infectedSd)

## % with mild anemia
pnorm(12,mean=infectedMu,sd=infectedSd)-pnorm(10,mean=infectedMu,sd=infectedSd)
## % with moderate anemia
pnorm(10,mean=infectedMu,sd=infectedSd)-pnorm(7,mean=infectedMu,sd=infectedSd)
## % with severe anemia
pnorm(7,mean=infectedMu,sd=infectedSd)

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







plot(log10(PRISM$parsdens),PRISM$temp)
abline(h=37)


plot(log10(PRISM$parsdens),PRISM$hospmal)

plot(log10(PRISM$parsdens),PRISM$hospnomal)



plot(log10(PRISM$parsdens),PRISM$hospmalreason=='Cerbral malaria')


plot(PRISM$age,Hb)
abline(h=c(7,10,12))


############## Age-Sex Hb relationship, Healthy

par(mfrow=c(2,2))

healthyFemaleAge = PRISM$age[intersect(which(PRISM$microDICH=="Negative"),which(PRISM$gender=="Female"))]
healthyFemaleHb = PRISM$hb[intersect(which(PRISM$microDICH=="Negative"),which(PRISM$gender=="Female"))]
plot(healthyFemaleAge,healthyFemaleHb,xlim=c(0,10),ylim=c(4,18),main="Healthy Female Hb",xlab="Age",ylab="Hb Concentration (g/dl)")
abline(h=c(7,10,12))

healthyMaleAge = PRISM$age[intersect(which(PRISM$microDICH=="Negative"),which(PRISM$gender=="Male"))]
healthyMaleHb = PRISM$hb[intersect(which(PRISM$microDICH=="Negative"),which(PRISM$gender=="Male"))]
##need to remove 3 data points which are obviously erroneous
healthyMaleAge[which(healthyMaleHb>30)]=NaN
healthyMaleHb[which(healthyMaleHb>30)]=NaN
plot(healthyMaleAge,healthyMaleHb,xlim=c(0,10),ylim=c(4,18),main="Healthy Male Hb",xlab="Age",ylab="Hb Concentration (g/dl)")
abline(h=c(7,10,12))

############ Age-Sex Hb relationship, Infected

infectedFemaleAge = PRISM$age[intersect(which(PRISM$microDICH=="Positive"),which(PRISM$gender=="Female"))]
infectedFemaleHb = PRISM$hb[intersect(which(PRISM$microDICH=="Positive"),which(PRISM$gender=="Female"))]
plot(infectedFemaleAge,infectedFemaleHb,xlim=c(0,10),ylim=c(4,18),main="Infected Female Hb",xlab="Age",ylab="Hb Concentration (g/dl)")
abline(h=c(7,10,12))

infectedMaleAge = PRISM$age[intersect(which(PRISM$microDICH=="Positive"),which(PRISM$gender=="Male"))]
infectedMaleHb = PRISM$hb[intersect(which(PRISM$microDICH=="Positive"),which(PRISM$gender=="Male"))]
##need to remove 1 data points which is obviously erroneous (millions of mg of Hb)
infectedMaleAge[which(infectedMaleHb>30)]=NaN
infectedMaleHb[which(infectedMaleHb>30)]=NaN
plot(infectedMaleAge,infectedMaleHb,xlim=c(0,10),ylim=c(4,18),main="Infected Male Hb",xlab="Age",ylab="Hb Concentration (g/dl)")
abline(h=c(7,10,12))

par(mfrow=c(1,1))





############ fitting by age in healthy individuals
## average by month for 8 years
healthyFemaleHbmu = rep(NaN,(12*8+1))
healthyMaleHbmu = rep(NaN,(12*8+1))
infectedFemaleHbmu = rep(NaN,(12*8+1))
infectedMaleHbmu = rep(NaN,(12*8+1))
for(i in 1:(12*8+1)){
  agemin = (i-1)/12
  agemax = agemin+1/12
  healthyFemaleHbmu[i] = mean(healthyFemaleHb[which(healthyFemaleAge>agemin & healthyFemaleAge<=agemax)],na.rm=T)
  healthyMaleHbmu[i] = mean(healthyMaleHb[which(healthyMaleAge>agemin & healthyMaleAge<=agemax)],na.rm=T)
  infectedFemaleHbmu[i] = mean(infectedFemaleHb[which(infectedFemaleAge>agemin & infectedFemaleAge<=agemax)],na.rm=T)
  infectedMaleHbmu[i] = mean(infectedMaleHb[which(infectedMaleAge>agemin & infectedMaleAge<=agemax)],na.rm=T)
}
age = seq(0,8,1/12)

par(mfrow=c(1,2))
plot(age,healthyFemaleHbmu,type="l",ylim=c(9,13),ylab="Mean Hb Concentration (g/dl)",xlab="age (in years)",main="Mean Hb Healthy Vs Infected Females")
lines(age,infectedFemaleHbmu,col="red")
plot(age,healthyMaleHbmu,type="l",ylim=c(9,13),ylab="Mean Hb Concentration (g/dl)",xlab="age (in years)",main="Mean Hb Healthy Vs Infected Males")
lines(age,infectedMaleHbmu,type="l",col="red")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plot(age,healthyFemaleHbmu-infectedFemaleHbmu,type="l",main="Difference in Female Mean Hb by Infection Status",xlab="age",ylab="Hb Concentration, g/dl",ylim=c(-2,3))
abline(h=mean(healthyFemaleHbmu-infectedFemaleHbmu,na.rm=T))
plot(age,healthyMaleHbmu-infectedMaleHbmu,type="l",main="Difference in Male Mean Hb by Infection Status",xlab="age",ylab="Hb Concentration, g/dl",ylim=c(-2,3))
abline(h=mean(healthyMaleHbmu-infectedMaleHbmu,na.rm=T))
par(mfrow=c(1,1))

