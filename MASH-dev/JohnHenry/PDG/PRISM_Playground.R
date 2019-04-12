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


plot(log10(PD),Hb,ylim=c(5,15),xlab="Parasite Denisty",ylab="Hemoglobin Reading")
abline(h=c(7,10,11))

muHb = rep(0,140)
for(i in 0:139){
  low = 2+.025*i
  high = low+.1
  Hbi = Hb[intersect(which(log10(PD)>=low),which(log10(PD)<high))]
  muHb[i+1] = mean(Hbi,na.rm=T)
}
ss = seq(2,5.475,.025)
plot(ss,muHb,xlab="Log10 Parasite Density",ylab="Hb Measurement",main="Hemoglobin as a Function of Parasitemia")

lm(muHb~ss)
lines(ss,-.135*ss+10.381)
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
abline(v=c(7,10,11))
hist(HbInfected,freq=F,xlim=c(5,15),breaks=20,ylim=c(0,.4))
infectedFit = fitdist(HbInfected,distr="norm",method="mle")
infectedMu = infectedFit$estimate[1]
infectedSd = infectedFit$estimate[2]
lines(t,dnorm(t,infectedMu,infectedSd))
abline(v=c(7,10,11))
par(mfrow=c(1,1))

## % with some form of anemia
pnorm(11,mean=healthyMu,sd=healthySd)

## % with mild anemia
pnorm(11,mean=healthyMu,sd=healthySd)-pnorm(10,mean=healthyMu,sd=healthySd)
## % with moderate anemia
pnorm(10,mean=healthyMu,sd=healthySd)-pnorm(7,mean=healthyMu,sd=healthySd)
## % with severe anemia
pnorm(7,mean=healthyMu,sd=healthySd)


## % with some form of anemia
pnorm(11,mean=infectedMu,sd=infectedSd)

## % with mild anemia
pnorm(11,mean=infectedMu,sd=infectedSd)-pnorm(10,mean=infectedMu,sd=infectedSd)
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
abline(v=c(7,10,11))
hist(healthyFemaleHb,freq=F,xlim=c(5,15),ylim=c(0,.35),breaks=30)
abline(v=c(7,10,11))
hist(infectedMaleHb,freq=F,xlim=c(5,15),ylim=c(0,.35),breaks=30)
abline(v=c(7,10,11))
hist(infectedFemaleHb,freq=F,xlim=c(5,15),ylim=c(0,.35),breaks=30)
abline(v=c(7,10,11))
par(mfrow=c(1,1))

maleHb = Hb[which(BinSex==1)]
femaleHb = Hb[which(BinSex==0)]

##Difference by Infection Status in Males
t.test(maleHb~BinMicro[which(BinSex==1)])

##Difference by Infection Status in Females
t.test(femaleHb~BinMicro[which(BinSex==0)])

############################### This is interesting - looking at the aggregate
############################### data, it looks like females have a significant
############################### lower baseline when healthy, but not when infected.
############################### This difference changes when age is taken into account,
############################### that is healthy males and females are identical but
############################### females are (slightly) more anemic in response to parasites
##Difference by Sex, Healthy
##SS diff, by (.107,.185)
t.test(HbHealthy~BinSex[intersect(which(Hb>0),which(BinMicro==0))])

##Difference by Sex, Infected
##No SS diff
t.test(HbInfected~BinSex[intersect(which(Hb>0),which(BinMicro==1))])
###################################

t.test(Hb~BinMicro)




########### Fever, Health-Seeking Behaviors
plot(log10(PRISM$parsdens),PRISM$temp,xlab="log10 Parasite Density",ylab="Temperature (Degrees Celsius)",main="Fever Signal from Parasite Density")
abline(h=37)

plot(log10(PRISM$parsdens),PRISM$hospnomal)

plot(log10(PRISM$parsdens),PRISM$hospmal,xlab="log10 Parasite Densities",ylab="Proportion Seeking Treatment for Malaria",main="Treatment Seeking as a Function of Parasite Density")

## which have recorded temperatures & sought treatment
tempHS = intersect(which(as.numeric(PRISM$hospital)!=1),which(!is.na(PRISM$temp)))
SeekTreatment = as.numeric(PRISM$hospmal[tempHS])-1
plot(PRISM$temp[tempHS],SeekTreatment)
abline(v=c(37,38,39))

##probability of seaking treatment as a function of temperature
PST = rep(0,17)
for(i in 1:17){
  low = 36+(i-1)*.25
  high = low+.25
  interval = which(PRISM$temp[tempHS]>=low & PRISM$temp[tempHS]<=high)
  PST[i] = mean(SeekTreatment[interval],na.rm=T)
}

tempSeq = seq(36,40,.25)
plot(tempSeq,PST,xlab="Temperature (Degrees Celsius)",ylab="Probability of Seeking Treatment",main="Treatment Seeking Behavior and Fever")

## Fitted with sigmoid function; not sure this is a great fit, very noisy and sparse data...
nls(PST~max*exp(slope*tempSeq)/(exp(slope*tempSeq)+exp(slope*half)),start=list(slope=1,half=38,max=.7))
Tslope = .8838
Thalf = 36.9450
Tmax = .6222
lines(tempSeq,Tmax*exp(Tslope*tempSeq)/(exp(Tslope*tempSeq)+exp(Tslope*Thalf)))
abline(v=c(37,38,39),lty=2)

plot(log10(PRISM$parsdens),PRISM$hospmalreason=='Cerbral malaria')


plot(PRISM$age,Hb)
abline(h=c(7,10,12))


############## Age-Sex Hb relationship, Healthy

par(mfrow=c(2,2))

healthyFemaleAge = PRISM$age[intersect(which(PRISM$microDICH=="Negative"),which(PRISM$gender=="Female"))]
healthyFemaleHb = PRISM$hb[intersect(which(PRISM$microDICH=="Negative"),which(PRISM$gender=="Female"))]
plot(healthyFemaleAge,healthyFemaleHb,xlim=c(0,10),ylim=c(4,18),main="Healthy Female Hb",xlab="Age",ylab="Hb Concentration (g/dl)")
abline(h=c(7,10,11))

healthyMaleAge = PRISM$age[intersect(which(PRISM$microDICH=="Negative"),which(PRISM$gender=="Male"))]
healthyMaleHb = PRISM$hb[intersect(which(PRISM$microDICH=="Negative"),which(PRISM$gender=="Male"))]
##need to remove 3 data points which are obviously erroneous
healthyMaleAge[which(healthyMaleHb>30)]=NaN
healthyMaleHb[which(healthyMaleHb>30)]=NaN
plot(healthyMaleAge,healthyMaleHb,xlim=c(0,10),ylim=c(4,18),main="Healthy Male Hb",xlab="Age",ylab="Hb Concentration (g/dl)")
abline(h=c(7,10,11))

############ Age-Sex Hb relationship, Infected

infectedFemaleAge = PRISM$age[intersect(which(PRISM$microDICH=="Positive"),which(PRISM$gender=="Female"))]
infectedFemaleHb = PRISM$hb[intersect(which(PRISM$microDICH=="Positive"),which(PRISM$gender=="Female"))]
plot(infectedFemaleAge,infectedFemaleHb,xlim=c(0,10),ylim=c(4,18),main="Infected Female Hb",xlab="Age",ylab="Hb Concentration (g/dl)")
abline(h=c(7,10,11))

infectedMaleAge = PRISM$age[intersect(which(PRISM$microDICH=="Positive"),which(PRISM$gender=="Male"))]
infectedMaleHb = PRISM$hb[intersect(which(PRISM$microDICH=="Positive"),which(PRISM$gender=="Male"))]
##need to remove 1 data points which is obviously erroneous (millions of mg of Hb)
infectedMaleAge[which(infectedMaleHb>30)]=NaN
infectedMaleHb[which(infectedMaleHb>30)]=NaN
plot(infectedMaleAge,infectedMaleHb,xlim=c(0,10),ylim=c(4,18),main="Infected Male Hb",xlab="Age",ylab="Hb Concentration (g/dl)")
abline(h=c(7,10,11))

par(mfrow=c(1,1))





############ fitting by age in healthy individuals
## average by month for 8 years
healthyFemaleHbmu = rep(NaN,(12*8+1))
healthyMaleHbmu = rep(NaN,(12*8+1))
infectedFemaleHbmu = rep(NaN,(12*8+1))
infectedMaleHbmu = rep(NaN,(12*8+1))
healthyFemaleHbvar = rep(NaN,(12*8+1))
healthyMaleHbvar = rep(NaN,(12*8+1))
infectedFemaleHbvar = rep(NaN,(12*8+1))
infectedMaleHbvar = rep(NaN,(12*8+1))
healthyFemaleHbN = rep(NaN,(12*8+1))
healthyMaleHbN = rep(NaN,(12*8+1))
infectedFemaleHbN = rep(NaN,(12*8+1))
infectedMaleHbN = rep(NaN,(12*8+1))
for(i in 1:(12*8+1)){
  agemin = (i-1)/12
  agemax = agemin+1/12
  healthyFemaleHbmu[i] = mean(healthyFemaleHb[which(healthyFemaleAge>agemin & healthyFemaleAge<=agemax)],na.rm=T)
  healthyMaleHbmu[i] = mean(healthyMaleHb[which(healthyMaleAge>agemin & healthyMaleAge<=agemax)],na.rm=T)
  infectedFemaleHbmu[i] = mean(infectedFemaleHb[which(infectedFemaleAge>agemin & infectedFemaleAge<=agemax)],na.rm=T)
  infectedMaleHbmu[i] = mean(infectedMaleHb[which(infectedMaleAge>agemin & infectedMaleAge<=agemax)],na.rm=T)
  healthyFemaleHbvar[i] = var(healthyFemaleHb[which(healthyFemaleAge>agemin & healthyFemaleAge<=agemax)],na.rm=T)
  healthyMaleHbvar[i] = var(healthyMaleHb[which(healthyMaleAge>agemin & healthyMaleAge<=agemax)],na.rm=T)
  infectedFemaleHbvar[i] = var(infectedFemaleHb[which(infectedFemaleAge>agemin & infectedFemaleAge<=agemax)],na.rm=T)
  infectedMaleHbvar[i] = var(infectedMaleHb[which(infectedMaleAge>agemin & infectedMaleAge<=agemax)],na.rm=T)
  healthyFemaleHbN[i] = sum((healthyFemaleHb[which(healthyFemaleAge>agemin & healthyFemaleAge<=agemax)]>0),na.rm=T)
  healthyMaleHbN[i] = sum((healthyMaleHb[which(healthyMaleAge>agemin & healthyMaleAge<=agemax)]>0),na.rm=T)
  infectedFemaleHbN[i] = sum((infectedFemaleHb[which(infectedFemaleAge>agemin & infectedFemaleAge<=agemax)]>0),na.rm=T)
  infectedMaleHbN[i] = sum((infectedMaleHb[which(infectedMaleAge>agemin & infectedMaleAge<=agemax)]>0),na.rm=T)
}
age = seq(0,8,1/12)

######### Distribution Snapshot by Age, Sex, and Infection Status

par(mfrow=c(2,2))
hist(healthyFemaleHb[which(healthyFemaleAge<2)],main="Healthy Females Under 2",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(healthyFemaleHb[which(healthyFemaleAge>=2 & healthyFemaleAge<4)],main="Healthy Females Between 2 and 4",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(healthyFemaleHb[which(healthyFemaleAge>=4 & healthyFemaleAge<6)],main="Healthy Females Between 4 and 6",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(healthyFemaleHb[which(healthyFemaleAge>=6 & healthyFemaleAge<8)],main="Healthy Females Between 6 and 8",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(infectedFemaleHb[which(infectedFemaleAge<2)],main="Infected Females Under 2",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(infectedFemaleHb[which(infectedFemaleAge>=2 & infectedFemaleAge<4)],main="Infected Females Between 2 and 4",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(infectedFemaleHb[which(infectedFemaleAge>=4 & infectedFemaleAge<6)],main="Infected Females Between 4 and 6",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(infectedFemaleHb[which(infectedFemaleAge>=6 & infectedFemaleAge<8)],main="Infected Females Between 6 and 8",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(healthyMaleHb[which(healthyMaleAge<2)],main="Healthy Males Under 2",xlim=c(6,15),freq=F,ylim=c(0,.45),breaks=20,xlab="Hb Concentration (g/dl)")
hist(healthyMaleHb[which(healthyMaleAge>=2 & healthyMaleAge<4)],main="Healthy Males Between 2 and 4",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(healthyMaleHb[which(healthyMaleAge>=4 & healthyMaleAge<6)],main="Healthy Males Between 4 and 6",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(healthyMaleHb[which(healthyMaleAge>=6 & healthyMaleAge<8)],main="Healthy Males Between 6 and 8",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(infectedMaleHb[which(infectedMaleAge<2)],main="Infected Males Under 2",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(infectedMaleHb[which(infectedMaleAge>=2 & infectedMaleAge<4)],main="Infected Males Between 2 and 4",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(infectedMaleHb[which(infectedMaleAge>=4 & infectedMaleAge<6)],main="Infected Males Between 4 and 6",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
hist(infectedMaleHb[which(infectedMaleAge>=6 & infectedMaleAge<8)],main="Infected Males Between 6 and 8",xlim=c(6,15),freq=F,ylim=c(0,.45),xlab="Hb Concentration (g/dl)")
par(mfrow=c(1,1))

######### Compare Means
par(mfrow=c(1,2))
plot(age,healthyFemaleHbmu,type="l",ylim=c(9,13),ylab="Mean Hb Concentration (g/dl)",xlab="age (in years)",main="Mean Hb Healthy Vs Infected Females")
lines(age,infectedFemaleHbmu,col="red")
abline(h=c(11,10,7),lty=2)
plot(age,healthyMaleHbmu,type="l",ylim=c(9,13),ylab="Mean Hb Concentration (g/dl)",xlab="age (in years)",main="Mean Hb Healthy Vs Infected Males")
abline(h=c(11,10,7),lty=2)
lines(age,infectedMaleHbmu,type="l",col="red")
par(mfrow=c(1,1))

######### Mean Differences
par(mfrow=c(1,2))
plot(age,healthyFemaleHbmu-infectedFemaleHbmu,type="l",main="Difference in Female Mean Hb by Infection Status",xlab="age",ylab="Hb Concentration, g/dl",ylim=c(-2,3))
abline(h=mean(healthyFemaleHbmu-infectedFemaleHbmu,na.rm=T))
plot(age,healthyMaleHbmu-infectedMaleHbmu,type="l",main="Difference in Male Mean Hb by Infection Status",xlab="age",ylab="Hb Concentration, g/dl",ylim=c(-2,3))
abline(h=mean(healthyMaleHbmu-infectedMaleHbmu,na.rm=T))
par(mfrow=c(1,1))

## two sample t-test of differences in mean
t.test((healthyFemaleHbmu-infectedFemaleHbmu),(healthyMaleHbmu-infectedMaleHbmu))
## no statistically significant difference in the average decline in Hb by sex


######### Compare SDs
par(mfrow=c(1,2))
plot(age,sqrt(healthyFemaleHbvar),type="l",ylim=c(0,3),ylab="SD Hb Concentration (g/dl)",xlab="age (in years)",main="SD Hb Healthy Vs Infected Females")
abline(h=sqrt(mean(healthyFemaleHbvar,na.rm=T,weights=sqrt(healthyFemaleHbN))))
lines(age,sqrt(infectedFemaleHbvar),col="red")
abline(h=sqrt(mean(infectedFemaleHbvar,na.rm=T,weights=sqrt(infectedFemaleHbN))),lty=2)
plot(age,sqrt(healthyMaleHbvar),type="l",ylim=c(0,3),ylab="SD Hb Concentration (g/dl)",xlab="age (in years)",main="SD Hb Healthy Vs Infected Males")
abline(h=sqrt(mean(healthyMaleHbvar,na.rm=T,weights=sqrt(healthyMaleHbN))))
lines(age,sqrt(infectedMaleHbvar),type="l",col="red")
abline(h=sqrt(mean(infectedMaleHbvar,na.rm=T,weights=sqrt(infectedMaleHbN))),lty=2)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(healthyFemaleHbvar,breaks=20)
abline(v=mean(healthyFemaleHbvar,na.rm=T,weights=sqrt(healthyFemaleHbN)))
hist(infectedFemaleHbvar,breaks=20)
abline(v=mean(infectedFemaleHbvar,na.rm=T,weights=sqrt(infectedFemaleHbN)))
hist(healthyMaleHbvar,breaks=20)
abline(v=mean(healthyMaleHbvar,na.rm=T,weights=sqrt(healthyMaleHbN)))
hist(infectedMaleHbvar,breaks=20)
abline(v=mean(infectedMaleHbvar,na.rm=T,weights=sqrt(healthyMaleHbN)))
par(mfrow=c(1,1))

######### Compare Sample Sizes
par(mfrow=c(1,2))
plot(age,log10(healthyFemaleHbN),type="l",ylim=c(0,3),ylab="Sample Sizes of Hb Concentration (g/dl)",xlab="age (in years)",main="Sample Size of Healthy Vs Infected Females")
lines(age,log10(infectedFemaleHbN),col="red")
plot(age,log10(healthyMaleHbN),type="l",ylim=c(0,3),ylab="Sample Sizes of Hb Concentration (g/dl)",xlab="age (in years)",main="Sample Size of Healthy Vs Infected Males")
lines(age,log10(infectedMaleHbN),type="l",col="red")
par(mfrow=c(1,1))


####################### Fit model to Hb Levels by age/sex

ageSub = seq(.5,8,1/12)

mf = function(age,Hb0,a,b){
  a/b*(1-exp(-b*age))+Hb0*exp(-b*age)
}

par(mfrow=c(1,2))
healthyFemaleHbmuSub = healthyFemaleHbmu[which(age>=.5)]
infectedFemaleHbmuSub = infectedFemaleHbmu[which(age>=.5)]
healthyMaleHbmuSub = healthyMaleHbmu[which(age>=.5)]
infectedMaleHbmuSub = infectedMaleHbmu[which(age>=.5)]

healthyFemaleHbNSub = healthyFemaleHbN[which(age>=.5)]
infectedFemaleHbNSub = infectedFemaleHbN[which(age>=.5)]
healthyMaleHbNSub = healthyMaleHbN[which(age>=.5)]
infectedMaleHbNSub = infectedMaleHbN[which(age>=.5)]

nls(healthyFemaleHbmuSub~a/b*(1-exp(-b*ageSub))+Hb0*exp(-b*ageSub),start=list(a=1,b=.2,Hb0=9),weights = sqrt(healthyFemaleHbNSub))
plot(age,healthyFemaleHbmu,type="l",ylim=c(9,13),ylab="Mean Hb Concentration (g/dl)",xlab="Age (in Years)",main="Mean [Hb] in Healthy Vs Infected Females")
aHF = 4.8788
bHF = .4157
Hb0HF = 9.7521
lines(ageSub,mf(ageSub,Hb0HF,aHF,bHF))
lines(age,infectedFemaleHbmu,col="red")
nls(infectedFemaleHbmuSub~a/b*(1-exp(-b*ageSub))+Hb0*exp(-b*ageSub),start=list(a=4,b=.4,Hb0=7.5),weights = sqrt(infectedFemaleHbNSub))
aIF = 6.9462
bIF = .6469
Hb0IF = 8.5157
lines(ageSub,mf(ageSub,Hb0IF,aIF,bIF),col="red")
abline(h=c(10,11),lty=2)

nls(healthyMaleHbmuSub~a/b*(1-exp(-b*ageSub))+Hb0*exp(-b*ageSub),start=list(a=2,b=.2,Hb0=8),weights = sqrt(healthyMaleHbNSub))
aHM = 4.7587
bHM = .4055
Hb0HM = 9.4930
plot(age,healthyMaleHbmu,type="l",ylim=c(9,13),xlab="Age (in Years)", ylab="Mean Hb Concentration (g/dl)",main="Mean [Hb] in Healthy Vs Infected Males")
lines(ageSub,mf(ageSub,Hb0HM,aHM,bHM))
lines(age,infectedMaleHbmu,col="red")
nls(infectedMaleHbmuSub~a/b*(1-exp(-b*ageSub))+Hb0*exp(-b*ageSub),start=list(a=2,b=.2,Hb0=8),weights = sqrt(infectedMaleHbNSub))
aIM = 6.9735
bIM = .6401
Hb0IM = 8.3541
lines(ageSub,mf(ageSub,Hb0IM,aIM,bIM),col="red")
abline(h=c(10,11),lty=2)
par(mfrow=c(1,1))


plot(ageSub,mf(ageSub,Hb0HF,aHF,bHF)-mf(ageSub,Hb0IF,aIF,bIF),type="l",col="red",ylim=c(0,1),xlab="Age (in Years)",ylab="Drop in Mean Hb Concentration (g/dl)",main="Difference in Mean Hb Concentration by Infection Status")
lines(ageSub,mf(ageSub,Hb0HM,aHM,bHM)-mf(ageSub,Hb0IM,aIM,bIM),col="blue")
lines(ageSub,(mf(ageSub,Hb0HF,aHF,bHF)-mf(ageSub,Hb0IF,aIF,bIF))-(mf(ageSub,Hb0HM,aHM,bHM)-mf(ageSub,Hb0IM,aIM,bIM)),lty=2)
## This is explained by the difference in baseline healthy Hb levels by sex



##### possibly develop Hb to PD?
muHb = rep(0,140)
for(i in 0:139){
  low = 2+.025*i
  high = low+.1
  Hbi = Hb[intersect(which(log10(PD)>=low),which(log10(PD)<high))]
  muHb[i+1] = mean(Hbi,na.rm=T)
}
ss = seq(2,5.475,.025)
plot(ss,muHb,xlab="Log10 Parasite Density",ylab="Hb Measurement",main="Hemoglobin as a Function of Parasitemia")
## @ -infty, Hb Measurement ~ 11.74
abline(v=c(2.8,5))


## explore Hb as a measure of parasite density - very informative for low Hb concentrations,
## uninformative for moderate/high Hb concentration
## Radon-Nikodym derivative for Hb WRT log10 PD

####################### use model predicting log10 Parasite Density
####################### from Hb measurements to give average parasite
####################### density profile by age and sex



##############

plot(PRISM$age[which(PRISM$parsdens>1)],log10(PRISM$parsdens[which(PRISM$parsdens>1)]),xlab="Age (in Years)",ylab="log10 Parasite Density (Parsites per microliter)",main="Parasite Densities by Age")

muPD = rep(NaN,120)
maxPD = rep(NaN,120)
medPD = rep(NaN,120)
for(i in 0:119){
  low = .5+1/12*i
  high = low+1/12
  PDi = PRISM$parsdens[intersect(which(PRISM$age>low & PRISM$age<=high),which(PRISM$parsdens>1))]
  muPD[i+1] = mean(PDi,na.rm=T)
  maxPD[i+1] = max(PDi,na.rm=T)
  medPD[i+1] = median(PDi,na.rm=T)
}
ageInfected = seq(.5+1/24,10.5-1/24,1/12)
plot(ageInfected,log10(maxPD),type="l",ylim=c(0,6))
lines(ageInfected,log10(muPD),type="l")
lines(ageInfected,log10(medPD),type="l",col="blue")





################## Difference in Anemia
#Healthy Male
MildHM = mean(pnorm(11,mean=mf(ageSub,Hb0HM,aHM,bHM),sd=1.1277))-mean(pnorm(10,mean=mf(ageSub,Hb0HM,aHM,bHM),sd=1.1277))
ModHM = mean(pnorm(10,mean=mf(ageSub,Hb0HM,aHM,bHM),sd=1.1277))-mean(pnorm(7,mean=mf(ageSub,Hb0HM,aHM,bHM),sd=1.1277))
SevHM = mean(pnorm(7,mean=mf(ageSub,Hb0HM,aHM,bHM),sd=1.1277))
AnemHM = MildHM+ModHM+SevHM

#Infected Male
MildIM = mean(pnorm(11,mean=mf(ageSub,Hb0IM,aIM,bIM),sd=1.3425))-mean(pnorm(10,mean=mf(ageSub,Hb0IM,aIM,bIM),sd=1.3425))
ModIM = mean(pnorm(10,mean=mf(ageSub,Hb0IM,aIM,bIM),sd=1.3425))-mean(pnorm(7,mean=mf(ageSub,Hb0IM,aIM,bIM),sd=1.3425))
SevIM = mean(pnorm(7,mean=mf(ageSub,Hb0IM,aIM,bIM),sd=1.3425))
AnemIM = MildIM+ModIM+SevIM

#Healthy Female
MildHF = mean(pnorm(11,mean=mf(ageSub,Hb0HF,aHF,bHF),sd=1.1801))-mean(pnorm(10,mean=mf(ageSub,Hb0HF,aHF,bHF),sd=1.1801))
ModHF = mean(pnorm(10,mean=mf(ageSub,Hb0HF,aHF,bHF),sd=1.1801))-mean(pnorm(7,mean=mf(ageSub,Hb0HF,aHF,bHF),sd=1.1801))
SevHF = mean(pnorm(7,mean=mf(ageSub,Hb0HF,aHF,bHF),sd=1.1801))
AnemHF = MildHF+ModHF+SevHF

#Infected Female
MildIF = mean(pnorm(11,mean=mf(ageSub,Hb0IF,aIF,bIF),sd=1.5016))-mean(pnorm(10,mean=mf(ageSub,Hb0IF,aIF,bIF),sd=1.5016))
ModIF = mean(pnorm(10,mean=mf(ageSub,Hb0IF,aIF,bIF),sd=1.5016))-mean(pnorm(7,mean=mf(ageSub,Hb0IF,aIF,bIF),sd=1.5016))
SevIF = mean(pnorm(7,mean=mf(ageSub,Hb0IF,aIF,bIF),sd=1.5016))
AnemIF = MildIF+ModIF+SevIF


Severity= c('Mild','Moderate','Severe','Total')
HealthyMale = c(MildHM,ModHM,SevHM,AnemHM)
InfectedMale = c(MildIM,ModIM,SevIM,AnemIM)
HealthyFemale = c(MildHF,ModHF,SevHF,AnemHF)
InfectedFemale = c(MildIF,ModIF,SevIF,AnemIF)
compare=matrix(c(HealthyMale,InfectedMale,HealthyFemale,InfectedFemale),nrow=4)
barplot(t(compare),beside=T)


par(mfrow=c(2,2))
plot(PRISM$age[which(PRISM$age<.5&PRISM$hb<100&(is.na(PRISM$parsdens)|PRISM$parsdens==0))]*12,PRISM$hb[which(PRISM$age<.5&PRISM$hb<100&(is.na(PRISM$parsdens)|PRISM$parsdens==0))],xlab="Age (in Months)",ylab="Hb Concentration (g/dl)",ylim=c(5,18))
HealthyNeonateHb = PRISM$hb[which(PRISM$age<.5&PRISM$hb<100&(is.na(PRISM$parsdens)|PRISM$parsdens==0))]
HealthyNeonateAge = PRISM$age[which(PRISM$age<.5&PRISM$hb<100&(is.na(PRISM$parsdens)|PRISM$parsdens==0))]*12
nls(HealthyNeonateHb~a/b*(1-exp(-b*HealthyNeonateAge))+Hb0*exp(-b*HealthyNeonateAge),start=list(a=.2,b=.05,Hb0=16))
aHN = 16.992
bHN = 1.671
Hb0HN = 15.368
Nage = seq(0,6,.01)
lines(Nage,aHN/bHN*(1-exp(-bHN*Nage))+Hb0HN*exp(-bHN*Nage),col="blue")
hist(HealthyNeonateHb,freq=F,xlim=c(5,15))

HealthyNeonateSex = PRISM$gender[which(PRISM$age<.5&PRISM$hb<100&(is.na(PRISM$parsdens)|PRISM$parsdens==0))]=='Male'
HealthyNeonateSex=as.numeric(HealthyNeonateSex)

### Healthy Neonates By Sex
plot(HealthyNeonateAge[which(HealthyNeonateSex==1)],HealthyNeonateHb[which(HealthyNeonateSex==1)],ylim=c(6,18))
points(HealthyNeonateAge[which(HealthyNeonateSex==0)],HealthyNeonateHb[which(HealthyNeonateSex==0)],ylim=c(6,18),col="red")


plot(PRISM$age[which(PRISM$age<.5&PRISM$hb<100&PRISM$parsdens>0)]*12,PRISM$hb[which(PRISM$age<.5&PRISM$hb<100&PRISM$parsdens>0)],xlab="Age (in Months)",ylim=c(5,18))
InfectedNeonateHb = PRISM$hb[which(PRISM$age<.5&PRISM$hb<100&PRISM$parsdens>0)]
InfectedNeonateAge = PRISM$age[which(PRISM$age<.5&PRISM$hb<100&PRISM$parsdens>0)]*12
nls(InfectedNeonateHb~a/b*(1-exp(-b*InfectedNeonateAge))+Hb0*exp(-b*InfectedNeonateAge),start=list(a=.2,b=.02,Hb0=14))
aIN = 32.108
bIN = 3.571
Hb0IN = 115.186
muIN = mean(InfectedNeonateHb)
lines(Nage,aIN/bIN*(1-exp(-bIN*Nage))+Hb0IN*exp(-bIN*Nage),col="red")
abline(h=muIN,col="green")
hist(InfectedNeonateHb,freq=F,xlim=c(5,15))
par(mfrow=c(1,1))

InfectedNeonateSex = PRISM$gender[which(PRISM$age<.5 & PRISM$hb<100 & PRISM$parsdens>0)]=='Male'
InfectedNeonateSex = as.numeric(InfectedNeonateSex)


### Infected Neonates By Sex
plot(InfectedNeonateAge[which(InfectedNeonateSex==1)],InfectedNeonateHb[which(InfectedNeonateSex==1)],xlim=c(0,6))
points(InfectedNeonateAge[which(InfectedNeonateSex==0)],InfectedNeonateHb[which(InfectedNeonateSex==0)],col="red")

NeonateAge = PRISM$age[which(PRISM$age<.5&PRISM$hb<100)]
NeonateInfectionStatus = c(rep(0,length(HealthyNeonateHb)),rep(1,length(InfectedNeonateHb)))
t.test(c(HealthyNeonateHb,InfectedNeonateHb)~NeonateInfectionStatus)
