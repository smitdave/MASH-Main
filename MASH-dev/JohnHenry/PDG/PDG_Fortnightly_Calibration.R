########### run PfPDG_Fitting.R, MT_Fever.R, MT_TE.R first
########### fortnightly PDG callibration


######################################## Time to Subpatency


N = rep(0,334)
for(i in 1:334){
  position = which(!is.na(M[,i]))
  nonzero = which(M[,i]>0)
  N[i] = max(intersect(position,nonzero))
}

N = sort(N)
NF = rep(0,26)
for(i in 1:26){
  NF[i] = tail(cumsum(N<(14*i)),1)
}
NF = NF/max(NF)
plot(-log(1-NF[1:15]),xlab="Fortnights")

## fitting to -log(1-Surv), including first 15 fortnights (~210 days) which has majority of data
y = -log(1-NF)
x = seq(1,26)
lamfit = lm(y[1:15]~x[1:15]+0)
lambdaF = lamfit$coefficients
lines(x,x*lambdaF)
lambdaF

hist(N/14,freq=F,main="Histogram of Surviving Patent Infections",xlab="Fortnights",breaks=20)
n = seq(0,30)
lines(dexp(n,lambdaF))


########### Parasite Densities, tracked fortnightly with final density around 1 year (26 fortnights)


WP = matrix(0,nrow=26,ncol=334)
MP = rep(0,26)
VP = rep(0,26)

for(j in 1:334){
  for(i in 1:26){
    WP[i,j] = mean(M[((i-1)*14+1):(i*14),j],na.rm=T)
  }
}

for(i in 1:26){
  MP[i] = mean(WP[i,],na.rm=T)
  VP[i] = var(WP[i,],na.rm=T)
}

plot(1:26,log10(MP),type="l",xlab="Fortnights Since Infection",ylab="Mean log10 Parasite Density",main="Mean log10 Density Averged over Fortnights")
plot(log10(MP),log10(VP),xlab="log10 Mean Parasite Density",ylab="log10 Variance Parasite Density",main="Mean-Variance Power Law")
mvfit = lm(log10(VP) ~ log10(MP))
b = mvfit$coefficients[1]
m = mvfit$coefficients[2]
x = seq(0,5,.1)
lines(x,m*x+b)


#################################### Fever Signals from Asexuals


FeverFNM = rep(0,26)
FeverFNP = rep(0,26)
FeverFN = matrix(0,nrow=26,ncol=334)
Fever[which(Fever<30)]=NaN
for(j in 1:334){
  for(i in 1:26){
    FeverFN[i,j] = mean(Fever[((i-1)*14+1):(i*14),j],na.rm=T)
  }
}

FeverFNBin = FeverFN
FeverFNBin[which(!is.na(FeverFNBin))]=1
FeverFNBin[which(is.na(FeverFNBin))]=0
for(i in 1:26){
  FeverFNM[i] = mean(FeverFN[i,],na.rm=T)
  FeverFNP[i] = mean(FeverFNBin[i,],na.rm=T)
}

LMP = log10(MP)
plot(LMP,FeverFNP,xlab="log10 Asexual Parasite Densities",ylab="Proportion with Fever",main="Fever Probability for Given Parasite Density",xlim=c(0,5),ylim=c(0,1))
fevfit = nls(FeverFNP~p1*exp(p2*(LMP))/(exp(p2*p3)+exp(p2*LMP)),start=list(p1=.9,p2=3,p3=3))
p1 = .8835
p2 = 3.038
p3 = 3.5246
x = seq(0,5,.1)
lines(x,p1*exp(p2*x)/(exp(p2*p3)+exp(p2*x)))


plot(log10(MP),FeverFNM,xlab="log10 Asexual Parasite Densities",ylab="Body Temperature",main="Degree of Fever for Given Parasite Density")
##hist(FeverFNM)
## compare to full dataset, is normally distributed:
hist(MFeverF,freq=F,xlab="Temperature (Degrees Fahrenheit)",main="Temperature Given Fever")
## for a given parasite density, calculate probability of fever; if fever present, draw from normal dist'n


#################################### Gametocytes


WG = matrix(0,nrow=26,ncol=334)
MG = rep(0,26)
VG = rep(0,26)

WTE = matrix(0,nrow=26,ncol=334)
MTE = rep(0,26)
VTE = rep(0,26)

for(j in 1:334){
  for(i in 1:26){
    WG[i,j] = mean(MGt[((i-1)*14+1):(i*14),j],na.rm=T)
  }
}

for(i in 1:26){
  MG[i] = mean(WG[i,],na.rm=T)
  VG[i] = var(WG[i,],na.rm=T)
  MTE[i] = mean(WTE[i,],na.rm=T)
  VTE[i] = var(WTE[i,],na.rm=T)
}

## asexual-gametocyte relationship
plot(log10(MP),log10(MG),xlab="log10 Asexual Parasite Density",ylab="log10 Gametocyte Density",main="Mean Asexual and Gametocyte Densities Power Law")
ccf(log10(MP),log10(MG))
## note here the lag-dependence is almost non-existent; for a one-week timestep this lag may still persist



#################################### TE Signals from Gametocytes


WTE = matrix(0,nrow=26,ncol=334)
MTE = rep(0,26)
VTE = rep(0,26)

for(j in 1:334){
  for(i in 1:26){
    WTE[i,j] = mean(TE[((i-1)*14+1):(i*14),j],na.rm=T)
  }
}

WTE = WTE/100

for(i in 1:26){
  MTE[i] = mean(WTE[i,],na.rm=T)
  VTE[i] = var(WTE[i,],na.rm=T)
}

LMG = log10(MG)
plot(LMG,MTE,xlim=c(-2,4),ylim=c(0,1),xlab="log10 Gametocyte Density",ylab="Transmission Efficiency",main="Transmission Efficiency for Given Gametocyte Density")
TEfit = nls(MTE~p1*exp(p2*(LMG))/(exp(p2*p3)+exp(p2*LMG)),start=list(p1=.9,p2=3,p3=3))
p1 = .4242
p2 = 3.5524
p3 = 2.3038
x = seq(-2,4,.1)
lines(x,p1*exp(p2*x)/(exp(p2*p3)+exp(p2*x)))


