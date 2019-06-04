library(readxl)
library(vioplot)
library(matrixStats)
library(stinepack)
library(viridis)
library(fitdistrplus)
library(zoib)
library(gamlss)


## import Asexual Parasite Data
MT_PT_NP <- read_xlsx("~/GitHub/MASH-Main/MASH-Main/MASH-dev/JohnHenry/PDG/MT_PT_NP.xlsx")
M = as.matrix(MT_PT_NP)

MT_Days <- read_excel("~/Malaria Data Files/MT_Days.xlsx")

## import Gametocyte Data
Gt_Col <- read_excel("~/Malaria Data Files/Gt_Col.xlsx")
G = matrix(NaN,nrow=1400,ncol=334)

## which rows denote beginning of individual
begin = which(Days==1)

## which rows denote end of individual
end = which(Days==1)-1
end = c(end,length(Days))
end = end[2:length(end)]

for(i in 1:334){
  G[1:(end[i]-begin[i]+1),i] = Gt[begin[i]:end[i]]
}

############################################################
############## Duration of Infection #######################
############################################################


## find the last day of patent infection
N = rep(0,334)
for(i in 1:334){
  position = which(!is.na(M[,i]))
  nonzero = which(M[,i]>0)
  N[i] = max(intersect(position,nonzero))
}

## plot empirical survival function
N = sort(N)
Ndays = rep(0,250)
for(i in 1:251){
  Ndays[i] = tail(cumsum(N<i),1)
}
Ndays = Ndays/334
plot(1-Ndays,main="Empirical Survival Function",xlab="Days Since First Patency",ylab="Proportion of Patients Infected",type="l")

## fit rate of recovery parameter by fitting to survival function of pdf, 1-exp(-lambda*t)
plot(-log(1-Ndays))
time = 1:length(Ndays)
f = -log(1-Ndays)
durfit = lm(f ~ time)
lambda = durfit$coefficients[2]
lines(0:250,durfit$coefficients[1] + durfit$coefficients[2]*0:250)

plot(1-Ndays,xlab="Days Since First Patency",ylab="Proportion of Patients Infected",main="Fitted Duration of Infection")
lines(0:250,exp(-lambda*0:250))

########################################################################
################ Variance-to-Mean Power Law ############################
########################################################################


# remove subpatent infections, then take mean across all patent infections
MP = M
MP[which(MP==0)] = NaN
rm = rowMeans(MP,na.rm=T)
rv = rowVars(MP,na.rm=T)

#Power Law for Asexual Densities, Removing the subpatent infections
plot(log10(rm),xlim=c(0,300),type="l")
plot(log10(rv),xlim=c(0,300),type="l")
plot(log10(rm)[1:300],log10(rv[1:300]),xlab="log10 Mean of Asexual Densities",ylab="log10 Variance of Asexual Densities",main="Variance-to-Mean Power Law for Asexual Parasitemia")
## weight fit by the square root of the sample size to account for sample mean uncertainty
rmrvfit = lm(log10(rv[1:125])~log10(rm[1:125]),weights = sqrt(N[1:125]))
lines(seq(-1,6),rmrvfit$coefficients[1]+rmrvfit$coefficients[2]*seq(-1,6))
#legend(c(2,5,9.5),legend=c("Slope: " + as.character(rmrvfit$coefficients[2])))

## restricted view of power law, at least 100 individuals (~30 percent) present for average
# .3 = exp(-lambda*maxDays), solving for maxDays determines the limit of time restriction
# which implies 30 percent of the patients being infected
# (floor guarantees an integer number of days)
maxDays = floor(-log(.3)/lambda)
plot(log10(rm[1:maxDays]),log10(rv[1:maxDays]),xlab="log10 Mean of Asexual Densities",ylab="log10 Variance of Asexual Densities",main="Variance-to-Mean Power Law, Restricted Domain")
lines(seq(-1,6),rmrvfit$coefficients[1]+rmrvfit$coefficients[2]*seq(-1,6))

################################################################################
################## Average Asexual and Gametocyte Densities ####################
################################################################################


# remove subpatent gametocytemia, then take average across all patent gametocyte measurements
GP = G
GP[which(GP==0)] = NaN
rmg = rowMeans(GP,na.rm=T)

# plot average asexual and gametocyte densities, conditioned on patency
plot(log10(rm),type="l",xlim=c(0,365),ylim=c(0,5),xlab="Days Since First Patency",ylab="log10 Parasite Densities per Microliter",main="Average Asexual and Gametocyte Densities Conditioned on Infection")
lines(log10(rmg),col="red")
## this line denotes the estimated limit of detection by microscopy
## point of blank is 0 parasites per microliter
abline(h=log10(88),lty=2)

## Bin by Month to show Violin Plots of Asexuals
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

## Bin by Month to show Violin Plots of Gametocytes
GV = matrix(0,nrow=30,ncol=length(G[1,]))
for(i in 1:30){
  GV[i,] = colMeans(GP[(30*(i-1)+1):(30*i),],na.rm=T)
}
GV[which(GV==0)] = NaN

vioplot(log10(na.omit(GV[1,])),log10(na.omit(GV[2,])),log10(na.omit(GV[3,])),log10(na.omit(GV[4,])),log10(na.omit(GV[5,])),log10(na.omit(GV[6,])),log10(na.omit(GV[7,])),
        ylim=c(0,5.2),col="lightblue")
title(main = "Violin Plot of Monthly Mean Gametocyte Densities", xlab="Months Post-Infection",ylab="Mean log10 Gametocyte Density per microliter")
abline(h=log10(88),lty=2)

##########################################################################
##################### Fever Risk and Severity ############################
##########################################################################

## import fever data
fever <- read_excel("~/GitHub/MASH-Main/MASH-Main/MASH-dev/JohnHenry/PDG/Fever.xlsx")
fever = as.matrix(fever)
dots = which(fever=='.')
fever[dots] = NaN
fever=as.numeric(fever)

## position of each infection measurement relative to fever measurement
pos = c(56,324,401,72,22,31,50,231,736,40,590,50,47,220,38,531,172,12,442,431,527,693,419,
        154,63,432,526,13,139,93,123,445,130,471,14,34,421,425,401,315,186,375,135,127,324,
        289,32,481,61,28,211,174,190,31,61,440,476,430,48,254,53,98,176,78,115,24,342,376,
        370,32,234,312,13,458,52,99,114,176,160,280,40,257,588,498,638,330,638,497,506,405,
        38,72,159,536,234,47,405,159,144,60,469,239,442,68,71,303,192,400,261,140,135,180,
        181,332,50,251,84,240,175,50,199,80,124,127,12,320,18,120,67,261,522,543,119,409,
        414,732,396,366,483,503,584,498,60,187,425,433,80,527,209,50,1327,43,43,42,18,106,
        50,45,10,81,36,39,120,66,41,7,6,43,45,58,109,54,113,125,199,9,12,32,71,65,95,108,
        50,43,77,41,209,80,47,231,88,178,26,33,84,49,146,56,70,85,80,176,48,178,18,120,46,
        131,67,74,49,84,57,45,243,102,41,195,173,56,30,143,41,167,145,105,46,108,64,117,45,
        343,137,51,111,71,102,240,160,201,96,160,211,130,160,50,227,118,158,170,57,131,
        177,115,46,62,103,101,52,215,39,106,35,141,99,89,188,108,107,107,160,67,83,107,86,
        75,36,92,162,139,126,152,138,11,93,85,47,152,131,152,35,106,108,17,31,115,103,37,93,
        89,45,80,57,83,110,33,15,77,80,57,66,45,47,114,93,35,22,96,44,33,104,32,10,113,6,10,
        10,34,71,45,80,8,94,105)

pospos = cumsum(pos)

Fever = matrix(NaN,ncol=334,nrow=1327)
Fever[1:pos[1],1] = fever[1:pospos[1]]
for(i in 2:334){
  Fever[1:pos[i],i] = fever[(pospos[i-1]+1):pospos[i]]
}

## define BinaryFever, 0 if no fever present, 1 if fever present
## average over BinaryFever to get PropFever, the proportion with a fever
## fever defined here as temp over 38 C / 100.4 F
BinaryFever = (Fever>38)
# temperature was recorded if the patient had fever
BinaryFever[which(is.na(BinaryFever))] = 0
PropFever = rowMeans(BinaryFever,na.rm=T)
plot(PropFever[1:365],type="l",ylim=c(0,1),xlab="Days Since Patency", ylab="Proportion with Fever",main="Daily Prevalence of Fever Conditioned on Infection")
#plot(-log10(1-PropFever[1:365]),type="l",main="Daily Rate of Fever")

## compare fever probability to parasite densities, normalized by maximum density for scale
plot(log10(rm[1:365])/max(log10(rm[1:200]),rm.na=T),type="l",ylim=c(0,1))
lines(PropFever[1:365])

##compare parasite densities to PropFever directly
plot(log10(rm[1:365]),PropFever[1:365],xlim=c(1,5),ylim=c(0,1),xlab="log10 Mean Asexual Densities",ylab="Proability of Fever",main="Probability of Fever Given Asexual Parasite Density")

## fit sigmoid function to Probability of Fever Given Asexual Parasitemia
aa = log10(rm[1:200])
bb = PropFever[1:200]
sigfitfev = nls(bb~p1*exp(p2*aa)/(p3+exp(p2*aa)),start=list(p1=1,p2=1,p3=100))
p1 = .5925
p2 = 3.027
p3 = 2.651*10^5
sigmoidFev = function(x,p1,p2,p3){
  p1*exp(p2*x)/(p3+exp(p2*x))
}
lines(seq(1,6,.01),sigmoidFev(seq(1,6,.01),p1,p2,p3))
abline(h=p1,lty=2)

## compare fever severity with parasite densities
# remove nonsensical fever measurements
Fever[which(Fever<30)] = NaN
MFever = rowMeans(Fever,na.rm=T)
plot(MFever,type="l",xlim=c(0,140),xlab="Days",ylab="Temperature (Degrees Celsius)")
plot(log10(rm[1:140]),MFever[1:140],xlab="log10 Asexual Parasitemia",ylab="Temperature (Degrees Celsius)")
#no apparent delay between parasitemia and fever response
ccf(rm[1:125],MFever[1:125],lag.max=20)
hist(MFever,breaks=15)
## severe fevers would be underestimated using a normal distributed temp
qqnorm(MFever)

###############################################################################
################ Transmission Efficiency from Gametocytemia ###################
###############################################################################


Mosquito_Transmission <- read_excel("~/Malaria Data Files/Mosquito_Transmission.xlsx")
MT_Days <- read_excel("~/Malaria Data Files/MT_Days.xlsx")
Gt_Col <- read_excel("~/Malaria Data Files/Gt_Col.xlsx")

Days = as.matrix(MT_Days)
## which rows denote beginning of individual
begin = which(Days==1)

## which rows denote end of individual
end = which(Days==1)-1
end = c(end,length(Days))
end = end[2:length(end)]

Mosq = as.matrix(Mosquito_Transmission)
Mosq[which(Mosq==".")]=NaN
Mosq = as.numeric(Mosq)

Gt = as.matrix(Gt_Col)
Gt[which(Gt==".")]=NaN
Gt = as.numeric(Gt)

Pt = as.matrix(MT_PT_NP)

TE = matrix(NaN,nrow=1327,ncol=334)
MGt = TE
for(i in 1:334){
  TE[1:(end[i]-begin[i]+1),i] = Mosq[begin[i]:end[i]]
  MGt[1:(end[i]-begin[i]+1),i] = Gt[begin[i]:end[i]]
}

### compare average gametocyte levels to average proportion of infected mosquitoes
TEmu = rowMeans(TE,na.rm=T)/100

# ** this is measured TE
plot(TEmu,type="l",xlim=c(0,200))

## This creates a sliding window for TE as a function of log10(Gt)
Mprime = Mosq[which(is.na(Mosq)==F)]
Gtprime = as.numeric(Gt[which(is.na(Mosq)==F)])

betazfit = function(a){
  ## a should be between .5 and 4
  betaz = Mprime[which(log10(Gtprime) > (a-.25) & log10(Gtprime) <= (a+.25))]/100
  logGTz = pmax(log10(Gtprime)[which(log10(Gtprime) > (a-.2) & log10(Gtprime) <= (a+.2))],0)
  fit = fitdist(betaz,distr="beta",method="mge")
  az = fit$estimate[1]
  bz = fit$estimate[2]
  return(c(az,bz))
}

z = seq(1,4,.1)
muz = 0*z
params = matrix(0,nrow=2,ncol=length(z))
for(i in 1:length(z)){
  params[,i] = betazfit(z[i])
  muz[i] = params[1,i]/(params[1,i]+params[2,i])
}

plot(z,muz)

aa = z
bb = muz
sigfitTE = nls(bb~p1*exp(p2*aa)/(p3+exp(p2*aa)),start=list(p1=.6,p2=2,p3=100))
p1 = .6828
p2 = 2.1433
p3 = 130.8316

sigmoidTE = function(x,p1,p2,p3){
  p1*exp(p2*x)/(p3+exp(p2*x))
}
plot(aa,bb,ylim=c(0,1))
lines(seq(.5,6,.01),sigmoidTE(seq(.5,6,.01),p1,p2,p3))
abline(h=p1,lty=2)

## Gametocytes seem to be a strong indicator of transmission efficiency, but only
## after 20 days post first patency of asexuals; about 10 days afer we first
## expect to see gametocytes. This suggests the first ones made are not very infectious

## here we estimate the total number of fully infectious days
## per individual infection by multiplying the transmission efficiency
## by the survival function of the infection
TEhat = sigmoidTE(log10(rmg),p1,p2,p3)
plot(TEhat,type="l",xlim=c(0,300),ylim=c(0,1))

TEhat[which(is.na(TEhat))] = 0
plot(1:300,cumsum(TEhat[1:300]*exp(-lambda*seq(1:300))),type="l",xlab="Days of Infection",ylab="Effective Number of Infectious Days",main="Cumulative Number of Totally Infectious Days")
abline(h=cumsum(TEhat[1:300]*exp(-lambda*seq(0:299)))[300],lty=2)

