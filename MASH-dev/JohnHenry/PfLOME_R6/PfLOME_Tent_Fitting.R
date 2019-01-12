MT_PT_NP <- read_excel("~/GitHub/MASH-Main/MASH-dev/JohnHenry/PDG/MT_PT_NP.xlsx")
M = as.matrix(MT_PT_NP)

Mmu = rowMeans(M,na.rm=T)

plot(log10(Mmu*5*10^6),type="l",xlim=c(0,365))
Mmx = which(Mmu == max(Mmu))
abline(v = Mmx)

plot(log10(Mmu),1:length(Mmu),type="l",ylim=c(0,365),ylab="Age of Infection (Days)",xlab="log10 Parasite Count")

## 4 pflome tent function parameters:
duration = rep(0,333)
peak = rep(0,333)
initial = rep(0,333)
peakTime = rep(0,333)

for(i in 1:333){
  peak[i] = max(M[,i],na.rm=T)
  peakTime[i] = which((M[,i])==peak[i])[1]
  initial[i] = M[min(which(M[,i]>0)),i]
  duration[i] = max(which(M[,i]>0))
}

################### Peak Density ######################

## histogram of the log10 peak parasite density
hist(log10(5*10^6*peak),freq=F,xlim=c(9,14),breaks=30,ylim=c(0,1))
mu = mean(log10(5*10^6*peak))
ss = var(log10(5*10^6*peak))
## this appears approximately normally distributed with mean ~11.4,
## standard deviation ~sqrt(.24)
x = seq(9,14,.01)
lines(x,dnorm(x,mu,sqrt(ss)))

##################### Time of Peak ####################

hist(peakTime,breaks=100,freq=F)
mu = mean(peakTime)
ss = var(peakTime)

############ winner
## LN using MLE
x = seq(0,80,.01)
## need package fitdistrplus
fitdist(peakTime,distr="lnorm",method="mle")
muMLE = 2.17
ssMLE = .534
lines(x,dlnorm(x,muMLE,ssMLE))

## Gamma using MoM estimators
#alphaMoM = mu^2/ss
#betaMoM = mu/ss
#x = seq(0,80,.01)
#lines(x,dgamma(x,alphaMoM,betaMoM))

#fitdist(peakTime,distr="gamma",method="mle")
#alphaMLE = 2.72
#betaMLE = .255
#lines(x,dgamma(x,alphaMLE,betaMLE))


## check for lognormality
hist(log(peakTime),breaks=30) #### appears kinda normal? bit skewed

## LN using MoM
#muhat = -log(sum(peakTime^2))/2+2*log(sum(peakTime))-3/2*log(length(peakTime))
#sshat = log(sum(peakTime^2))-2*log(sum(peakTime))+log(length(peakTime))
#x = seq(0,80,.01)
#lines(x,dlnorm(x,muhat,sshat))



#################### initial density ##################

hist(log10(initial*5*10^6),breaks=50,freq=F,xlim=c(7,15))
x = seq(0,4.5,.01)
## this is both MoM and MLE, but biased estimator
lambda = 1/mean(log10(initial*5*10^6)-7.5)
lines(x+7.5,dexp(x,lambda))

## therefore the dist'n should be approximately Exp(1.06)+7.5


################### Total Duration ####################

hist(duration,freq=F,breaks=20)
x = seq(0,400,.1)
lambda = 1/mean(duration)
lines(x,dexp(x,lambda))



################### Examples of Tent Functions ###########


tent_test = function(){
  MZ0 = rexp(1,1.06)+7.5
  peak = rnorm(1,11.4,sqrt(.24))
  while(peak <= MZ0){
    MZ0 = rexp(1,1.06)+7.5
  }
  peakTime = rlnorm(1,2.17,.534)
  duration = rexp(1,1/103.8)
  while(duration <= peakTime){
    duration = rexp(1,1/103)
  }
  
  x = seq(0,365,1)
  tent = x
  for(i in 1:length(x)){
    if(x[i] <= peakTime){
      tent[i] = MZ0 + (peak-MZ0)/peakTime*x[i]
    }
    if(x[i] > peakTime){
      tent[i] = max(peak - (peak-log10(2.5*10^7))/(duration-peakTime)*(x[i]-peakTime),0)
    }
  }
  tent = log10(10^tent*(2*10^-7))
  plot(x,tent,type="l",ylab="log10 Parasites per microliter",xlab="days",ylim=c(-1,6))
  abline(h=log10(50))
  abline(h=log10(5000),col="red")
  pars = c(MZ0,peak,peakTime,duration)
  return(pars)
}

