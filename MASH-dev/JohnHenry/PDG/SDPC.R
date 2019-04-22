
Tmax = 2000
Reps = 100000
xd = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
x0 = 50
xd[1,] = x0
lambdaB = 10
lambdaD = 4
pB = lambdaB/(lambdaB+lambdaD)

## This is for the population; this is what Anderson showed
#for(i in 1:(Tmax-1)){
#  pB = lambdaB/(lambdaB+lambdaD)
#  B = rbinom(Reps,1,pB)
#  BB = which(B==1)
#  DD = which(B==0)
#  x[i+1,BB] = x[i,BB] + rgamma(BB,lambdaB,lambdaD)
#  x[i+1,DD] = rbeta(DD,lambdaB,lambdaD)*x[i,DD]
#}

for(i in 1:(Tmax-1)){
  pB = lambdaB/(lambdaB+lambdaD)
  B = rbinom(1,1,pB)
  if(B==1){
    xd[i+1,] = xd[i,] + rgamma(Reps,lambdaB,lambdaD)
  }
  if(B==0){
    xd[i+1,] = rbeta(Reps,lambdaB,lambdaD)*xd[i,]
  }
}


## plot one trajectory to see it makes sense

plot(xd[,1],type="l")
hist(xd[,1],breaks=30)
hist(log10(xd[,1]),breaks=30)

plot(log10(xd[,1]),type="l")
abline(h=log10(mean(xd[,1])),lty=2)
## see duration of each infection; small probability of extinction early on,
## but then the rest carry on

duration = rep(0,Reps)
for(i in 1:Reps){
  duration[i] = max(which(xd[,i]>0))
}
which(duration<Tmax)

## compute and plot the mean and variance
## histogram of mean should give stable temporal forcing,
## plot of log(mean) vs log(variance) gives power law,
## confirming tweedie convergence

mud = rep(0,Tmax)
vard = rep(0,Tmax)
for(i in 1:Tmax){
  mud[i] = mean(xd[i,])
  vard[i] = var(xd[i,])
}
plot(mud,type="l")
plot(log10(mud),type="l")
plot(log10(mud),log10(vard))
pl = lm(log10(vard[2:1000])~log10(mud[2:1000]))
t = seq(0,3,.01)
lines(t,pl$coefficients[1]+pl$coefficients[2]*t)
hist(pl$residuals,freq=F,breaks=20)
hist(10^pl$residuals,freq=F,breaks=20)

hist(mud,breaks=50,freq=F)
abline(v=mean(mud))
hist(log10(mud),breaks=50,freq=F)

## 
f = seq(1,(floor((Tmax/2))))/Tmax
plot(log10(f),log10(Mod(fft(mud/sum(mud)))[1:(Tmax/2)]^2))#xlim=c(-1.5,-.25))

y = log10(Mod(fft(mud/sum(mud)))[1:(Tmax/2)]^2)
ylm = lm(y~log10(f))
lines(log10(f),ylm$coefficients[1]+ylm$coefficients[2]*log10(f))

hist(lm(y~log10(f))$residuals,breaks=20)
abline(v=mean(lm(y~log10(f))$residuals))
