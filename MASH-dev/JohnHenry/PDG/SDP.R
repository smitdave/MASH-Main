
Tmax = 2000
Reps = 100000
xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
x0 = 50
xn[1,] = x0
lambdaB = 4
lambdaD = 3
pB = lambdaB/(lambdaB+lambdaD)

for(i in 1:(Tmax-1)){
  pB = lambdaB/(lambdaB+lambdaD)
  B = rbinom(1,1,pB)
  if(B==1){
    xn[i+1,] = xn[i,] + rnbinom(Reps,20,.6)
    ## if it was zero, it's always zero
    xn[i+1,which(xn[i,]==0)] = 0
  }
  if(B==0){
    Beta = rbeta(Reps,2,10)
    xn[i+1,] = xn[i,] - rbinom(Reps,xn[i,],Beta)
    ## if it was zero, it's always zero
    xn[i+1,which(xn[i,]==0)] = 0
  }
}


## plot one trajectory to see it makes sense

plot(xn[,1],type="l")
hist(xn[,1],breaks=30)
hist(log10(xn[,1]),breaks=20)

## see duration of each infection; small probability of extinction early on,
## but then the rest carry on

duration = rep(0,Reps)
for(i in 1:Reps){
  duration[i] = max(which(xn[,i]>0))
}
which(duration<Tmax)
hist(duration,freq=F)
## compute and plot the mean and variance
## histogram of mean should give stable temporal forcing,
## plot of log(mean) vs log(variance) gives power law,
## confirming tweedie convergence

mun = rep(0,Tmax)
varn = rep(0,Tmax)
for(i in 1:Tmax){
  mun[i] = mean(xn[i,which(xn[i,]>0)])
  varn[i] = var(xn[i,which(xn[i,]>0)])
}
plot(mun,type="l")
plot(log10(mun),type="l")
plot(log10(mun),log10(varn))
pl = lm(log10(varn[4:2000])~log10(mun[4:2000]))
t = seq(-5,3,.01)
lines(t,pl$coefficients[1]+pl$coefficients[2]*t)
hist(pl$residuals,freq=F,breaks=50)
#plot(mun,type="l")

hist(mun,breaks=20,freq=F)
abline(v=mean(mun))
hist(log10(mun),breaks=20,freq=F)

## 
f = seq(1,(Tmax)/2)/Tmax
plot(log10(f),log10(Mod(fft(mun/sum(mun)))[1:(Tmax/2)]^2))#xlim=c(-1.5,-.25))
y = log10(Mod(fft(mun/sum(mun)))^2)[1:(Tmax/2)]
ylm = lm(y~log10(f))
lines(log10(f),ylm$coefficients[1] + ylm$coefficients[2]*log10(f))
hist(lm(y~log10(f))$residuals,breaks=20)

#plot(log10((seq(1,Tmax/2)/Tmax)),log10(Mod(fft(mun/sum(mun)))[1:Tmax/2]^2),type="l",xlim=c(-1.5,0))


## SAVED ENSEMBLES: ##################

x05
mun05
plot(log10(mun05),type="l")

x01525
mun01525

ave = rep(0,500)
for(i in 1:500){
  ave[i] = mean((x05[i,]+x01525[i,])/2)
}


hist(c(mun05,mun01525),breaks=100,freq=F)
hist(log10(c(mun05,mun01525)),breaks=100,freq=F)

## fit distributions to see if we can estimate the approximate malthusian
## fitness of the average 'fractal' individual