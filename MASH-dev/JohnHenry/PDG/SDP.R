library('animation')

Tmax = 100000#5000
Reps = 1#10000#1
xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
x0 = 300000
xn[1,] = x0
lambdaB = 1
lambdaD = .01#.001
epsilon = .001#.005
pB = matrix(rep(0,Reps*Tmax),nrow=Tmax,ncol=Reps)
pB[1,] = lambdaB/(lambdaB+lambdaD+epsilon*x0)

########### Population View ####################

for(i in 1:(Tmax-1)){
  for(j in 1:Reps){
    B = ifelse(xn[i,j]==0,0,rbinom(1,xn[i,j],pB[i,j]))
    BB = sum(rnbinom(B,50,.3))
    D = rbinom(1,xn[i,j],1-pB[i,j])
    xn[i+1,j] = ifelse(xn[i,j]==0,0,xn[i,j] + BB - D)
    pB[i+1,j] = lambdaB/(lambdaB+lambdaD+epsilon*xn[i,j])
  }
}
plot(xn,type="l")
plot(log10(xn),type="l")

#for(i in 1:(Tmax-1)){
#  B = rbinom(Reps,1,pB[i])
#  WB = which(B==1)
#  WD = which(B==0)
#  for(j in 1:length(WB)){
#    xn[i+1,WB[j]] = ifelse(xn[i,WB[j]]==0,0,xn[i,WB[j]] + sum(rnbinom(1,20,.8)))
#  }
#  for(j in 1:length(WD)){
#    xn[i+1,WD[j]] =  ifelse(xn[i,WD[j]]==0,0,max(xn[i,WD[j]] - rbinom(1,xn[i,WD[j]],1-pB), 0))
#  }
#}

rowVars = function (x,na.rm = TRUE) {
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}

saveGIF({
  pstar = lm(log10(rowVars(xn[2:Tmax,]))~log10(rowMeans(xn[2:Tmax,])))
  pint = pstar$coefficients[1]
  pslope = pstar$coefficients[2]
  t = seq(0,10,.1)
  par(mfrow=c(2,2))
  for(i in 2:Tmax){
    plot(1:i,xn[1:i,1],type="l",xlim=c(0,Tmax),ylim=c(0,100),main="Example Trajectory",xlab="Number of Generations",ylab="Number of Organisms")
    plot(1:Tmax,c(rowMeans(xn)[1:i],rep(NaN,Tmax-i)),type="l",main="Mean of Population",xlab="Number of Generations",ylab="Mean Number of Organisms",ylim=c(0,100),xlim=c(1,Tmax))
    hist(xn[i,],freq=F,xlim=c(0,100),ylim=c(0,.05),breaks=(20+floor(i/3)),main="Probability Distribution of Population Size",xlab="Number of Organisms",ylab="Probability Density")
    plot(log10(rowMeans(xn[1:i,])),log10(rowVars(xn[1:i,])),xlim=c(1,2.5),ylim=c(1,4),main="Mean-Variance Power Law",xlab="log10 Mean",ylab="log10 Variance")
    legend(1,3.6,legend=paste("Slope:",floor(pslope*1000)/1000))
    lines(t,pint+pslope*t)
  }
  par(mfrow=c(1,1))
},interval = .2, ani.width = 1200, ani.height = 600,navigator=ani.options("nmax") <= 1001)

##### Stationary snapshot of final image

par(mfrow=c(2,2))
plot(1:Tmax,xn[1:Tmax,1],type="l",xlim=c(0,Tmax),ylim=c(0,100),main="Example Trajectory",xlab="Number of Reproduction Cycles",ylab="Number of Organisms")
plot(1:Tmax,rowMeans(xn)[1:Tmax],type="l",main="Mean of Population",xlab="Number of Reproduction Cycles",ylab="Mean Number of Organisms",xlim=c(0,Tmax),ylim=c(0,100))
hist(xn[Tmax,],freq=F,xlim=c(0,100),ylim=c(0,.04),breaks=50,main="Histogram of Population Size",xlab="Number of Organisms",ylab="Probability Density")
plot(log10(rowMeans(xn)),log10(rowVars(xn)),xlim=c(1,2.5),ylim=c(1,4),main="Mean-Variance Power Law",xlab="log10 Mean",ylab="log10 Variance")
legend(1.93,2.3,legend=paste("Slope:",floor(pslope*1000)/1000))
lines(t,pint+pslope*t)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
acf(rowMeans(xn),main="ACF of the Mean Signal")
pacf(rowMeans(xn),main="PACF of the Mean Signal")
par(mfrow=c(1,1))

mun = xn
f = seq(1,floor(Tmax/2))/(Tmax)
plot(log10(f),log10(Mod(fft(mun))[1:floor(Tmax/2)]^2/Tmax),xlim=c(-1.5,-.3))
y = log10(Mod(fft(mun))[intersect(which(log10(f)>(-.8)),which(log10(f)<(-.3)))]^2)
ylm = lm(y~log10(f)[intersect(which(log10(f)>(-.8)),which(log10(f)<(-.3)))])
lines(log10(f),ylm$coefficients[1] + ylm$coefficients[2]*log10(f))

#mun = sample(10000,xn[Tmax,],replace=T)
#f = seq(1,10000/2)/10000
#P = (Mod(fft(mun)[1:(10000/2)])^2/10000
#plot(log(f),log(P),xlab="log10 Frequency",ylab="log10 Power",main="Power Spectrum of Stable Distribution")
#y = log(P[intersect(which(log(f)>(-1.9)),which(log(f)<(-1)))])
#ylm = lm(y~log(f)[intersect(which(log(f)>(-1.9)),which(log(f)<(-1)))])
#abline(v=-1.9,lty=2)
#lines(log(f),ylm$coefficients[1] + ylm$coefficients[2]*log(f))
#legend(-1.5,18,legend=paste("Slope:",floor(ylm$coefficients[2]*1000)/1000))


########## Individual View ###################

#for(i in 1:(Tmax-1)){
#  pB = lambdaB/(lambdaB+lambdaD)
#  B = rbinom(1,1,pB)
#  for(j in 1:Reps){
#    xn[i+1,j] = ifelse(B==1,xn[i,j]+rnbinom(1,20,.6),xn[i,j]-rbinom(1,xn[i,j],1-pB))
#  }
#}

#for(i in 1:(Tmax-1)){
#  pB = lambdaB/(lambdaB+lambdaD)
#  pS = rbinom(Reps,1,pB)
#  B = sum()
#  D = xn[i,]*(1-pS)-B
#  BB = 0*B
#  for(j in 1:length(B)){
#    BB[j] = ifelse(B[j]==0,0,sum(rnbinom(B[j],20,.6)))
#  }
#  xn[i+1,] = pmax(xn[i,] + BB*pS - D*(1-pS),0)
#  xn[i+1,which(xn[i,]==0)] = 0
#}


## plot one trajectory to see it makes sense

#plot(xn[,1],type="l")
#hist(xn[,1],breaks=30,freq=F)
#hist(log10(xn[,1]),breaks=30)

## see duration of each infection; small probability of extinction early on,
## but then the rest carry on

#duration = rep(0,Reps)
#for(i in 1:Reps){
#  duration[i] = max(which(xn[,i]>0))
#}
#which(duration<Tmax)
#hist(duration,freq=F)

## compute and plot the mean and variance
## histogram of mean should give stable temporal forcing,
## plot of log(mean) vs log(variance) gives power law,
## confirming tweedie convergence

mun = rep(0,Tmax)
varn = rep(0,Tmax)
for(i in 1:Tmax){
  mun[i] = mean(xn[i,])
  varn[i] = var(xn[i,])
}
plot(mun,type="l")
plot(log10(1+mun),type="l")
plot(log10(mun),log10(varn))
pl = lm(log10(varn[2:30])~log10(mun[2:30]))
t = seq(-5,4,.01)
lines(t,pl$coefficients[1]+pl$coefficients[2]*t)
hist(pl$residuals,freq=F,breaks=40)
hist(10^pl$residuals,freq=F,breaks=30)
#plot(mun,type="l")

hist(mun,breaks=30,freq=F)

abline(v=mean(mun))
hist(log10(1+mun),breaks=20,freq=F)

###############################################
###############################################
## average around 30k
plot(xn,type="l",main="Population Size, Zoom 1X",ylab="Number of Organisms",xlab="Time Steps")
plot(10000:60000,xn[10000:60000],type="l",main="Population Size, Zoom 2X",ylab="Number of Organisms",xlab="Time Steps")
plot(10000:35000,xn[10000:35000],type="l",main="Population Size, Zoom 4X",ylab="Number of Organisms",xlab="Time Steps")
plot(10000:22500,xn[10000:22500],type="l",main="Population Size, Zoom 8X",ylab="Number of Organisms",xlab="Time Steps")
plot(10000:16250,xn[10000:16250],type="l",main="Population Size, Zoom 16X",ylab="Number of Organisms",xlab="Time Steps")
#plot(10001:12500,xn[10001:12500],type="l")
plot(log10(xn),type="l",main="Log10 Population Size, Zoom 1X",ylab="Log10 Number of Organisms",xlab="Time Steps")
plot(10000:60000,log10(xn[10000:60000]),type="l",main="Log10 Population Size, Zoom 2X",ylab="Log10 Number of Organisms",xlab="Time Steps")
plot(10000:35000,log10(xn[10000:35000]),type="l",main="Log10 Population Size, Zoom 4X",ylab="Log10 Number of Organisms",xlab="Time Steps")
plot(10000:22500,log10(xn[10000:22500]),type="l",main="Log10 Population Size, Zoom 8X",ylab="Log10 Number of Organisms",xlab="Time Steps")
plot(10000:16250,log10(xn[10000:16250]),type="l",main="Log10 Population Size, Zoom 16X",ylab="Number of Organisms",xlab="Time Steps")
#plot(10001:12500,log10(xn[10001:12500]),type="l")
pacf(xn)
acf(xn,lag.max=200,ylim=c(-1,1))
hist(xn,freq=F,breaks=500,xlim=c(0,200000),main="Histogram of Population Size",ylab="Frequency Density",xlab="Number of Organisms")
abline(v=mean(xn))
hist(log10(xn)-mean(log10(xn)),freq=F,breaks=100)
abline(v=mean(log10(xn)))
hist(log10(xn)-mean(log10(xn)),freq=F,breaks=100)
abline(v=0)
#abline(v=log10(mean(xn)),col="red")
qqnorm((log10(xn)-mean(log10(xn)))/sqrt(var(log10(xn))[1]))
lines(seq(-5,5,.01),seq(-5,5,.01))

## spectral analysis on raw data
lag.max = 50000
yy = acf(xn,lag.max=lag.max)
pyy = Mod(fft(yy$acf))[1:(lag.max/2)]^2
fyy = seq(1,(lag.max/2))
plot(log(fyy),log(pyy),xlim=c(1,6.5),ylim=c(-2,6),main="Log-Log Plot of Power Spectrum of Data",ylab="Log Spectral Density",xlab="Log Frequency")
lmp = lm(log(pyy[which(log(fyy)>4 & log(fyy)<6)])~log(fyy[which(log(fyy)>4 & log(fyy)<6)]))
lines(seq(4.4,10,.01),lmp$coefficients[1]+lmp$coefficients[2]*seq(4.4,10,.01),col="red",lwd=2)
lines(seq(0,4.4,.01),2.5*rep(1,length(seq(0,4.4,.01))),col="red",lwd=2)
abline(v=4.4,lty=2)

