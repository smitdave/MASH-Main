## 'Stochastic Demographic Process'
## 

library('animation')
#install.packages("remotes")
#remotes::install_github("piklprado/sads")
library(plotly)
library(hexbin)

#this saves the run for later analysis
#write.csv(xn,file="Chaotic_Trajectory_SPD_10k.csv")
#write.csv(xn,'Honeycomb_SPD.csv')

Tmax = 3000
Reps = 5000 #set to 200 for "Chaotic_Trajectory_SPD.csv"
xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
x0 = 10^5.5
xn[1,] = rnbinom(Reps,x0*2,.5)
lambdaB = 4#1 for regular/irregular, 2 for chaos
lambdaD = .5
epsilon = .005
pB = matrix(rep(0,Reps*Tmax),nrow=Tmax,ncol=Reps)
pB[1,] = lambdaB/(lambdaB+lambdaD+epsilon*x0)

########### Population View ####################

for(i in 1:(Tmax-1)){
  for(j in 1:Reps){
    B = ifelse(xn[i,j]==0,0,rbinom(1,xn[i,j],pB[i,j]))
    BB = sum(rnbinom(B,20,.05)) # B,20,.05 for chaos, B,10,.05 for irregular, B,50,.9 for regular
    D = rbinom(1,xn[i,j],1-pB[i,j]) 
    xn[i+1,j] = ifelse(xn[i,j]==0,0,xn[i,j] + BB - D)
    pB[i+1,j] = lambdaB/(lambdaB+lambdaD+epsilon*xn[i,j])
  }
  if(i == floor(Tmax/10)){
    print('10 Percent')
  }
  if(i == floor(Tmax/5)){
    print('20 Percent')
  }
  if(i == floor(Tmax*3/10)){
    print('30 Percent')
  }
  if(i == floor(Tmax*2/5)){
    print('40 Percent')
  }
  if(i == floor(Tmax/2)){
    print('50 Percent')
  }
  if(i == floor(Tmax*6/10)){
    print('60 Percent')
  }
  if(i == floor(Tmax*7/10)){
    print('70 Percent')
  }
  if(i == floor(Tmax*8/10)){
    print('80 Percent')
  }
  if(i == floor(Tmax*9/10)){
    print('90 Percent')
  }
}


plot(log10(xn[1:Tmax,2]),xlab="Reproductive Cycles",ylab="Log10 Population Size",main="Example Simulated Population",type="l")
plot(log10(xn[2:Tmax,2]),log10(xn[1:(Tmax-1),2]),xlab="Log10 Population",ylab="Log10 Population Lagged by 1 Cycle",main="Lag 1 Embedding",type="l")
plot(log10(xn[2:Tmax,2]),log10(xn[1:(Tmax-1),2]))

#plot(log10(xn[5000:Tmax,1]),log10(xn[4999:(Tmax-1),1]),type="l")
#plot(log10(xn[5000:Tmax,1]),log10(xn[4999:(Tmax-1),1]))

#plot(log10(xn[500:1000,1]),log10(xn[499:999,1]),type="l",xlim=c(5.4,5.75),ylim=c(5.4,5.75))
#plot(log10(xn[4000:5000,1]),log10(xn[3999:4999,1]),xlim=c(5.4,5.75),ylim=c(5.4,5.75))
N = 10
lag0 = list(log10(xn[3:Tmax,N]))
lag1 = list(log10(xn[2:(Tmax-1),N]))
lag2 = list(log10(xn[1:(Tmax-2),N]))
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])

df01 = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]])
df02 = data.frame(lag0 = lag0[[1]], lag2 = lag2[[1]])
df12 = data.frame(lag1 = lag1[[1]], lag2 = lag2[[1]])
h01 = hexbin(df01,xbins=50)
h02 = hexbin(df02,xbins=50)
h12 = hexbin(df12,xbins=50)
plot(h01)
plot(h02)
plot(h12)

xnacf = acf(xn[1:Tmax,N],lag.max=1000)
plot(log10(1:500),log10(Mod(fft(xnacf$acf[1:1000]))[1:500]^2))

plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode="points",marker=list(size=2)) %>%
  add_trace(
    x = lag0,
    y = lag1,
    z = lag2,
    marker = list(
      color = 'rgb(17, 157, 255)',
      size = .001,
      opacity = 0.5,
      line = list(
       color = 'rgb(231, 99, 250)',
       width = 1
      )
    ),
  showlegend = F
)

xx = rowMeans(xn[1:Tmax,])
plot(log10(xx))
acf(log10(xx),lag.max=3000)
plot(log10(xx),xlab="Reproductive Cycles",ylab="Log10 Population Size",main="Average Population Size")

plot(log10(xx[2:Tmax]),log10(xx[1:(Tmax-1)]),xlab="Population Size",ylab="Population Size Lagged By 1 Cycle",main="Single Lag Embedding",type="l")
plot(log10(xx[2:Tmax]),log10(xx[1:(Tmax-1)]))

#plot(log10(xx[2:Tmax]),log10(xx[1:(Tmax-1)]),xlim=c(5.565,5.62),ylim=c(5.565,5.62))
#plot(log10(xx[10001:Tmax]),log10(xx[10000:(Tmax-1)]))#,xlim=c(5.565,5.62),ylim=c(5.565,5.62),type="l")

lag0 = list(log10(xx[3:Tmax]))
lag1 = list(log10(xx[2:(Tmax-1)]))
lag2 = list(log10(xx[1:(Tmax-2)]))
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode='lines')

df01 = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]])
df02 = data.frame(lag0 = lag0[[1]], lag2 = lag2[[1]])
df12 = data.frame(lag1 = lag1[[1]], lag2 = lag2[[1]])
h01 = hexbin(df01,xbins = 50)
h02 = hexbin(df02, xbins = 50)
h12 = hexbin(df12, xbins = 50)
plot(h01)
plot(h02)
plot(h12)

laglag = 6
plot(log(log(xx[1:(Tmax-laglag)])),log(log(xx[(1+laglag):Tmax])))
fitlag = lm(log(log(xx[(1+laglag):Tmax]))~log(log(xx[1:(Tmax-laglag)])))
hist(mean(xx)*fitlag$residuals-min(mean(xx)*fitlag$residuals),freq=F,breaks=25)
qqnorm(fitlag$residuals/sqrt(var(fitlag$residuals)))
lines(seq(-5,5),seq(-5,5))
var(fitlag$residuals)
mean(xx)^2*var(fitlag$residuals)

laglag = 6
plot(xn[1:(Tmax-laglag),1],xn[(1+laglag):Tmax,1])
plot(log(xn[1:(Tmax-laglag),1]),log(xn[(1+laglag):Tmax,1]))
#fitlag = lm(log(log(xn[(1+laglag):Tmax,1]))~log(log(xn[1:(Tmax-laglag),1])))
hist(mean(xn[,1])*fitlag$residuals-min(mean(xn[,1])*fitlag$residuals),freq=F,breaks=25)
qqnorm(fitlag$residuals/sqrt(var(fitlag$residuals)))
lines(seq(-5,5),seq(-5,5))
var(fitlag$residuals)
mean(xn)^2*var(fitlag$residuals)

## now we can fit the sum of a bell curve to the death rate
## and a compound poisson-gamma distribution to the birth rate

mean(xx)

#lag0 = list(log10(xx[5000:Tmax]))
#lag1 = list(log10(xx[4999:(Tmax-1)]))
#lag2 = list(log10(xx[4998:(Tmax-2)]))
#df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
#plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode='points')

rowVars = function (x,na.rm = TRUE) {
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}

xy = rowVars(xn[1:1000,])
plot(log10(xy))

plot(log10(xy[2:1000]),log10(xy[1:999]))

lag0 = list(log10(xy[3:Tmax]))
lag1 = list(log10(xy[2:(Tmax-1)]))
lag2 = list(log10(xy[1:(Tmax-2)]))
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode='lines')

############################################################################
#### Generate Gif for small pops and short time scales to demonstrate proces
############################################################################


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

#############################################################
########## The following code takes the view of an individual
########## and gives a stable distribution which describes
########## the expected contribution of an individual to the
########## population size over their lifetime
#############################################################

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

##############################################################
#### This code computes the sample mean and variance over time
##############################################################

mun = rep(0,Tmax)
varn = rep(0,Tmax)
for(i in 1:Tmax){
  mun[i] = mean(xn[i,])
  varn[i] = var(xn[i,])
}
plot(mun,type="l")
plot(varn,type="l")
plot(log10(mun),log10(varn))
pl = lm(log10(varn[50:1000])~log10(mun[50:1000]))
t = seq(3,6,.01)
lines(t,pl$coefficients[1]+pl$coefficients[2]*t)
p#hist(pl$residuals,freq=F,breaks=40)
#hist(10^pl$residuals,freq=F,breaks=30)
#plot(mun,type="l")

hist(mun,breaks=30,freq=F)

abline(v=mean(mun))
hist(log10(1+mun),breaks=20,freq=F)

###############################################
### Back to Population View - this 'zooms in' 
### on a population trajectory simulated over
### a large number of generations to show the
### self-similarity...-ish...-ness of the trajectories,
### hinting at chaos in the works. See above plot_ly
### functions which plot the embedded fractal
###############################################


##
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
pacf(xn[,1])
acf(xn[,1],lag.max=1000,ylim=c(-1,1))
hist(xn[,1],freq=F,breaks=100,main="Histogram of Population Size",ylab="Frequency Density",xlab="Number of Organisms")
abline(v=mean(xn))
hist(log10(xn)-mean(log10(xn)),freq=F,breaks=100)
abline(v=mean(log10(xn)))
hist(log10(xn)-mean(log10(xn)),freq=F,breaks=100)
abline(v=0)
#abline(v=log10(mean(xn)),col="red")
qqnorm((log10(xn)-mean(log10(xn)))/sqrt(var(log10(xn))[1]))
lines(seq(-5,5,.01),seq(-5,5,.01))

#################################
#### This chunk of code computes
#### the acf, applies a fast fourier
#### transform to get the power spectrum,
#### then plots the log10 of the
#### magnitude squared
##################################

lag.max = 500#50000
yy = acf(xn[,1],lag.max=lag.max)
pyy = Mod(fft(yy$acf))[1:(lag.max/2)]^2
fyy = seq(1,(lag.max/2))
plot(log(fyy),log(pyy),main="Log-Log Plot of Power Spectrum of Data",ylab="Log Spectral Density",xlab="Log Frequency")
lmp = lm(log(pyy[which(log(fyy)>4 & log(fyy)<5)])~log(fyy[which(log(fyy)>4 & log(fyy)<5)]))
lines(seq(1.5,3,.01),lmp$coefficients[1]+lmp$coefficients[2]*seq(1.5,3,.01),col="red",lwd=2)

plot(log(fyy),log(pyy),ylim=c(-2,10),main="Log-Log Plot of Power Spectrum of Data",ylab="Log Spectral Density",xlab="Log Frequency")
lmp = lm(log(pyy[which(log(fyy)>5 & log(fyy)<6)])~log(fyy[which(log(fyy)>5 & log(fyy)<6)]))
lines(seq(4.4,10,.01),lmp$coefficients[1]+lmp$coefficients[2]*seq(4.4,10,.01),col="red",lwd=2)
lines(seq(0,4.4,.01),1*rep(1,length(seq(0,4.4,.01))),col="red",lwd=2)
abline(v=4.4,lty=2)

lag.max=1000
yymu = acf(xx,lag.max=lag.max)
pyymu = Mod(fft(yymu$acf))[1:(lag.max/2)]^2
fyy = seq(1,(lag.max/2))
plot(log(fyy),log(pyymu),main="Log-Log Plot of Power Spectrum of Data",ylab="Log Spectral Density",xlab="Log Frequency")





#######################################################
####### complete loss of autocorrelation in the presence
####### of both chaos and stochasticity
#######################################################

############# Part 2: adding white noise


Tmax = 1000
Reps = 1000
x0 = 10^5
xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
xn[1,] = rnbinom(Reps,x0*2,.5)
lambdaB = 1
lambdaD = .5
lambdaE = 1
  
epsilon = .0001
for(i in 1:(Tmax-1)){
  for(j in 1:Reps){
    pB = lambdaB/(lambdaB+lambdaE+lambdaD+epsilon*xn[i,j])
    pD = (lambdaD+epsilon*xn[i,j])/(lambdaB+lambdaE+lambdaD+epsilon*xn[i,j])
    dB = rnorm(1,0,.0003)
    dBd = rnorm(1,0,.0003)
    B = ifelse(xn[i,j]==0,0,rpois(1,xn[i,j]*abs(pB+dB)))
    BB = sum(rnbinom(B,20,.5))
    D = rbinom(1,xn[i,j],min(abs(pD-dBd),1))
    xn[i+1,j] = ifelse(xn[i,j]==0,0, xn[i,j] + BB-D)
  }
  if(i == floor(Tmax/10)){print('10 Percent')}
  if(i == floor(Tmax/2)){print('50 Percent')}
}

plot(log10(xn[1:Tmax,1]),xlab="Reproductive Cycles",ylab="Log10 Population Size",main="Example Simulated Population",type="l")
plot(log10(xn[2:Tmax,1]),log10(xn[1:(Tmax-1),1]),xlab="Log10 Population",ylab="Log10 Population Lagged by 1 Cycle",main="Lag 1 Embedding",type="l")
plot(log10(xn[2:Tmax,1]),log10(xn[1:(Tmax-1),1]))

#plot(log10(xn[1500:Tmax,1]),log10(xn[1499:(Tmax-1),1]),type="l")
#plot(log10(xn[1500:Tmax,1]),log10(xn[1499:(Tmax-1),1]))

#plot(log10(xn[500:1000,1]),log10(xn[499:999,1]),type="l",xlim=c(5.4,5.75),ylim=c(5.4,5.75))
#plot(log10(xn[4000:5000,1]),log10(xn[3999:4999,1]),xlim=c(5.4,5.75),ylim=c(5.4,5.75))

lag0 = list(log10(xn[3:Tmax]))
lag1 = list(log10(xn[2:(Tmax-1)]))
lag2 = list(log10(xn[1:(Tmax-2)]))
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode="lines") %>%
  add_trace(
    x = lag0,
    y = lag1,
    z = lag2,
    marker = list(
      color = 'rgb(17, 157, 255)',
      size = .001,
      opacity = 0.5,
      line = list(
        color = 'rgb(231, 99, 250)',
        width = 1
      )
    ),
    showlegend = F
  )

xx = rowMeans(xn)
plot(log10(xx[1:Tmax]),xlab="Reproductive Cycles",ylab="Log10 Population Size",main="Average Population Size")

plot(log10(xx[2:Tmax]),log10(xx[1:(Tmax-1)]),xlab="Population Size",ylab="Population Size Lagged By 1 Cycle",main="Single Lag Embedding",type="l")
plot(log10(xx[2:Tmax]),log10(xx[1:(Tmax-1)]))

#plot(log10(xx[2:Tmax]),log10(xx[1:(Tmax-1)]),xlim=c(5.565,5.62),ylim=c(5.565,5.62))
#plot(log10(xx[10001:Tmax]),log10(xx[10000:(Tmax-1)]))#,xlim=c(5.565,5.62),ylim=c(5.565,5.62),type="l")

lag0 = list(log10(xx[5:Tmax]))
lag1 = list(log10(xx[4:(Tmax-1)]))
lag2 = list(log10(xx[3:(Tmax-2)]))
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode='lines',marker=list(size=2))

df01 = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]])
df02 = data.frame(lag0 = lag0[[1]], lag2 = lag2[[1]])
df12 = data.frame(lag1 = lag1[[1]], lag2 = lag2[[1]])
h01 = hexbin(df01)
h02 = hexbin(df02)
h12 = hexbin(df12)
plot(h01)
plot(h02)
plot(h12)

lag0 = list(log10(xx[1500:Tmax]))
lag1 = list(log10(xx[1499:(Tmax-1)]))
lag2 = list(log10(xx[1498:(Tmax-2)]))
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode='lines')




################ Part 3: Beta noise


Tmax = 100
Reps = 100
x0 = 10^6
xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
xn[1,] = rnbinom(Reps,x0*2,.5)
lambdaB = 1
lambdaD = .5
lambdaE = 1
epsilon = .00001

for(i in 1:(Tmax-1)){
  for(j in 1:Reps){
    B = ifelse(xn[i,j]==0,0,rpois(1,xn[i,j]*rbeta(1,lambdaB,lambdaE+lambdaD+epsilon*xn[i,j])))
    BB = sum(rnbinom(B,100,.95))
    D = rbinom(1,xn[i,j],rbeta(1,lambdaD+epsilon*xn[i,j],lambdaE+lambdaB))
    xn[i+1,j] = ifelse(xn[i,j]==0,0, xn[i,j] + BB-D)
  }
}

plot(log10(xn[1:Tmax,1]),xlab="Reproductive Cycles",ylab="Log10 Population Size",main="Example Simulated Population",type="l")
plot(log10(xn[2:Tmax,1]),log10(xn[1:(Tmax-1),1]),xlab="Log10 Population",ylab="Log10 Population Lagged by 1 Cycle",main="Lag 1 Embedding",type="l")
plot(log10(xn[2:Tmax,1]),log10(xn[1:(Tmax-1),1]))

#plot(log10(xn[1500:Tmax,1]),log10(xn[1499:(Tmax-1),1]),type="l")
#plot(log10(xn[1500:Tmax,1]),log10(xn[1499:(Tmax-1),1]))

#plot(log10(xn[500:1000,1]),log10(xn[499:999,1]),type="l",xlim=c(5.4,5.75),ylim=c(5.4,5.75))
#plot(log10(xn[4000:5000,1]),log10(xn[3999:4999,1]),xlim=c(5.4,5.75),ylim=c(5.4,5.75))

lag0 = list(log10(xn[3:Tmax]))
lag1 = list(log10(xn[2:(Tmax-1)]))
lag2 = list(log10(xn[1:(Tmax-2)]))
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode="points") %>%
  add_trace(
    x = lag0,
    y = lag1,
    z = lag2,
    marker = list(
      color = 'rgb(17, 157, 255)',
      size = .001,
      opacity = 0.5,
      line = list(
        color = 'rgb(231, 99, 250)',
        width = 1
      )
    ),
    showlegend = F
  )

xx = rowMeans(xn)
plot(log10(xx[1:Tmax]),xlab="Reproductive Cycles",ylab="Log10 Population Size",main="Average Population Size",type="l")

plot(log10(xx[2:Tmax]),log10(xx[1:(Tmax-1)]),xlab="Population Size",ylab="Population Size Lagged By 1 Cycle",main="Single Lag Embedding",type="l")
plot(log10(xx[2:Tmax]),log10(xx[1:(Tmax-1)]))

#plot(log10(xx[2:Tmax]),log10(xx[1:(Tmax-1)]),xlim=c(5.565,5.62),ylim=c(5.565,5.62))
#plot(log10(xx[10001:Tmax]),log10(xx[10000:(Tmax-1)]))#,xlim=c(5.565,5.62),ylim=c(5.565,5.62),type="l")

lag0 = list(log10(xx[3:Tmax]))
lag1 = list(log10(xx[2:(Tmax-1)]))
lag2 = list(log10(xx[1:(Tmax-2)]))
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode='lines')

df01 = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]])
df02 = data.frame(lag0 = lag0[[1]], lag2 = lag2[[1]])
df12 = data.frame(lag1 = lag1[[1]], lag2 = lag2[[1]])
h01 = hexbin(df01)
h02 = hexbin(df02)
h12 = hexbin(df12)
plot(h01)
plot(h02)
plot(h12)

lag0 = list(log10(xx[1500:Tmax]))
lag1 = list(log10(xx[1499:(Tmax-1)]))
lag2 = list(log10(xx[1498:(Tmax-2)]))
df = data.frame(lag0 = lag0[[1]], lag1 = lag1[[1]], lag2 = lag2[[1]])
plot_ly(df,x=~lag0,y=~lag1,z=~lag2, type='scatter3d',mode='lines')
