
x = matrix(rep(0,10000000),nrow=100,ncol=10000)
N = rpois(10000,5)
for(j in 1:10000){
  x[1,j] = sum(rgamma(N[j],3,3))-lambda*alpha/beta
}
#x[1,] = rgamma(10000,3,3)
lambda = 5
alpha = 3
beta = 3
for(i in 2:100){
  N = rpois(10000,lambda)
  dx = rep(0,10000)
  for(j in 1:10000){
    dx[j] = ifelse(N[j]==0,0,sum(rgamma(N[j],alpha,beta)-alpha/beta)/(i*lambda*alpha*(1+alpha)/beta^2))
  }
  x[i,] = x[i-1,] + dx
}

plot(x[1,],type="l")

hist(x[1,],breaks=30,freq=F,ylim=c(0,.2),xlim=c(-6,10))
hist(x[2,],breaks=30,freq=F,xlim=c(-6,10),ylim=c(0,.2))
hist(x[3,],breaks=30,freq=F,xlim=c(-6,10),ylim=c(0,.2))
hist(x[5,],breaks=30,freq=F,xlim=c(-6,10),ylim=c(0,.2))
hist(x[30,],breaks=30,freq=F,xlim=c(-6,10),ylim=c(0,.2))
hist(x[50,],breaks=30,freq=F,xlim=c(-6,10),ylim=c(0,.2))
hist(x[100,],breaks=30,freq=F,xlim=c(-6,10),ylim=c(0,.2))

hist(x[100,],breaks=30,freq=F,xlim=c(-6,10),ylim=c(0,.2))
hist(log10(x[100,]-min(x[100,])),freq=F)

y = seq(0,100-.01,.01)/100
hist(Mod(fft(x[100,]))^2,breaks=50,freq=F,main="Spectral Density")

plot(y,log10(rev((cumsum(sort(Mod(fft(x[100,]))^2))/sum(Mod(fft(x[100,]))^2)))),type="l",xlab="Frequency",ylab="Log Spectral Density",main="Log of Spectral Density of ED Process",xlim=c(0,.5),ylim=c(-1,0))

length(y)
length(Mod(fft(x[100,]))^2)
