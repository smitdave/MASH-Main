
for(k in 1:100){

lambda = 2
alpha = 3
beta = 3

x = matrix(rep(0,10000000),nrow=1000,ncol=1000)
N = rpois(1000,5)
for(j in 1:1000){
  x[1,j] = sum(rgamma(N[j],3,3)-1)
}
#x[1,] = rgamma(10000,3,3)
for(i in 2:1000){
  N = rpois(1000,lambda)
  dx = rep(0,1000)
  for(j in 1:1000){
    dx[j] = ifelse(N[j]==0,0,sum(rgamma(N[j],alpha,beta))-lambda*alpha/beta-.3125)
  }
  x[i,] = x[i-1,] + dx
}

plot(x[,1],type="l")

hist(x[1,],breaks=30,freq=F,ylim=c(0,.4),xlim=c(-4,6))
hist(x[10,],breaks=30,freq=F)#xlim=c(-4,6),ylim=c(0,.4))
hist(x[100,],breaks=30,freq=F)#xlim=c(-4,6),ylim=c(0,.4))
hist(x[200,],breaks=30,freq=F)#,xlim=c(-4,6),ylim=c(0,.4))
hist(x[500,],breaks=30,freq=F)#,xlim=c(-4,6),ylim=c(0,.4))
hist(x[800,],breaks=30,freq=F)#,xlim=c(-4,6),ylim=c(0,.4))
hist(x[1000,],breaks=30,freq=F)#,xlim=c(-4,6),ylim=c(0,.4))

varPG = rep(0,1000)
muPG = rep(0,1000)
medPG = rep(0,1000)
for(i in 1:1000){
  varPG[i] = var(x[i,])
  muPG[i] = mean(x[i,])
  medPG[i] = median(x[i,])
}
plot(varPG,type="l")
plot(muPG,type="l")
plot(medPG,type="l")

muPG1000 = c(muPG1000,muPG[1000])
}

hist(x[1000,],breaks=30,freq=F)
hist(log10(x[1000,]+sign(min(x[1000,]))*min(x[1000,])),freq=F,breaks=50)

y = seq(0,1-.001,.001)
hist(Mod(fft(x[1000,]))^2/sum(Mod(fft(x[100,]))^2),breaks=50,freq=F,main="Spectral Density")

plot(log10(y),-log10(1-rev((cumsum(sort(Mod(fft(x[100,]))^2))/sum(Mod(fft(x[100,]))^2)))),xlab="Log10 Frequency",ylab="Log10 Spectral Density",main="Log of Spectral Density of ED Process",ylim=c(0,4.5),xlim=c(-4,0))#,xlim=c(0,.5),ylim=c(-1,0))
z = seq(.001,.1,.001)
lines(log10(z),log10(1/z))
