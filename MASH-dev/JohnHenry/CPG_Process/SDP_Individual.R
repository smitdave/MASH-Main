N = rep(100,100000)
pb = rep(1,100000)

for(i in 1:(length(N)-1)){
  pb[i] = 1/(5+.1*N[i])
  N[i+1] = ifelse(rbinom(1,1,pb[i])==1,N[i]+rnbinom(1,10,.4),N[i]-1)
  if(N[i+1]==0){
    break
  }
}
plot(N,type="l")
hist(N,breaks=50,freq=F)
abline(v=mean(N))
qqnorm((N-mean(N))/sqrt(var(N)))
lines(seq(-5,5),seq(-5,5))

lag.max = 5000
acfN = acf(N,lag.max=lag.max)
f = seq(1,lag.max/2)/lag.max
pyy = Mod(fft(acfN$acf))[1:(lag.max/2)]^2
plot(log(f),log(pyy))

lm(log(pyy[which((log(f)>(-4))&(log(f)<(-2)))])~log(f[which((log(f)>-4)&(log(f)<(-2)))]))
lines(log(f),-3.646-1.971*log(f))

