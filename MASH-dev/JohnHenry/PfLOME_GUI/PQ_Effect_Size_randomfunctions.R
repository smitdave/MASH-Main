EffectSize = rep(0,10)
for(i in 1:10){
  EffectSize[i] = PQsim((i-1)/10,1000)
}

f1 = function(t){
  (15.3-29.14)*t+29.14
}

f2 = function(t){
  (11.3-29.14)*t+29.14
}

t = seq(0,1,.01)

plot(t,f2(t)/f1(t),type="l",ylim=c(0,1),ylab="Proportional Reduction in Transmission-Days",xlab="Proportion of Population Treated")
abline(h=.74,lty=2)

x = seq(0,12,.01)
sigma = function(x,a,b){
  (x/b)^a/(1+(x/b)^a)
}

plot(x,sigma(x,8,7),type="l",ylab="Transmission Efficiency",xlab="log10 Gametocyte Numbers in the Blood",ylim=c(0,1))

