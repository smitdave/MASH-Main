Tmax = 10000#5000
Reps = 1#1000#10000#1
xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
x0 = 10000
xn[1,] = rnbinom(Reps,x0*2,.5)
lambdaB = 1
lambdaD = 1#.001
epsilon = .001
pB = matrix(rep(0,Reps*Tmax),nrow=Tmax,ncol=Reps)
pB[1,] = lambdaB/(lambdaB+lambdaD+epsilon*x0)

mean = rep(0,50)
var = rep(0,50)


################# Beta Noise for probability of events, Power Law ST p = 2


for(k in 1:50){
  Tmax = 2000#5000
  Reps = 1#1000#10000#1
  x0 = 10000
  xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
  xn[1,] = rnbinom(Reps,x0*2,.5)
  yn = xn*0
  lambdaB = 1
  lambdaD = .5#.001
  lambdaE = 1
  
  epsilon = .0001 + .0001*k
    for(i in 1:(Tmax-1)){
    for(j in 1:Reps){
      #B = ifelse(xn[i,j]==0,0,rbinom(1,xn[i,j],pB[i,j]))
      B = ifelse(xn[i,j]==0,0,rpois(1,xn[i,j]*rbeta(1,lambdaB,lambdaE+lambdaD+epsilon*xn[i,j])))
      BB = sum(rnbinom(B,100,.4))
      D = rbinom(1,xn[i,j],rbeta(1,lambdaD+epsilon*xn[i,j],lambdaE+lambdaB))
      xn[i+1,j] = ifelse(xn[i,j]==0,0, xn[i,j] + BB-D)
    }
  }
  mean[k] = mean(xn[500:Tmax])
  var[k] = var(xn[500:Tmax])
}

plot(log10(mean)[which(mean>0)],log10(var)[which(mean>0)])

mvpl = lm(log10(var)[which(mean>1000)]~log10(mean)[which(mean>1000)])
lines(seq(-10,10),mvpl$coefficients[1]  + seq(-10,10)*mvpl$coefficients[2])

qqnorm((log10(xn[100:Tmax])-mean(log10(xn[100:Tmax])))/sqrt(var(log10(xn[100:Tmax]))[1]))
lines(seq(-5,5),seq(-5,5))

hist(xn[100:Tmax],breaks=10)
hist(log10(xn),breaks=20)
#qqnorm((log10(xn)-mean(log10(xn)))/sqrt(var(log10(xn))[1]))
#lines(-5:5,-5:5)




################### White noise added to the probability of events, power law ST 1 < p < 2


for(k in 1:50){
  Tmax = 2000#5000
  Reps = 1#1000#10000#1
  x0 = 10000
  xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
  xn[1,] = rnbinom(Reps,x0*2,.5)
  lambdaB = 1
  lambdaD = .5#.001
  lambdaE = 1
  
  epsilon = .0001 + .0001*k
  for(i in 1:(Tmax-1)){
    for(j in 1:Reps){
      pB = lambdaB/(lambdaB+lambdaE+lambdaD+epsilon*xn[i,j])
      pD = (lambdaD+epsilon*xn[i,j])/(lambdaB+lambdaE+lambdaD+epsilon*xn[i,j])
      dB = rnorm(1,0,.0003)
      B = ifelse(xn[i,j]==0,0,rpois(1,xn[i,j]*abs(pB+dB)))
      BB = sum(rnbinom(B,100,.4))
      D = rbinom(1,xn[i,j],min(abs(pD-dB),1))
      xn[i+1,j] = ifelse(xn[i,j]==0,0, xn[i,j] + BB-D)
    }
  }
  mean[k] = mean(xn[500:Tmax])
  var[k] = var(xn[500:Tmax])
  if(k==1){
    xnhat = xn
  }
}

plot(log10(mean)[which(mean>0)],log10(var)[which(mean>0)])

mvpl = lm(log10(var)[which(mean>0)]~log10(mean)[which(mean>0)])
lines(seq(-10,10),mvpl$coefficients[1]  + seq(-10,10)*mvpl$coefficients[2])

plot(xn[100:2000])
hist(xn[100:2000])
hist(log10(xn[100:2000]),breaks=20)


##################### No Noise in Probability of Events, Power Law ST p = 1


for(k in 1:50){
  Tmax = 2000#5000
  Reps = 1#1000#10000#1
  x0 = 10000
  xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
  xn[1,] = rnbinom(Reps,x0*2,.5)
  lambdaB = 1
  lambdaD = .5#.001
  lambdaE = 1
  
  epsilon = .0001 + .0001*k
  for(i in 1:(Tmax-1)){
    for(j in 1:Reps){
      B = ifelse(xn[i,j]==0,0,rpois(1,xn[i,j]*(lambdaB/(lambdaB+lambdaE+lambdaD+epsilon*xn[i,j]))))
      BB = sum(rnbinom(B,100,.4))
      D = rbinom(1,xn[i,j],(lambdaD+epsilon*xn[i,j])/(lambdaB+lambdaE+lambdaD+epsilon*xn[i,j]))
      xn[i+1,j] = ifelse(xn[i,j]<=0,0, xn[i,j] + BB-D)
    }
  }
  mean[k] = mean(xn[500:Tmax])
  var[k] = var(xn[500:Tmax])
}

plot(log10(mean)[which(mean>0)],log10(var)[which(mean>0)])

mvpl = lm(log10(var)[which(mean>1000)]~log10(mean)[which(mean>1000)])
lines(seq(-10,10),mvpl$coefficients[1]  + seq(-10,10)*mvpl$coefficients[2])

plot(xn[100:2000])
hist(xn[100:2000])
hist(log10(xn[100:2000]),breaks=20)


###################################

