

################# Beta Noise for probability of events, Power Law ST p = 2


Tmax = 1000
Reps = 1000
x0 = 19000
xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
xn[1,] = x0
lambdaB = 1
lambdaD = .5#.001
lambdaE = 1
epsilon = .01 
mu = rep(0,Tmax)
mu[1] = x0
  
for(i in 1:(Tmax-1)){
  for(j in 1:Reps){
      #B = ifelse(xn[i,j]==0,0,rbinom(1,xn[i,j],pB[i,j]))
      B = ifelse(xn[i,j]==0,0,rpois(1,mu[i]*rbeta(1,lambdaB,lambdaE+lambdaD+epsilon*mu[i])))
      BB = sum(rnbinom(B,100,.4))
      D = rbinom(1,floor(mu[i]),rbeta(1,lambdaD+epsilon*mu[i],lambdaE+lambdaB))
      xn[i+1,j] = ifelse(xn[i,j] <= 0, 0, xn[i,j] + BB - D)
      xn[i,j] = ifelse(xn[i,j]<=0,0,xn[i,j])
  }
  mu[i+1] = mean(xn[i+1,])
}

ps = acf(mu[2:length(mu)],lag.max=200)
pn = Mod(fft(ps$acf))[1:100]^2
plot(log10(seq(1:100)),log10(pn)[1:100])
pnlm = lm(log10(pn[2:50])~log10(2:50))
lines(seq(-1,3),pnlm$coefficients[1]+seq(-1,3)*pnlm$coefficients[2])

plot(mu[2:100],type="l")
plot(log10(mu[2:100]),type="l")


var(mu[10:Tmax])

plot(log10(mu[10:Tmax]),ylim=c(4.17,4.179),type="l")



################### White noise added to the probability of events, power law ST 1 < p < 2


Tmax = 1000
Reps = 1000
x0 = 19000
xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
xn[1,] = x0
lambdaB = 1
lambdaD = .5#.001
lambdaE = 1
epsilon = .01
mu = rep(0,Tmax)
mu[1] = x0

for(i in 1:(Tmax-1)){
  for(j in 1:Reps){
      pB = lambdaB/(lambdaB+lambdaE+lambdaD+epsilon*mu[i])
      pD = (lambdaD+epsilon*mu[i])/(lambdaB+lambdaE+lambdaD+epsilon*mu[i])
      dB = rnorm(1,0,.0003)
      B = rpois(1,mu[i]*abs(pB+dB))
      BB = sum(rnbinom(B,100,.4))
      D = rbinom(1,floor(mu[i]),min(abs(pD-dB),1))
      xn[i+1,j] = ifelse(xn[i,j] <= 0, 0, xn[i,j] + BB - D)
      xn[i,j] = ifelse(xn[i,j]<=0,0,xn[i,j])
  }
  mu[i+1] = mean(xn[i+1,]) 
}

ps = acf(mu[10:Tmax],lag.max=200)
pn = Mod(fft(ps$acf))[1:100]^2
plot(log10(1:100),log10(pn))
pnlm = lm(log10(pn)[1:50]~log10(1:50))
lines(seq(-1,3),pnlm$coefficients[1]+seq(-1,3)*pnlm$coefficients[2])
## true pink noise - 1/f

plot(mu[2:100],type="l")
plot(log10(mu[2:100]),type="l")

var(mu[10:Tmax])

plot(log10(mu[10:Tmax]),ylim=c(4.16,4.19),type="l")


##################### No Noise in Probability of Events, Power Law ST p = 1


Tmax = 1000
Reps = 1000
x0 = 1450
xn = matrix(rep(0,Tmax*Reps),nrow=Tmax,ncol=Reps)
xn[1,] = x0
lambdaB = 1
lambdaD = .5
lambdaE = 1
epsilon = .01
mu = rep(0,Tmax)
mu[1] = x0
  
for(i in 1:(Tmax-1)){
  for(j in 1:Reps){
      B = rpois(1,mu[i]*(lambdaB/(lambdaB+lambdaE+lambdaD+epsilon*mu[i])))
      BB = sum(rnbinom(B,10,.4))
      D = rbinom(1,floor(mu[i]),(lambdaD+epsilon*mu[i])/(lambdaB+lambdaE+lambdaD+epsilon*mu[i]))
      xn[i+1,j] = ifelse(xn[i,j] <= 0, 0, xn[i,j] + BB - D)
      xn[i,j] = ifelse(xn[i,j]<=0,0,xn[i,j])
  }
  mu[i+1] = mean(xn[i+1,])
}

ps = acf(mu[2:length(mu)],lag.max=200)
pn = Mod(fft(ps$acf))[1:100]^2
plot(log10(1:100),log10(pn))
pnlm = lm(log10(pn)[3:100]~log10(3:100))
lines(seq(-1,3),pnlm$coefficients[1]+seq(-1,3)*pnlm$coefficients[2])
## red shifted noise
plot(mu[2:100],type="l")
plot(log10(mu[2:100]),type="l")

plot(xn[,5])
xnacf = acf(xn[,1],lag.max=200)
plot(log10(1:100),log10(Mod(fft(xnacf$acf)[1:100])^2))


## p1 is variance for compound poisson process (CPP) without exogeneous noise
## p2 " " with white noise
## p3 " " with beta noise
p1 = c(22584,10292,5933,2854,1388,686,348,184,83)
p2 = c(464099,226843,111039,57154,29892,13272,7135,3842,1772)
p3 = c(2232956,1025253,561703,260952,137281,67143,32306,17455,8752)
n = c(100,200,400,800,1600,3200,6400,12800,26500)

plot(log2(n),log2(p1),ylim=c(0,25))
lmp1 = lm(log2(p1)~log2(n))
lines(seq(0,20),lmp1$coefficients[1] + lmp1$coefficients[2]*seq(0,20))
points(log2(n),log2(p2))
lmp2 = lm(log2(p2)~log2(n))
lines(seq(0,20),lmp2$coefficients[1] + lmp2$coefficients[2]*seq(0,20))
points(log2(n),log2(p3))
lmp3 = lm(log2(p3)~log2(n))
lines(seq(0,20),lmp3$coefficients[1] + lmp3$coefficients[2]*seq(0,20))

