###################################################################################
######## Base AoI Model (discrete time, no exposed class, no treatment) ###########
###################################################################################

########################## Time Series Model

Tmax = 500
lambda = .1
r = 200/365*14/365
N = 100
x = matrix(0,nrow=Tmax,ncol=N)
for(i in 1:(Tmax-1)){
  x[i+1,1] = lambda/(1+lambda+r)*sum(x[i,]) + lambda/(1+lambda)*(1-sum(x[i,]))
  x[i+1,2:(N-1)] = 1/(1+lambda+r)*x[i,1:(N-2)]
  x[i+1,N] = 1/(1+lambda+r)*(x[i,N-1]+x[i,N])
}

# plot total prevalence
X = rowSums(x)
Xbar = lambda*(1+lambda+r)/(r*(1+lambda)+lambda*(1+lambda+r))
plot(X,xlim=c(0,50),ylim=c(0,1),type="l")
abline(h=Xbar,lty=2)

# plot AoI distribution at Tmax
plot(x[Tmax,])
Xbar0 = lambda*(lambda+r)/(r*(1+lambda)+lambda*(1+lambda+r))
p = 1/(1+lambda+r)
lines(1:N,Xbar0*p^(0:(N-1)),lty=2)

########################### Equilibrium Functions: #################################

PR2FOI_AoI = function(PR,r){
  FOI = (-(1+(1-2*PR)/PR*r) + sqrt((1+(1-2*PR)/PR*r)^2+4*PR/(1-PR)*r))/2
  return(FOI)
}

PR2AR_AoI = function(PR,r){
  lambda = PR2FOI_AoI(PR,r)
  1-exp(-lambda)
}

x = seq(0,1,.001)
plot(x,PR2FOI_AoI(x,200/365*10),type="l",xlab="Prevalence",ylab="Force of Infection (per Fortnight)")
plot(x,PR2AR_AoI(x,200/365*10),type="l",xlab="Prevalence",ylab="Attack Rate (per Forntight)")
plot(log10(x/(1-x)),log10(PR2AR_AoI(x,200/365*10)),xlab="log10 Odds Ratio of Infection",ylab="log10 Attack Rate",type="l")
abline(v=0)

##################### Quasistationary Model for AoI Dist'n ##########################



#####################################################################################
#################### Model With Exposed Class, No Treatment #########################
#####################################################################################


Tmax = 500
lambda = .1
r = 200/365*14/365
N = 100
x = matrix(0,nrow=Tmax,ncol=N)
E = matrix(0,nrow=Tmax,ncol=N)
for(i in 1:(Tmax-1)){
  x[i+1,1] = 1/(1+lambda)*sum(E[i,])
  x[i+1,2:(N-1)] = 1/(1+lambda+r)*x[i,1:(N-2)]
  x[i+1,N] = 1/(1+lambda+r)*(x[i,N-1]+x[i,N])
  E[i+1,1] = lambda/(1+lambda)*(1-sum(x[i,])-sum(E[i,]))
  E[i+1,2] = lambda/(1+lambda)*(sum(E[i,])+x[i,1])
  E[i+1,3:N] = lambda/(1+lambda+r)*x[i,3:N]
}

plot(rowSums(x)+rowSums(E[,2:N]),type="l",xlim=c(0,50),ylim=c(0,1))

## plot AoI for both infected and exposed classes
plot(x[Tmax,])
plot(E[Tmax,])



#####################################################################################
#################### Model With Treatment, No Exposed Class #########################
#####################################################################################


Tmax = 500
lambda = .1
r = 200/365*14/365
N = 100
rho = .1
w = 1
x = matrix(0,nrow=Tmax,ncol=N)
C = rep(0,Tmax)

#### Here we assume that chemoprotection duration is geometrically distributed with
#### an expected value of w/(1+w) fortnights (usually about 2?)

for(i in 1:(Tmax-1)){
  x[i+1,1] = lambda/(1+lambda+r+rho)*sum(x[i,]) + lambda/(1+lambda)*(1-C[i]-sum(x[i,]))
  x[i+1,2:(N-1)] = 1/(1+lambda+r+rho)*x[i,1:(N-2)]
  x[i+1,N] = 1/(1+lambda+r)*(x[i,N-1]+x[i,N])
  C[i+1] = rho/(1+lambda+r+rho)*x[i,1] + w/(1+w)*C[i]
}

# plot total prevalence
X = rowSums(x)
plot(X,xlim=c(0,50),ylim=c(0,1),type="l")

plot(x[Tmax,])



#####################################################################################
#################### Model With Treatment and Exposed Class #########################
#####################################################################################




