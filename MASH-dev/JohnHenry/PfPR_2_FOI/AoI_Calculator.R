################    Model Simulation + Functions   #############################
################ For Age of Infection Modification #############################
################  to Standard Ross-Macdonald Model #############################


###################################################################################
######## Base AoI Model (discrete time, no exposed class, no treatment) ###########
###################################################################################

########################## Time Series Simulation

Tmax = 500
lambda = .1
## 10 day time increments, each day has a rate of recovery of 1/200 (assumed)
r = 10/200
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

PR2FOI_EQ_AoI = function(PR,r){
  FOI = (-(1+(1-2*PR)/PR*r) + sqrt((1+(1-2*PR)/PR*r)^2+4*PR/(1-PR)*r))/2
  return(FOI)
}

PR2AR_EQ_AoI = function(PR,r){
  lambda = PR2FOI_AoI(PR,r)
  1-exp(-lambda)
}

x = seq(0,1,.001)
plot(x,PR2FOI_EQ_AoI(x,200/365*10),type="l",xlab="Prevalence",ylab="Force of Infection (Infectious Bites per Fortnight)",ylim=c(0,2),xlim=c(0,.5))
plot(x,PR2FOI_EQ_AoI(x,200/365*10),type="l",xlab="Prevalence",ylab="Force of Infection (Infectious Bites per Fortnight)",ylim=c(0,10))
plot(x,PR2AR_EQ_AoI(x,200/365*10),type="l",xlab="Prevalence",ylab="Attack Rate (per Forntight)")
abline(v = .5, h=c(0,1),lty=2)

##################### Quasistationary Model for AoI Dist'n ##########################

## Forward Equation - needs second order terms for robustness
## currently, if lambda0 > PR, the equation blows up
FOI_FW_AoI = function(PR,r,lambda0){
  At = 1-exp(-lambda0)
  Bt = 1-exp(-(r+1))
  LHS = At/PR + (1-Bt)*(lambda0+r)/(lambda0+r+1)
  ((1+r)*LHS-r)/(1-LHS)
}

## Backward Equation

## Time Series Equations
## this calculates the first AR assuming equilibrium, then
## uses this value with PR time series in the forward equation
## to calculate a time series of AR

## PR to FOI
PR2FOI_TS_AoI = function(PRTS,r){
  lambda = rep(0,PRTS)
  x0 = PRTS[1]
  lambda[1] = PR2FOI_EQ_AoI(x0,r)
  for(i in 1:(length(PRTS)-1)){
    lambda[i+1] = FOI_FW_AoI(PRTS[i+1],r,lambda[i])
  }
  return(lambda)
}

## PR to AR
PR2AR = function(PRTS,r){
  lambda = rep(0,PRTS)
  x0 = PRTS[1]
  lambda[1] = PR2FOI_EQ_AoI(x0,r)
  for(i in 1:(length(PRTS)-1)){
    lambda[i+1] = FOI_FW_AoI(PRTS[i+1],r,lambda[i])
  }
  return(1-exp(-lambda))
}


#####################################################################################
#################### Model With Exposed Class, No Treatment #########################
#####################################################################################


Tmax = 500
lambda = .1
r = 10/200
N = 100
x = matrix(0,nrow=Tmax,ncol=N)
E = matrix(0,nrow=Tmax,ncol=N)
for(i in 1:(Tmax-1)){
  x[i+1,1] = 1/(1+lambda)*sum(E[i,])
  x[i+1,2:(N-1)] = 1/(1+lambda+r)*x[i,1:(N-2)]
  x[i+1,N] = 1/(1+lambda+r)*(x[i,N-1]+x[i,N])
  E[i+1,1] = lambda/(1+lambda)*(1-sum(x[i,])-sum(E[i,2:N]))
  E[i+1,2] = lambda/(1+lambda)*(sum(E[i,])+x[i,1])
  E[i+1,3:(N-1)] = lambda/(1+lambda+r)*x[i,2:(N-2)]
  E[i+1,N] = lambda/(1+lambda+r)*(x[i,N-1] + x[i,N])
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
r = 10/200
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

# plot total prevalence over time
X = rowSums(x)
plot(X,xlim=c(0,50),ylim=c(0,1),type="l")

# plot distribution of AoI at Tmax
plot(x[Tmax,])



#####################################################################################
#################### Model With Treatment and Exposed Class #########################
#####################################################################################


Tmax = 500
lambda = .1
r = 10/200
rho = 1
## number of age bins to include
N = 100
## infected classes; x[,i] is infected with age i time intervals
x = matrix(0,nrow=Tmax,ncol=N)
## exposed classes; E[,1] is exposed and uninfected, E[,i] is exposed and infected
## with infection of age i-1 time intervals
E = matrix(0,nrow=Tmax,ncol=N)
## chemoprotected class
C = rep(0,Tmax)
for(i in 1:(Tmax-1)){
  x[i+1,1] = 1/(1+lambda)*sum(E[i,])
  x[i+1,2:(N-1)] = 1/(1+lambda+r)*x[i,1:(N-2)]
  x[i+1,N] = 1/(1+lambda+r)*(x[i,N-1]+x[i,N])
  E[i+1,1] = lambda/(1+lambda)*(1-sum(x[i,])-sum(E[i,2:N])-C[i])
  E[i+1,2] = lambda/(1+lambda+rho)*(sum(E[i,2:N])+x[i,1])+lambda/(1+lambda)*E[i,1]
  E[i+1,3:(N-1)] = lambda/(1+lambda+r+rho)*x[i,2:(N-2)]
  E[i+1,N] = lambda/(1+lambda+r+rho)*(x[i,N-1] + x[i,N])
  ## here we're assuming all classes have equal likelyhood of treatment;
  ## this can be relaxed
  C[i+1] = rho/(1+lambda+rho)*sum(E[i,2:N]) + rho/(1+lambda+r+rho)*sum(x[i,]) 
}

plot(rowSums(x)+rowSums(E[,2:N]),type="l",xlim=c(0,50),ylim=c(0,1))

## plot AoI for both infected and exposed classes
plot(x[Tmax,])
plot(E[Tmax,])

##plot chemoprotected fraction
plot(C)
