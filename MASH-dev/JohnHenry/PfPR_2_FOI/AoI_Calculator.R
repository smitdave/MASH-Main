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
AoI_Dist = x[Tmax,]
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
  lambda = PR2FOI_EQ_AoI(PR,r)
  1-exp(-lambda)
}

x = seq(0,1,.001)
plot(x,PR2FOI_EQ_AoI(x,200/365*10),type="l",xlab="Prevalence",ylab="Force of Infection (Infectious Bites per Fortnight)")
plot(x,PR2FOI_EQ_AoI(x,200/365*10),type="l",xlab="Prevalence",ylab="Force of Infection (Infectious Bites per Fortnight)",ylim=c(0,10))
plot(x,PR2AR_EQ_AoI(x,200/365*10),type="l",xlab="Prevalence",ylab="Attack Rate (per Forntight)")
abline(v = .5, h=c(0,1),lty=2)

##################### Quasistationary Model for AoI Dist'n ##########################

## Forward Equation - needs second order terms for robustness
## currently, if lambda0 >~ PR, the equation blows up
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
N = 1000
x = matrix(0,nrow=Tmax,ncol=N)
E = matrix(0,nrow=Tmax,ncol=N)
for(i in 1:(Tmax-1)){
  x[i+1,1] = 1/(1+lambda)*sum(E[i,])
  x[i+1,2:(N-1)] = 1/(1+lambda+r)*x[i,1:(N-2)]
  x[i+1,N] = 1/(1+lambda+r)*(x[i,N-1]+x[i,N])
  E[i+1,1] = lambda/(1+lambda)*(1-sum(x[i,])-sum(E[i,]))
  E[i+1,2] = lambda/(1+lambda)*(sum(E[i,]))
  E[i+1,3:(N-1)] = lambda/(1+lambda+r)*x[i,1:(N-3)]
  E[i+1,N] = lambda/(1+lambda+r)*sum(x[i,(N-2):N])
}

plot(seq(0,50,1),rowSums(x)[1:51]+rowSums(E[1:51,2:N]),type="l",xlim=c(0,50),ylim=c(0,1))


lam = seq(0,50,.01)
x1 = function(lambda,r) {
  lambda/((1+lambda)^2+lambda*r/(lambda+r))
}
plot(lam,x1(lam,r),type="l",xlim=c(0,30))
## plot AoI for both infected and exposed classes
plot(x[Tmax,],xlim=c(0,100))
plot(E[Tmax,],xlim=c(0,100))
E0 = function(lambda,r){
  lambda/(1+lambda)*(1/(lambda+1)*(1+lambda^2*(lambda+r+1)/((1+lambda)^2*(lambda+r)+lambda*r))-lambda*(2*lambda+r+1)/((1+lambda)^2*(lambda+r)+lambda*r))
}
E1 = function(lambda,r){
  lambda/(1+lambda)*(lambda/(1+lambda)*(1-lambda*(lambda+r+1)/((lambda+1)^2*(lambda+r)+lambda*r)) + lambda^2/((lambda+1)^2*(lambda+r)+lambda*r))
}
xk = function(lambda,r,k){
  x1(lambda,r)*(1/(1+lambda+r))^(k-1)
}

##compare simulation to predicted AoI dist'n
plot(x[Tmax,],xlim=c(0,50),main="Equilibrium Age of Infection",xlab="Age (10 Day Increments)",ylab="Fraction of Population")
lines(xk(lambda,r,1:50))

Ek = function(lambda,r,k){
  Ekk = rep(0,length(k))
  k0 = which(k==1)
  k1 = which(k==2)
  kk = which(k>2)
  ek0 = E0(lambda,r)
  ek1 = E1(lambda,r)
  xkk = xk(lambda,r,kk-2)
  Ekk[k0] = ek0
  Ekk[k1] = ek1
  Ekk[kk] = xkk*(lambda/(1+lambda+r))
  return(Ekk)
}
##compare simulation to predicted Exposed AoI dist'n
plot(E[Tmax,],xlim=c(0,50),main="Equilibrium Age of Infection, Exposed Categories",ylab="Fraction of Population",xlab="Age (10 Day Increments)")
lines(Ek(lambda,r,seq(1:50)))

plot(lam,E0(lam,r),type="l",main="Fraction Exposed and Uninfected",ylab="Fraction of Population",xlab="Force of Infection")
plot(lam,E1(lam,r),type="l",main="Fraction Exposed with New Infection",ylab="Fraction of Population",xlab="Force of Infection")

Xbar = function(lambda,r){
  E1(lambda,r)+x1(lambda,r)*(1+(lambda+1)/(lambda+r))
}

plot(seq(0,50,1),rowSums(x)[1:51]+rowSums(E[1:51,2:N]),type="l",xlim=c(0,50),ylim=c(0,1),main="Simulation Time Series to Equilibrium",xlab="Time (10 Day Increments)",ylab="Fraction Infected")
abline(h = Xbar(lambda,r),lty=2)

lam = seq(0,10,.01)
plot(lam,Xbar(lam,r),type="l",ylab="Prevalence",xlab="FOI",ylim=c(0,1),main="Equilibrium Prevalence vs FOI")
lines(lam,E1(lam,r)+x1(lam,r),col="red",lty=2)
legend(5,.5,legend=c("Total Prevalence","New Infections"),col=c("black","red"),lty=1:2)

plot(lam,(E1(lam,r)+x1(lam,r))/Xbar(lam,r),type="l",ylab="Fraction",xlab="FOI",main="Fraction of Young Infections",ylim=c(0,1))

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
points(AoI_Dist,col="blue")
plot(AoI_Dist-x[Tmax,])

#####################################################################################
#################### Model With Treatment and Exposed Class #########################
#####################################################################################

Tmax = 500
lambda = 5
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

plot(rowSums(x)+rowSums(E[,2:N]),type="l",xlim=c(0,50),ylim=c(0,5))
abline(h=1)

## plot AoI for both infected and exposed classes
plot(x[Tmax,])
plot(E[Tmax,])

##plot chemoprotected fraction
plot(C)

### * Note: This section will 'vectorize' the previous model and analyze the resulting
### system in terms of the matrix; equilibrium analysis will then be simplified to a
### single matrix inversion.

rho = c(2,1,.5,rep(.001,(N-3)))
M = function(lambda,r,rho){
  M0 = matrix(rep(0,2*(2*N+1)),nrow=2*N+1)
  # E_0
  M0[1,] = -lambda/(1+lambda)*rep(1,2*N+1)
  # E_1
  M[2,1] = lambda/(1+lambda)
  M[2,2:(N+1)] = lambda/(1+lambda+rho[1:N])
  # E_k
  M[3:(N-1)] = lambda/(1+lambda+r+rho[1:])
}