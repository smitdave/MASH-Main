
source ("macroRM-ODE.R")

H = c(92, 21, 10, 20, 51)


par = c(RMparams(Q=.1), N=15) 
Hi = rep(c(20, 40, 30),5) 
Li = c(400, rep(0.001, 14))/2
alpha = c(1:15)/50
inits = closedRMEq.Inits(Li,Hi,alpha,par, nudge=0.02) 
#Xo = rep(c(2, 4, 1),5)
#Mo = Li*10
#Yo = Xo*0
sigma = matrix(0.01, 15, 15)-diag(.01, 15)
#sigma = sigma/rowSums(sigma)

alpha = rep(0.3, 15)

psi = matrix(.3/14, 15, 15) -diag(.3/14,15) +diag(.7, 15)

test.eq = RMMacro0.Eq(Li, Hi, psi, alpha, par)
test.steady = RMMacro0.steady(Li, Hi, psi, alpha, par)

Omega = matrix(
  c(rep(c(.3, .3, .4), 5),
    rep(c(.3, .1, .5),5),
    rep(c(.4, .6, .1),5)), 15, 3) 

#out <- lsode(c(Xo,Mo,Yo), 0:3650, macroRMode.derivs, par, H=Hi, L=Li,psi=psi, sigma=sigma) 
#out.eq <- steady(c(Xo,Mo,Yo), 0:3650, macroRMode.derivs, par, H=Hi, L=Li,psi=psi, sigma=sigma) 
#eqs = macroRMode.eq(Li, Hi, psi, sigma, alpha, par)
#out1=RMEqSummary(eqs,Hi,alpha,Omega,par)

#out1=RMEqSummary(eqs,Hi,alpha,Omega,par)

inits = closedRMEq(Li,Hi,alpha,par)
