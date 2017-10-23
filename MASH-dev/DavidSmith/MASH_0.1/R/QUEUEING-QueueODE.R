#################################################################
#
#   MASH
#   R6-ified
#   Generic Bite Queueing Algorithms for SimBite
#   David Smith, Hector Sanchez, Sean Wu
#   June 13, 2017
#
#################################################################


# interaction > 0 competition for host resources; interaction < 0 facilitation against host immune system
oneHuman = function(biteRate = 1/50, recoveryRate = 1/200, interaction = 0, tMax = 1000){
  MOI = 0
  time = 0
  while(time < tMax){
    # sample which event happens next; ie what happens first? (Gillespie algorithm)
    nextBite = time + rexp(1,biteRate)

    if(MOI>0){
      nextClearance = time + rexp(1,(recoveryRate) * (MOI^interaction))
      event = which.min(x = c(nextBite, nextClearance))
      if(event == 1){
        # infection
        MOI = MOI + 1
        time = time + nextBite
      } else {
        # recovery
        MOI = MOI -1
        time = time + nextClearance
      }

    } else {
      event = 1
      MOI = MOI + 1
      time = time + nextBite
    }

    print(paste0("event: ",event," current MOI: ",MOI," happened at time: ",time))
  }
  return(MOI)
}


# # queueMG1.dy: dynamical system representation of stochastic process (infinite population size)
# queueMG1.dy = function(t, y, par, foi, MX){
#   dy = 0 * y # vector of derivatives
#   down = par[["r"]] * y[-1] * (1:MX) # recovery process
#   up = foi(t,par) * y[-MX] # infection (queueing) process
#
#   # recovery from m+1 to m
#   dy[-1] = dy[-1] - down
#   dy[-MX] = dy[-MX] + down
#
#   # infection from m to m+1
#   dy[-MX] = dy[-MX] - up
#   dy[-1] = dy[-1] + up
#
#   # return vector of derivatives
#   return(list(dy))
# }
#
# # queueMG1.eq: based on initial estimates of equilbrium from queueMG1.hr, run ODE to equilibrium
# queueMG1.eq = function(par,foi,T){
#   yi = queueMG1.hr(par,foi,T) # Poisson distributed fraction of population in MOI categories
#   # MX should be length(yi) - 1 because do not count uninfected m=0 category
#   y.eq = steady(yi, time=0, func=queueMG1.dy, parms=par, foi=foi.m, MX=length(yi)-1) # equilibrium solution
#   y.eq$y
# }
#
# # queueMG1.hr: give initial population fractions of distribution of MOI
# queueMG1.hr = function(par,foi,T){
#   mfoi = mean(foi(T,par)) # calculate mean force of infection
#   mMOI = mfoi/par[1] # calculate mean MOI
#   MXMOI = 10*mMOI # maximum MOI for finite strain model
#   dpois(c(0:MXMOI),mMOI) # PDF of poisson distribution of MOI for initial state
# }
#
# # queueMG1.sim: run queueing simulator
# queueMG1.sim = function(par,foi,T=c(0:1825)){
#   yi = queueMG1.hr(par,foi,T)
#   lsode(yi,T,queueMG1.dy,par,foi=foi,MX=length(yi)-1)
# }
#
# # foi.m: constant FOI
# foi.m = function(t,p){p[2]}
#
# # foi.sin: sinusoidally forced FOI (b controls strength of seasonal forcing)
# foi.sin = function(t,p){
#   p[["a"]] * (1 + p[["b"]] * sin(2*pi*t/365))
# }
#
# # parameters
# T=c(0:(365*10))
# par = c(r=1/200, a=1/200, b=0.9)
# out.hr = queueMG1.hr(par,foi.sin,T)
# out.eq = queueMG1.eq(par,foi.sin,T)
# out.sim = queueMG1.sim(par,foi.sin,T)
#
# y.t = out.sim[,-1] # total population model
# p.t = y.t[,-1] # only model those with MOI > 0
# MOI = 1:ncol(p.t) # discrete MOI compartments
#
# dMOI.t = MOI*t(p.t) # renormalize by multiplying fraction of pop in compartment m by the value m
# mMOI.t = colSums(dMOI.t) # mean MOI of population at each time step
# # PR = 1-p.t[,1]
# PR = 1 - y.t[,1]
#
# par(mfrow = c(3,2))
# # FOI vs. time
# plot(T/365, foi.sin(T,par), type = "l", main = "FOI vs. Time")
# # Poisson distributed MOI
# plot(c(0,MOI), out.hr, type = "h", lwd=3, xlim = c(0,(length(MOI)+5)),main = "Population MOI Distribution",xlab="MOI",ylab="Density")
# lines(c(0,MOI), out.eq, col = "red", type = "h")
# # PfPR vs. time
# plot(T/365, PR, type = "l", main = "Prevalence")
# # MOI vs. time
# plot(T/365, mMOI.t, type = "l", main = "MOI", xlab = "Time")
# # PfPR vs. FOI
# # ixo=c(1:730) # transient burn-in
# ixo=1
# plot(foi.sin(T[-ixo],par), PR[-ixo],  type = "l", xlab = "FOI", ylab = "PR")
# points(x = foi.sin(T[ixo[1]],par), y = PR[ixo[1]],pch=16,col="red")
# # mean MOI vs. PfPR
# plot(mMOI.t[-ixo], PR[-ixo],  type = "l", xlab = "Mean MOI", ylab = "PR")
# par(mfrow=c(1,1))
