###############################################################################
#       __  ______   _____ __  __      __  ______   __________  ____
#      /  |/  /   | / ___// / / /     /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| | \__ \/ /_/ /_____/ /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ |___/ / __  /_____/ /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_/____/_/ /_/     /_/  /_/_/  |_\____/_/ |_|\____/
#
#   MASH-MACRO
#   Testing Ground
#   August 22, 2017
#
###############################################################################

rm(list=ls());gc()
library(MASHmacro)

testELP <- R6::R6Class(classname = "testELP",
                       public = list(
                         alpha=NULL,
                         gamma=NULL,
                         psi=NULL,
                         sigma=NULL,
                         P=NULL,
                         L=NULL,
                         E=NULL,
                         initialize = function(alpha=0.1,gamma=0.1,psi=0.01,sigma=1){
                           self$alpha = alpha
                           self$gamma = gamma
                           self$psi = psi
                           self$sigma = sigma
                           self$P = 0
                           self$L = 0
                           self$E = 0
                         },
                         addE = function(E){
                           self$E = E
                         },
                         runStep = function(){
                           browser()
                           L0 = self$L;
                           self$P = self$alpha*L0;
                           self$L = self$E - (self$alpha + self$gamma + (self$psi * (L0^self$sigma)))*L0;
                           E = 0.0;
                           lambda = self$P
                           print(paste0("lambda: ",lambda))
                           self$P = 0
                           return(lambda)
                         },
                         oneStep = function(){
                           dL = self$E - (self$alpha + self$gamma + (self$psi * (self$L^self$sigma)))*self$L
                           lambda = self$alpha*self$L
                           self$E = 0
                           self$L = self$L + dL
                           print(paste0("L: ",self$L))
                           print(paste0("lambda: ",lambda))
                           return(lambda)
                         },
                         finalize = function(){print("i'm being garbage cleaned")}
                       ))

v = 25
f = 0.3
m = 20

# ELP = testELP$new()
# tMax = 250
# lambdaVec = numeric(tMax)
# for(i in 1:tMax){
#   ELP$addE(E = v*f*m)
#   lambdaVec[i] = ELP$runStep()
# }
#
# plot(lambdaVec,type="l")

MASHmacro::K2psi_ELP(f = f,v = v,alpha = 0.1,g = 1/10,gamma = 0.1,K = 20)


# test diff eqn

library(deSolve)

ELPode <- function(t,x,parms){
 with(as.list(c(parms,x)),{
  dL = f*v*M - (alpha + gamma + psi*L)*L
  dM = alpha*L - g*M
  return(list(c(dL,dM),eggIn=(f*v*M),larMort=(alpha+gamma+psi*L)*L,larPup=(alpha*L),adultMort=(g*M)))
 })
}

ELPout = deSolve::ode(y = c(L=20,M=20),times = 1:250,func = ELPode,
                      parms = c(f=0.3,v=20,alpha=0.1,gamma=0.1,psi=0.01,g=1/10))

par(mfrow=c(1,2))
maxY = max(max(ELPout[,"L"]),max(ELPout[,"M"]))
plot(ELPout[,"L"],type="l",col="purple",ylim=c(0,maxY),main="ODE solution (1 L compartment)")
lines(ELPout[,"M"],col="red")
grid()

ELPodeN <- function(time,state,par){
  with(as.list(c(par,state)),{

    Ltot = sum(L1,L2,L3,L4)
    dL1 = f*v*M - ((alpha*n) + gamma + psi*Ltot)*L1
    dL2 = (alpha*n)*L1 - ((alpha*n) + gamma + psi*Ltot)*L2
    dL3 = (alpha*n)*L2 - ((alpha*n) + gamma + psi*Ltot)*L3
    dL4 = (alpha*n)*L3 - ((alpha*n) + gamma + psi*Ltot)*L4
    dM = (alpha*n)*L4 - g*M
    return(
      list(c(dL1,dL2,dL3,dL4,dM),Ltot=Ltot)
    )
  })
}

ELPoutN = deSolve::ode(y = c(L1=20,L2=0,L3=0,L4=0,M=20),times = 1:250,func = ELPodeN,
                       parms = c(f=0.3,v=20,alpha=0.1,gamma=0.1,psi=0.01,g=1/10,n=4))

plot(ELPoutN[,"Ltot"],col="darkgreen",type="l",main="purple is L1-L4,green sum(L), red mosy",ylim=c(0,max(ELPoutN[,-1])))
matplot(x = ELPoutN[,c("L1","L2","L3","L4")],type="l",col="purple",add=T)
lines(ELPoutN[,"M"],col="red")
lines(ELPoutN[,"Ltot"],col="darkgreen")
grid()
par(mfrow=c(1,1))

###############################################################################
#
# COMPARE DIFFERENCE EQNS TO DIFFERENTIAL EQNS
#
###############################################################################

# test diff eqn
rm(list=ls());gc()

tGrain = 12

v = 20
f = (0.3)/tGrain
g = (1/10)/tGrain
alpha=(0.1)/tGrain
gamma=(0.1)/tGrain

psiFit = MASHmacro::K2psi_ELP(f = f,v = v,alpha = alpha,g = g,gamma = gamma,K = sqrt(16))

# psi=.Machine$double.eps
# psi = 0.001
psi = psiFit

Lstart = 20
Mstart = 20
tMax=(10*tGrain)

Mhist = numeric(tMax+1)
Lhist = numeric(tMax+1)
Mhist[1] = Mstart
Lhist[1] = Lstart
for(i in seq(from=2,to=tMax+1,by=1/tGrain)){
  M = Mhist[i-1]
  L = Lhist[i-1]
  # print(paste0("at i: ",i," eggs laid: ",(f*v*M)))
  # print(paste0("at i larvae dying: ",((alpha + gamma + psi*L)*L)))
  # print(paste0("at i adults emerging: ",(alpha*L)))
  # print(paste0("at i adults dying: ",(g*M)))
  dL = f*v*M - (alpha + gamma + psi*L)*L
  dM = alpha*L - g*M
  # L <<- L + dL
  # M <<- M + dM
  Mhist[i+1] = M +dM
  Lhist[i+1] = L +dL
}

par(mfrow=c(1,2))
maxY = max(max(Mhist),max(Lhist))
minY = min(min(Mhist),min(Lhist))
plot(Mhist,col="red",type="l",ylim=c(minY,maxY),main="larvae is purple, adults are red",xlab=paste0("tGrain: ",tGrain))
lines(Lhist,col="purple")
grid()


ELPode <- function(t,x,parms){
  with(as.list(c(parms,x)),{
    dL = f*v*M - (alpha + gamma + psi*L)*L
    dM = alpha*L - g*M
    return(list(c(dL,dM),eggIn=(f*v*M),larMort=(alpha+gamma+psi*L)*L,larPup=(alpha*L),adultMort=(g*M)))
  })
}


ELPout = deSolve::ode(y = c(L=Lstart,M=Mstart),times = seq(from=1,to=tMax+1,by=1/tGrain),func = ELPode,
                      parms = c(f=f,v=v,alpha=alpha,gamma=gamma,psi=psi,g=g))

maxY = max(max(ELPout[,"L"]),max(ELPout[,"M"]))
plot(ELPout[,"L"],type="l",col="purple",ylim=c(0,maxY),main="ODE solution")
lines(ELPout[,"M"],col="red")
grid()
par(mfrow=c(1,1))
