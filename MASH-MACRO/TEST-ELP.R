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
  return(list(c(dL,dM)))
 })
}

ELPout = deSolve::ode(y = c(L=100,M=100),times = 1:250,func = ELPode,
                      parms = c(f=0.3,v=20,alpha=0.1,gamma=0.1,psi=0.01,g=1/10))

maxY = max(max(ELPout[,"L"]),max(ELPout[,"M"]))
plot(ELPout[,"L"],type="l",col="red",ylim=c(0,maxY),main="ODE solution")
lines(ELPout[,"M"],col="purple")
grid()

# test diff eqn
rm(list=ls());gc()

v = 20
f = 0.3
g = 1/10
alpha=0.1
gamma=0.1
psi=0.01

L = 100
M = 100

tMax=250

Mhist = numeric(tMax+1)
Lhist = numeric(tMax+1)
Mhist[1] = M
Lhist[1] = L
for(i in 1:tMax){
  dL = f*v*M - (alpha + gamma + psi*L)*L
  dM = alpha*L - g*M
  L <<- L + dL
  M <<- M + dM
  Mhist[i+1] = M
  Lhist[i+1] = L
}
maxY = max(max(Mhist),max(Lhist))
plot(Mhist,col="red",type="l",ylim=c(0,maxY))
