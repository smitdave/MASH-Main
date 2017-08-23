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

maxY = max(max(ELPout[,"L"]),max(ELPout[,"M"]))
plot(ELPout[,"L"],type="l",col="purple",ylim=c(0,maxY),main="ODE solution (1 L compartment)")
lines(ELPout[,"M"],col="red")
grid()

ELPodeN <- function(time,state,par){
  with(as.list(par),{


  })
}


modBasic <- function(time,state,par){
  with(as.list(par),{

    sumY  = sum(state[2:(n+1)])

    dxVec = vector(mode="numeric",length=n+2)
    names(dxVec) = paste0("d",names(state))

    dxVec["dX"] = (a*b*MtoH*state[["Z"]]*(1-state[["X"]])) - (r*state[["X"]])
    dxVec["dY1"] = (a*c*state[["X"]]*(1-sumY-state[["Z"]])) - (((q*n)+g)*state[["Y1"]])
    for(i in 2:n){
      dxVec[[paste0("dY",i)]] = (q*n*state[[paste0("Y",(i-1))]]) - (((q*n)+g)*state[[paste0("Y",i)]])
    }
    dxVec["dZ"] = (q*n*state[[paste0("Y",n)]]) - (g*state[["Z"]])

    return(list(dxVec))
  })
}

# DS EXAMPLE OF GAMMA DISTRIBUTED DWELL TIMES
RossRealInc = function(t,y,par)
{
  g = par[1]
  a = par[2]
  b = par[3]
  c = par[4]
  r = par[5]
  n = par[6]
  m = par[7]
  N = par[8]
  X = y[1]
  idx = c(1:N)+1
  Y = y[idx]
  Z = y[N+2]
  q = 1/n

  dY = Y
  dX = m*a*b*Z*(1-X)-r*X
  dY[1] = a*c*X*(1-sum(Y)-Z)-(q*N+g)*Y[1]
  for(i in c(2:N))
    dY[i] = q*N*Y[i-1] - (q*N+g)*Y[i]
  dZ = q*N*Y[N] - g*Z

  list(c(dX,dY,dZ))
}

##Parameters
g = 1/10   #Mosquito death rate
a = 0.3    #Human feeding rate
b = 0.5    #Transmission efficiency
c = 0.5    #Transmission efficiency
r = 1/100  #Human infectious period
n = 10     #Incubation period
m = 5     #Mosquito density, per human
# N = 8     #Mosquito density, per human (SEEMS TO ACTUALLY BE NUMBER OF DISCRETIZED EIP STATES)

N=16

params = c(g,a,b,c,r,n,m,N)
inits = c(0.01, rep(0,N+1))
t = 1:35
lsoda(y = inits, times = t, func = RossRealInc, parms = params)

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
