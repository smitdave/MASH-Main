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


###############################################################################
# John Marshall EL4P
###############################################################################

N_genotypes = 2

# EL4P = matrix(data = 0,nrow = 7,ncol = N_genotypes,dimnames = list(c("E","L1","L2","L3","L4","P","lambda"),NULL))
E = rep(0,N_genotypes)
L1 = rep(0,N_genotypes)
L2 = rep(0,N_genotypes)
L3 = rep(0,N_genotypes)
L4 = rep(0,N_genotypes)
P = rep(0,N_genotypes)
lambda = rep(0,N_genotypes)


K = 1e3
muE = 0.1
muL = 0.1
muP = 0.1
durE = 1
durL = 4
durP = 1

L = sum(L1,L2,L3,L4)

coef = matrix(data = c(
  rep(x =  exp(-muE)*(1-(1-exp(-1/durE))),times = N_genotypes), # E
  rep(x = exp(-muE)*(1-exp(-1/durE)) + exp(-muL*(1+(L/K)))*(1-(1-exp(-4/durL))),times = N_genotypes), # L1
  rep(x = exp(-muL*(1+(L/K)))*((1-exp(-4/durL)) + (1-(1-exp(-4/durL)))),times = N_genotypes), # L2
  rep(x = exp(-muL*(1+(L/K)))*((1-exp(-4/durL)) + (1-(1-exp(-4/durL)))),times = N_genotypes), # L3
  rep(x = exp(-muL*(1+(L/K)))*((1-exp(-4/durL)) + (1-(1-exp(-4/durL)))),times = N_genotypes), # L4
  rep(x = exp(-muL*(1+(L/K)))*(1-exp(-4/durL)) + exp(-muP)*(1-(1-exp(-1/durP))),times = N_genotypes), # P
  rep(x = exp(-muP)*(1-exp(-1/(2*durP))),times = N_genotypes) # lambda
),nrow = 7,ncol = N_genotypes,byrow = TRUE,dimnames = list(c("E","L1","L2","L3","L4","P","lambda"),NULL))


EL4P["E",1:2] = 10

# new = 

pop$eggs   = exp(-muE)*(1-(1-exp(-1/durE)))*eggso
pop$L1     = exp(-muE)*(1-exp(-1/durE))*eggso + exp(-muL*(1+(Lo/K)))*(1-(1-exp(-4/durL)))*L1o
pop$L2     = exp(-muL*(1+(Lo/K)))*((1-exp(-4/durL))*L1o + (1-(1-exp(-4/durL)))*L2o)
pop$L3     = exp(-muL*(1+(Lo/K)))*((1-exp(-4/durL))*L2o + (1-(1-exp(-4/durL)))*L3o)
pop$L4     = exp(-muL*(1+(Lo/K)))*((1-exp(-4/durL))*L3o + (1-(1-exp(-4/durL)))*L4o)
pop$P      = exp(-muL*(1+(Lo/K)))*(1-exp(-4/durL))*L4o + exp(-muP)*(1-(1-exp(-1/durP)))*Po
pop$lambda = exp(-muP)*(1-exp(-1/(2*durP)))*Po


###############################################################################
#
# EL4P R VS C++ HEAD TO HEAD
# ITS TIME TO D-D-D-D-D-D-DUEL
#
###############################################################################


tGrain = 24
v = 20
f = (0.3)/tGrain
g = (1/10)/tGrain
alpha=(0.9)/tGrain
gamma=(0.1)/tGrain
psi = 0.001/tGrain
Lstart = 20
Mstart = 20
tMax=100
n=1

L1hist = numeric(tMax)
L2hist = numeric(tMax)
L3hist = numeric(tMax)
L4hist = numeric(tMax)
Mhist = numeric(tMax)
LtotHist = numeric(tMax)

Mhist[1] = Mstart
L1hist[1] = Lstart/4
L2hist[1] = Lstart/4
L3hist[1] = Lstart/4
L4hist[1] = Lstart/4
LtotHist[1] = Lstart

tVec = seq(from=2,to=tMax+1,by=1/tGrain)
for(i in 2:length(tVec)){
  L1 = L1hist[i-1]
  L2 = L2hist[i-1]
  L3 = L3hist[i-1]
  L4 = L4hist[i-1]
  M = Mhist[i-1]

  Ltot = sum(L1,L2,L3,L4)

  dL1 = f*v*M - (alpha*n)*L1 - gamma*L1 - (psi*Ltot)*L1
  dL2 = (alpha*n)*L1 - (alpha*n)*L2 - gamma*L2 - (psi*Ltot)*L2
  dL3 = (alpha*n)*L2 - (alpha*n)*L3 - gamma*L3 - (psi*Ltot)*L3
  dL4 = (alpha*n)*L3 - (alpha*n)*L4 - gamma*L4 - (psi*Ltot)*L4
  dM = (alpha*n)*L4 - g*M

  L1hist[i] = L1 + dL1
  L2hist[i] = L2 + dL2
  L3hist[i] = L3 + dL3
  L4hist[i] = L4 + dL4
  Mhist[i] = M + dM
  LtotHist[i] = sum(L1hist[i],L2hist[i],L3hist[i],L4hist[i])
}

# par(mfrow=c(1,2))
maxY = max(max(L1hist),max(L2hist),max(L3hist),max(L4hist),max(Mhist),max(LtotHist))
plot(L1hist,type="l",col="purple",lty=1,ylim=c(0,maxY),ylab="Counts")
lines(L2hist,col="purple",lty=2)
lines(L3hist,col="purple",lty=3)
lines(L4hist,col="purple",lty=4)
lines(LtotHist,col="darkgreen")
lines(Mhist,col="red")
grid()


# rm(EL4P_cpp);gc()
# EL4P_cpp = MASHcpp::ELP(alpha_new = alpha,gamma_new = gamma,psi_new = psi,tGrain_new = tGrain)
# out = EL4P_cpp$ecologicalSimulation2(g_init = g,f_init = f,v_init = v,L_init = Lstart,M_init = Mstart,tMax = tMax)
# 
# lTotHist = Reduce(f = "+",x = out[1:4])
# maxY = max(vapply(X = c(out,lTotHist),FUN = max,FUN.VALUE = numeric(1)))
# plot(out$L1,type = "l",col="purple",lty=1,ylim=c(0,maxY),ylab = "Counts")
# lines(out$L2,col="purple",lty=2)
# lines(out$L3,col="purple",lty=3)
# lines(out$L4,col="purple",lty=4)
# lines(lTotHist,col="darkgreen")
# lines(out$M,col="red")
# grid()
# par(mfrow=c(1,1))

# ELP_roots <- function(x,par){
#   c(
#     F1 = par$f*par$v*x[2] - (par$alpha+par$gamma+par$psi*x[1])*x[1],
#     F2 = par$alpha*x[1] - par$g*x[2]
#   )
# }
# par = list(f=0.3,v=20,alpha=0.1,gamma=0.1,psi=0.01,g=1/10)
# rootSolve::multiroot(f = ELP_roots,start = c(500,500),parms = par,useFortran = TRUE,positive = TRUE)
#
#
# EL4P_roots <- function(x,par){
#   c(
#     F1 = par$f*par$v*x[5] - (4*par$alpha+par$gamma+par$psi*sum(x[1],x[2],x[3],x[4]))*x[1],
#     F2 = 4*par$alpha*x[1] - (4*par$alpha+par$gamma+par$psi*sum(x[1],x[2],x[3],x[4]))*x[2],
#     F3 = 4*par$alpha*x[2] - (4*par$alpha+par$gamma+par$psi*sum(x[1],x[2],x[3],x[4]))*x[3],
#     F4 = 4*par$alpha*x[3] - (4*par$alpha+par$gamma+par$psi*sum(x[1],x[2],x[3],x[4]))*x[4],
#     F5 = 4*par$alpha*x[4] - par$g*x[5]
#   )
# }
# par = list(f=0.3,v=20,alpha=0.1,gamma=0.1,psi=0.01,g=1/10)
# out = rootSolve::multiroot(f = EL4P_roots,start = c(100,100,100,100,100),parms = par,useFortran = TRUE,positive = TRUE)
# sum(out$root[1:4])
