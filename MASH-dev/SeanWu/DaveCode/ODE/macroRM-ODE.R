require(deSolve)
require(rootSolve)

#############################################################
#  RM Variable Names  
#  X     : Population density of infected humans
#  M     : Mosquito population density
#  Y     : Population density of infected mosquitoes
#  L     : emergence rate of adult mosquitoes m = L/g/H
#  H     : human population density 
#  alpha : proportion of incident malaria cases treated & cured
#############################################################

#############################################################
#  Mosquito bionomic parameters (f,Q,n,g) follow: 
#  Smith DL, McKenzie FE. Statics & dynamics of malaria
#  infection in Anopheles mosquitoes. (2004) Malaria J 3:13. 
#############################################################
RMparams = function(
  f=1/2.5,  # The average interval between bloodmeals is 1/f
  Q=0.9,    # The proportion of bites taken on a human 
  n=12,     # The Extrinsic Incubation Period (EIP) in days 
  b=0.55,   # Average daily infectiousness of mosquitoes 
  c=0.15,   # Average daily infectiousness of humans 
  g=1/10,   # Mosquito lifespan is 1/g
  r=1/200,  # Average duration of a human infection is 1/r
  q=.01,    # Daily diffusion rate
  N=1
)
{list(f=f, Q=Q, g=g, n=n, b=b,c=c, P=exp(-g*n), S=f*Q/g, r=r,N=N)} 

Rc2L = function(Rc, H, alpha, par){with(par,{
  Rc*r*H/b/c/S^2/P/(1-alpha)  
})}


#############################################################
#  Closed Population Solutions
#############################################################
closedRMEq.Rc2X = function(Rc,par){with(par,{
  pmax((Rc-1)/(Rc+c*S),0)
})} 

closedRMEq.x2Rc = function(x,par){with(par,{
  #Rcx/(1+cSx)(1-x)=x
  #Rc(1-x)=(1+cSx)
  (1+c*S*x)/(1-x)
})} 

closedL2RMEq = function(L,H,alpha,par){with(par,{
  # Mosquito Population Density 
  M = L/g  
  lambda = L/H
  m = M/H
  # Reproductive Numbers  
  Ro = lambda*S^2*P/r
  Rc = (1-alpha)*Ro
  ix = which(H==0)
  Rc[ix]=0
  # Prevalence of infection 
  x = pmax((Rc-1)/(Rc+c*S),0)
  y = pmax((Rc-1)/Rc*P*c*S/(1+c*S),0)
  y[ix]=0
  z = P*y
  # EIR 
  dEIR=m*f*Q*z
  aEIR = 365*dEIR
  list(M=M, m=m, 
       Ro=Ro, Rc=Rc, 
       PR=x, X=x*H, 
       y=y, Y=y*M, 
       z=z, Z=z*M, 
       dEIR=dEIR, dFOI=b*dEIR, 
       aEIR=aEIR, aFOI=b*aEIR)
})} 

closedRMEq.Inits = function(L,H,alpha,par,nudge=0.01){
  Eq = closedRMEq(L,H,alpha,par)
  N = length(H)
  Xo = (nudge+Eq$X)#*(1+rnorm(N,nudge,nudge/4))
  Yo = (nudge+Eq$Y)#*(1+rnorm(N,nudge,nudge/4))
  Mo = (nudge+Eq$M)#*(1+rnorm(N,nudge,nudge/4))
  c(Xo,Mo,Yo)
} 


#############################################################
# RMpsi  
#############################################################

RMASn.derivs = function(t, y, par, L, H, Hi, AS, tAS, ASn, Dfsum, Df, Dfn, alpha){
  with(as.list(par),{
    X = y[1:N]
    M = y[1:N + N]
    Y = y[1:N + 2*N]

    EIR = f*Q*P*AS*Y[ASn]/Hi
    
    dX = b*(1-alpha)*EIR*(H-X) - r*X
    dM = L-g*M - q*Dfsum*M + q*Df*M[Dfn] 

    dY = f*Q*c*X[tAS]/Hi*(M-Y)- g*Y - q*dAsum*Y + q*dA*Y[Mnbrs] 
    
    list(c(dX,dM,dY))
})}

RMASn.derivs = function(t, y, par, L, H, Hi, AS, tAS, ASn, Dfsum, Df, Dfn, alpha){
  with(as.list(par),{
    X = y[1:N]
    M = y[1:N + N]
    Y = y[1:N + 2*N]

    EIR = f*Q*P*AS*Y[ASn]/Hi
    
    dX = b*(1-alpha)*EIR*(H-X) - r*X
    dM = L-g*M - q*Dfsum*M + q*rowSums(Df*M[Dfn]) 
    dY = f*Q*c*X[tAS]/Hi*(M-Y)- g*Y- q*Dfsum*Y + q*rowSums(Df*Y[Dfn]) 
    
    list(c(dX,dM,dY))
})}


RMASn.sim= function(par, L, H, Hi, AS, ASn, Dfsum, Df, Dfn, alpha){
  inits = closedRMEq.Inits(L,H,alpha,par,nudge=0.01)
  out =steady(inits, 0, RMpsi0.derivs, par,  L=L, H=H, Hi=Hi, 
	AS=AS, ASn=ASn, alpha=alpha, Df=Df, Dfn=Dfn)$y
})}

RMM.derivs = function (t, y, par, L, Dfsum, Df, Dfn, alpha){
  with(as.list(par),{
    dM = L-g*M - q*Dfsum*M + q*rowSums(Df*M[Dfn]) 
    list(dM) 
})}
 
 
#############################################################
#  RMpsi and RMpsi0 
#  X     : Population density of infected humans
#  M     : Mosquito population density
#  Y     : Population density of infected mosquitoes
#  psi   : human time at risk kernel
#  sigma : mosquito connectivity kernel (ignored in RMpsi0)
#############################################################

RMpsi.derivs = function(t, y, par, L, H, psi, alpha, sigma){
  with(as.list(par),{
    X = y[1:N]
    M = y[1:N + N]
    Y = y[1:N + 2*N]
    Hi = t(psi)%*%H 
    EIR = f*Q*P*psi%*%Y/Hi
    
    dX = b*(1-alpha)*EIR*(H-X) - r*X
    dM = L-g*M - q*(sigma%*%M - t(sigma)%*%M)
    dY = f*Q*c*t(psi)%*%X/Hi*(M-Y)- g*Y = q*(sigma%*%Y - t(sigma)%*%Y)
    
    list(c(dX,dM,dY))
})}

RMpsi0.derivs = function(t, y, par, L, H, psi, alpha){
  with(as.list(par),{
    X = y[1:N]
    M = y[1:N + N]  
    Y = y[1:N + 2*N]
    Hi = t(psi)%*%H 
    EIR = f*Q*P*psi%*%Y/Hi
    
    dX = b*(1-alpha)*EIR*(H-X) - r*X
    dM = L-g*M
    dY = f*Q*c*t(psi)%*%X/Hi*(M-Y) - g*Y
    
    list(c(dX,dM,dY))
})}

RMpsi0.steady = function(L, H, psi, alpha, par){
  inits = closedRMEq.Inits(L,H,alpha,par,nudge=0.01)
  out =steady(inits, 0, RMpsi0.derivs, par, H=H, L=L, psi=psi, alpha=alpha)$y
  N=length(H)
  X=out[1:N]
  M=out[1:N+N]
  Y=out[1:N+2*N]
  list(X=X, x=X/H, Y=Y, y=Y/H,M=M,m=M/H) 
} 

RMpsiHum.derivs = function(t,y,par,Rc,H,psi,alpha){with(par,{
  Hi = t(psi)%*%H
  Xi = t(psi)%*%(y*H)
  x1 = Xi/Hi
  list(psi%*%(Rc*H*x1/(1+c*S*x1))/Hi*(1-y)-y) 
})}

RMpsiHum.eq = function(Rc, H, psi, alpha, par){
  inits = pmax((Rc-1)/(Rc+c*S),0) 
  runsteady(y=inits, times = c(0,1000), func=RMpsiHum.derivs, parms=par, Rc=Rc, L=L, psi=psi, alpha=alpha)$y
} 


RMpsi0.dX = function(x,Rc,H,psi,alpha,par){with(par,{
  Hi = t(psi)%*%H
  Xi = t(psi)%*%(x*H)
  x1 = Xi/Hi
  psi%*%(Rc*H*x1/(1+c*S*x1))/Hi*(1-x)-x
})}

RMpsi0.Rc2x = function(Rc,H,psi,alpha,par){with(par,{
  xi=pmax((Rc-1)/(Rc+c*S),0) 
  x=nleqslv(xi, RMpsi0.dX,Rc=Rc,H=H,psi=psi,alpha=alpha,par=par)
})}

RMpsi0.X2Rc = function(x,H,psi,alpha,par){with(par,{
  Hi = t(psi)%*%H
  Xi = t(psi)%*%(x*H)
  x1 = Xi/Hi
  Y = as.vector(H*x1/(1+c*S*x1))
  solve(psi%*%diag(Y, N)*(1-x), Hi*x)
})}

macroRModeHuman.derivs = function(t, X, par, H, L, psi, alpha){
  #############################################################
  #  Mosquito bionomic parameters (f,Q,n,g) follow: 
  #  Smith DL, McKenzie FE. Statics & dynamics of malaria
  #  infection in Anopheles mosquitoes. (2004) Malaria J 3:13. 
  #
  #  N     : the number of patches
  #  L     : emergence rate of adult mosquitoes m = L/g/H
  #  H     : human population density 
  #  sigma : mosquito diffusion 
  #  alpha : proportion of incident malaria cases treated & cured
  #  psi   : human time at risk kernel
  #
  #  X     : Population density of infected humans
  #  M     : Mosquito population density
  #  Y     : Population density of infected mosquitoes
  #############################################################
  
  with(as.list(par),{
    EIR = as.vector(L*c*S^2*P*X/(1 + c*S*X)%*%psi/H%*%psi) 
    dX = b*(1-alpha)*EIR*(H-X) - r*X 
    list(dX)
})}

macroRModeHuman.eq = function(L, H, psi, alpha, par){
  inits = closedRMEq(L,H,alpha,par)[1:par$N]
  #steady(inits, 0, macroRModeHuman.derivs, par, H=H, L=L, psi=psi, alpha=alpha)$y
  runsteady(y=inits, times = c(0,1000), func=macroRModeHuman.derivs, parms=par, H=H, L=L, psi=psi, alpha=alpha)$y
} 

macroRMode.eq = function(L, H, psi, sigma, alpha, par){
  inits = closedRMEq(L,H,alpha,par)
  print(c("length of inits = ", length(inits))) 
  rs.sigma = rowSums(sigma) 
  print(c("length of rs.sigma = ", length(rs.sigma)))
  dXdt = macroRMode.derivs(0, inits, par,  H, L, psi, sigma, rs.sigma, alpha)
  print(c("length of dXdt[[1]] = ", length(dXdt[[1]])))
  steady(inits, 0, macroRMode.derivs, par, H=H, L=L, psi=psi, sigma=sigma, rs.s = rs.sigma, alpha=alpha)$y
} 

RMEqSummary = function(y, H, alpha, Omega, par){with(as.list(par),{
  #  alpha : proportion of incident malaria cases treated & cured
  #  H     : human population density 
  #  Omega : Health Seeking 
  
  X = y[1:N]
  x = X/H
  dEIR = f*Q*P*y[2*N+1:N]/H
  aEIR = 365*dEIR
  aFOI = b*aEIR
  aCases = alpha*aFOI
  aClinicalCases = aCases %*% Omega 
  list(X=X, PfPR=x, aEIR=aEIR, aFOI=aFOI, aCases=aCases, aCasesAtClinics = aClinicalCases) 
})}

#sumit = function(out, par){with(as.list(par),{
#  l = dim(out)[1]
#  X = out[l,1+1:N]
#  M = out[l,1+N+1:N]
#  Y = out[l,1+2*N+1:N]
#  list(x=X/Hi,m=M/Hi, z=M*P/Hi)
#})}

