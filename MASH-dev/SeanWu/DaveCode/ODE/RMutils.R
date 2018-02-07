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
{list(f=f, Q=Q, g=g, n=n, b=b,c=c, P=exp(-g*n), S=f*Q/g,
r=r,N=N)}

Rc2L = function(Rc, H, alpha, par){with(par,{
  Rc*r*H/b/c/S^2/P/(1-alpha)
})}

#############################################################
#  Closed Population Solutions
#############################################################

Rc2x = function(Rc,par){with(par,{
  pmax((Rc-1)/(Rc+c*S),0)
})}

x2Rc = function(x,par){with(par,{
  (1+c*S*x)/(1-x)
})}

x2Ro = function(x, alpha, par){
  x2Rc(x,par)/(1-alpha) 
}

Rc2h = function(Rc,par){with(par,{
  r*(Rc-1)/(1+c*S)  
})}

Ro2h = function(Ro,alpha,par){
  Rc2h(Ro*(1-alpha), par)    
}

h2Rc = function(h,par){with(par,{
  1 + h*(1+c*S)/r  
})}

ci2Rc = function(ci,par){with(par,{
  1  + ci/alpha*(1+c*S)/r  
})}


h2Ro = function(h, alpha, par){
 h2Rc(h,par)/(1-alpha)  
} 

h2x = function(h,par){with(par,{
  h/(h+r)  
})}

x2h = function(x,par){with(par,{
  r*x/(1-x)
})}

x2ci = function(x,alpha,par){
  alpha*x2h(x,par) 
}

ci2x = function(ci,alpha,par){
  ci/(ci+alpha*r)  
}

ci2Ro = function(ci, alpha, par){
 h2Rc(ci/alpha,par)/(1-alpha)  
} 
