
source ("macroRM-ODE.R")

par = c(RMparams(Q=.1), N=6) 
H = c(10,20,40,80,60,5)
alpha = rep(0.3, 6)

Rc = c(2,1.3,0.5,1,0.2,8)
getm = function(Rc,alpha,par){
  with(par,Rc*g*r/b/c/f^2/Q^2/P/(1-alpha)) 
}

L = getm(Rc,alpha,par)*H*par$g 

getRc = function(m, alpha, par){
  with(par,(1-alpha)*getm(Rc,alpha,par)*f^2*Q^2*P*b*c/g/r)
} 

psi = matrix(
  c(.85, .01, .06, .01, .01, .06,
    .06, .85, .06, .01, .01, .01,
    .01, .06, .85, .06, .01, .01, 
    .01, .01, .06, .85, .06, .01,
    .01, .01, .01, .06, .85, .06, 
    .06, .01, .01, .01, .06, .85), 6,6)

Xo = pmax(1-1/Rc, 0.01)
id6 = diag(1,6)

Xeq = RMMacro0.steady(L,H,psi,alpha,par)

Rci = RMMacro0.X2Rc(Xeq2,H,psi,alpha,par)

dX = function(X, Rc, psi){Rc*psi%*%X*(1-X)-X}

RMMacro0.dX2 = function(x,Rc,H,psi,alpha,par){
  sum(RMMacro0.dX(x,Rc,H,psi,alpha,par)^2)
}

Xeq2 = nlm(RMMacro0.dX2, Xeq$x, Rc=Rc, H=H,psi=psi,alpha=alpha,par=par)$estimate

X1 = Xo + h*dX(Xo,Rc,psi)*Xo

RMMacroSimple.derivs = function(t, Y, par, Rc, H, psi, alpha){
  with(as.list(par),{
    x = Y[1:N]
    y = Y[1:N + N]
    
    dXdt = 1/H*psi%*%H*psi%*%(Rc*r/c/S*y)*(1-x) - r*x
    dYdt = f*Q*c*t(psi)%*%x*(1-y)- g*y
    
    list(c(dXdt,dYdt))
})}

RMMacroSimple.steady = function(m, H, psi, alpha, par){
  Rc = getRc(m,alpha,par)
  xo = with(par,pmax(.01, (Rc-1)/Rc+c*S)) 
  yo = with(par,pmax(.01, (Rc-1)*c*S/Rc/(1+c*S)))
  inits = c(xo,yo)
  out=runsteady(y=inits, times = c(0,1000), func=RMMacroSimple.derivs, parms=par, H=H, Rc=Rc, psi=psi, alpha=alpha)$y
  x=out[1:par$N]
  y=out[1:par$N+par$N]
  list(x=x,y=y)
} 

Xeq.d = RMMacroSimple.steady(m,H,psi,alpha,par)
Xeq.m = RMMacro.linear(m,H,alpha,par)
