
RM_ASn_Rc.derivs = function(t, y, par, Rc, H, Hi, AS, tAS, ASn, alpha){
  with(as.list(par),{
    X = y[1:N]
    x = rowSums(X[tAS]/Hi)
    Y = x/(1+c*S*x) 
    EIR = Rc*H*rowSums(Y[ASn]/Hi)
    dX = Rc*H*rowSums(Y[ASn]/Hi)*(H-X) - r*X
    list(c(dX,dM,dY))
})}

RM_ASn_Rc.sim= function(par, Rc, H, Hi, AS, ASn, tAS, alpha, tm=c(0:365)){
  inits = closedRMEq.Inits(L,H,alpha,par,nudge=0.01)
  out = lsode(inits, 0, RMpsi0.derivs, par,  L=L, H=H, Hi=Hi, 
	AS=AS, ASn=ASn, alpha=alpha, Df=Df, Dfn=Dfn)$y
})}


