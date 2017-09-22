
xbar = function(P, P50=6, Ps=1, fac=1.05){
 pmin(fac*(1/(1+exp(-Ps*(P-P50))) - 1/(1+exp(Ps*P50))), 1) 
}

waxingF = function(x, wax, xb, xw=.5, kw=5){
  a = (1-1/(1+exp(kw*xw*xb)))-(1-1/(1+exp(-kw*(xb*(1-xw)))))
  b = (1-1/(1+exp(-kw*(x-xw*xb))))-(1-1/(1+exp(-kw*(xb*(1-xw)))))
  wax*b/a
}

waningF = function(x,wane,xb,xn=0.5,kn=5){
  a = 1/(1+exp(-kn*(1-xn*(1+xb)))) - 1/(1+exp(-kn*(xb-xn*(1+xb))))
  b = 1/(1+exp(-kn*(x-xn*(1+xb)))) - 1/(1+exp(-kn*(xb-xn*(1+xb))))
  -wane*b/a
}

waxwaneRates = function(x, P, P50 = 6, Ps = 1, wax=1/180, xw=0.7, kw=30, wane=1/30, xn=0.5, kn=30){
  xb = xbar(P, P50, Ps)
  R=x*0
  ix = which(x<=xb)
  if(length(ix)>0)
    R[ix] = waxingF(x[ix], wax[ix], xb, xw, kw)
  ix = which(x>xb)
  if(length(ix)>0)
    R[ix] = waningF(x[ix], wane[ix], xb, xn, kn)
  R
}

iterWaxWane = function(x, P, PAR){with(PAR,{
  x + waxwaneRates(x,P,P50,Ps,wax,xw,kw,wane,xn,kn)
})}

PAR.def = list(P50=6, Ps=1, wax=1/180, xw=0.5, kw=10, wane=1/30, xn=0.5, kn=10) 