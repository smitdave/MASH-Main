
##################################################################
# The notion here is that we have some underlying dynamic:
# dX/dt = r(K(P)-K(X))*(K(P)-X)
##################################################################

sigmoidX = function(X, X50=6, Xs=3, atMax=13){
  pmin((1/(1+exp(-Xs*(X-X50))) - 1/(1+exp(Xs*X50)))/(1/(1+exp(-Xs*(atMax-X50))) - 1/(1+exp(Xs*X50))),1)
}

dynamicXdt = function(X, P, PAR){with(PAR,{
  X = ifelse(is.na(X),0,X)
  P = ifelse(is.na(P),0,P)
  K = sigmoidX(P, P50, Ps, atMax)
  a = sign(K-X)
  b = exp(abs(b))
  X + (a*((1-exp(-b*abs(K-X)^sigma))*c(wn,0,wx)[a+2])^sigma)
})}

daysSinceUnder = function(X, P, PAR){with(PAR,{
  X = ifelse(is.na(X),0,X)
  P = ifelse(is.na(P),0,P)
  ifelse(P<Pthresh, X+1, 0)
})}

daysSinceOver = function(X, P, PAR){with(PAR,{
  X = ifelse(is.na(X),0,X)
  P = ifelse(is.na(P),0,P)
  ifelse(P>Pthresh, X+1, 0)
})}

antibodyRegister  = function(pfid, t, ixH){
  HUMANS[[ixH]]$Pf$types <<- rbind(HUMANS[[ixH]]$Pf$types, c(pfid, t))
}

dynamicCounter = function(P, PAR){
  X = 0
  Xt = X
  for(t in 1:length(P)){
    X = dynamicXdt(X, P[t], PAR)
    Xt = c(Xt, X)
  }
  Xt
}

gImPAR = function(wx=1/80, wn=1/180, P50=6, Ps=1, atMax=11, b=2, sigma=1){
  list(wx=wx,wn=wn,P50=P50,Ps=Ps,atMax=atMax,b=b,sigma=sigma)
}

#P=seq(0, 12, by = .1)
#X=seq(0,1, by = .01)
#
#plot(P, sigmoidX(P,6,3) , type = "l")
#
#plot(P, dX(.9, P, PAR.def), type = "l")
#segments(0,0,12,0, lty =2)
#
#
#
#plot(sigmoidX(tent0, 6,1), type = "l", ylim = c(0,1))
#Xt = dynamicCounter(tent0, gImPAR(wx=1/10, wn=1/10, P50=6))
#lines(Xt, type ="l")
#Xt = dynamicCounter(tent0, gImPAR(wx=1/30, wn=1/10, P50=6))
#lines(Xt, type ="l")
#Xt = dynamicCounter(tent0, gImPAR(wx=1/180, wn=1/10, P50=6))
#lines(Xt, type ="l")
#
#
#Xt = dynamicCounter(tent0, gImPAR(wx=1/10, wn=1/30, P50=6))
#lines(Xt, type ="l")
#Xt = dynamicCounter(tent0, gImPAR(wx=1/30, wn=1/30, P50=6))
#lines(Xt, type ="l")
#Xt = dynamicCounter(tent0, gImPAR(wx=1/180, wn=1/30, P50=6))
#lines(Xt, type ="l")
#
#Xt = dynamicCounter(tent0, gImPAR(wx=1/10, wn=1/180, P50=6))
#lines(Xt, type ="l")
#Xt = dynamicCounter(tent0, gImPAR(wx=1/30, wn=1/180, P50=6))
#lines(Xt, type ="l")
#Xt = dynamicCounter(tent0, gImPAR(wx=1/180, wn=1/180, P50=6))
#lines(Xt, type ="l")
