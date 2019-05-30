tt = seq(1, 365, length.out=26)
St = 1+sin(2*pi*tt/365)
#plot(tt, St, type = "l")

iterX = function(t, X, h, St, r=1/200){
  P = exp(-14*r)
  A = 1-exp(-14*St[1+(t%%26)]*h)
  P*X + A*(1-X)
}

yrX = function(X, h, St, r=1/200){
  Xs = rep(0,26)
  for(t in 1:26){
    X = iterX(t,X,h,St,r)
    Xs[t] = X
  }
  Xs
}

Xeq = function(h,St,r=1/200, tol = 0.0001){
  X0 = 0
  Xs = yrX(h/(h+r),h,St,r)
  while(abs(mean(X0)-mean(Xs)) > tol){
    X0 = Xs
    Xs = yrX(Xs[26],h,St,r) 
  }
  Xs
}

Xs = Xeq(2/365, St)
plot(1:52, c(Xs,Xs), type = "l")

pr2h = function(X,t,St,r=1/200, ltol=0.0001){
  #Xs = Xeq(1/365,St,r,tol)
  errf = function(h, X, t, St, r, ltol){
    sum((Xeq(h, St, r, ltol)[t] - X)^2)
  }
  optimize(errf, c(0,1), maximum = FALSE, X=X,t=t,St=St,r=r,ltol=ltol)$min
}

par(mfrow = c(1,1))
#xx = .5
xx=c(.5, .3)
#tt = 15
tt=c(12, 18)
h.est = pr2h(xx,tt, St)
XX = Xeq(h.est,St)
plot(1:26, XX, type = "l", ylim = range(xx, XX))
points(tt,xx)
