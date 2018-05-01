f = function(t){
  ifelse(t <= .5, sqrt(t), sqrt(1-t))
}

t = seq(0,1,.01)
plot(t,f(t),type="l",ylim = c(-1,1))
lines(t,-f(t))
lines(t,0*t)


BBf = function(Tfin,dt,a,b){
  t = 0
  i = 1
  x = rep(a,Tfin/dt)
  while(t<Tfin){
    x[i+1] = x[i]+rnorm(1,0,f((t-a)/(b-a)))
    i = i+1
    t = t+dt
  }
  return(x)
}

##Brownian Bridge function
##  Tfin : total length of bridge
##    dt : step size
##     a : initial height
##     b : final height
##    sd : standard deviation of underlying weiner process
BB = function(Tfin,dt,a,b,sd){
  t = 0
  i = 1
  drift = seq(0,Tfin,dt)*(b-a)/Tfin
  w = rep(0,Tfin/dt)
  while(t<Tfin){
    w[i+1] = w[i]+rnorm(1,0,sd)*dt
    i = i+1
    t = t+dt
  }
  t = seq(0,Tfin,dt)
  w = w[1:length(t)]
  bb = pmax(w - t/Tfin*w[length(w)]+drift+a,0)
  return(bb)
}

LevyBridge = function(Tfin,dt,a,b,sd){
  t = 0
  i = 1
  drift = seq(0,Tfin,dt)*(b-a)/Tfin
  w = rep(0,Tfin/dt)
  while(t<Tfin){
    w[i+1] = w[i]+rexp(1,1/sd)*dt
    i = i+1
    t = t+dt
  }
  t = seq(0,Tfin,dt)
  w = w[1:length(t)]
  lb = w - t/Tfin*w[length(w)]+drift+a
  return(pmax(lb,0))
}

BBTent = function(sd){
  plot(seq(0,1,.01),BB(1,.01,4,11,sd),type="l",xlim=c(0,20),ylim=c(0,12))
  lines(seq(1,10,.01),BB(9,.01,11,2,sd))
  lines(seq(10,20,.01),BB(10,.01,2,2,sd))
}

LBTent = function(sd){
  plot(seq(0,1,.01),LevyBridge(1,.01,4,11,sd),type="l",xlim=c(0,20),ylim=c(0,12))
  lines(seq(1,10,.01),LevyBridge(9,.01,11,2,sd))
  lines(seq(10,20,.01),LevyBridge(10,.01,2,2,sd))
}

plot(seq(0,12,.001),BBTent(20),type="l")
