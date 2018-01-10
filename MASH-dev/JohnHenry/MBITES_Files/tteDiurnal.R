par(mfrow=c(2,2))

ff = function(t, peak=0,amp=1){
  amp*(1+cos(2*pi*(t-peak)))
}

ttt = seq(0,1,1/24)

plot(ttt*24, ff(ttt), type = 'l', ylab = "Activity Level", xlab = "Time of Day", xaxt = "n") 
axis(1, c(0:4)*6, c("12 am", "6 am", "12 pm", "6 pm", "12 am"))

gg = function(t,lam){
  lam*(1+cos(2*pi*t))*exp(-lam*(t+sin(2*pi*t)/(2*pi)))
}

hh = function(t,lam){
  1-exp(-lam*(t+sin(2*pi*t)/(2*pi)))
}

t = 1:700/100
plot(t,hh(t,1),type="l",ylab = "tteCDF")

newton = function(f,J,x0,tol) {
  #standard newton's method, compute f and Jacobian as functions
  while (norm(f(x0),'2') > tol) {
    x0 = x0 - qr.solve(J(x0),f(x0))
  }
  return(x0)
}

tteDiurnal = function(N,lam){
  v = rep(0,N)
  u = rexp(N,lam)
  for(i in 1:N){
    f = function(t,s=u[i]){
      1.1*t+sin(2*pi*t)/(2*pi)-s
    }
    fp = function(t){
      1.1+cos(2*pi*t)
    }
    v[i] = newton(f,fp,u[i],10^-2)
  }
  return(v)
}

v = tteDiurnal(10000,1)

count = c(0,hist(v,freq=F,breaks=100)$count)
plot(seq(0,7,7/(length(count)-1)),cumsum(count)/sum(count),type="l",ylab="empirical tteCDF",ylim=c(0,1))


##################### second method - rejection method #####################
#####################         more efficient           ##################### 

par(mfrow=c(1,1))

gg = function(t,lam){
  lam*(1+cos(2*pi*t))*exp(-lam*(t+sin(2*pi*t)/(2*pi)))
}

GG = function(t,lam){
  lam*exp(-lam*t)
}

plot(t,gg(t,1),type="l")
lines(t,2.3*GG(t,1))

tteDiurnal = function(N,lam){
  v = rep(0,N)
  for(i in 1:N){
    c = 2.3
    vtemp = 0
    tt = 2
    u = 1
    while(tt*u > 1){
      v[i] = rexp(1,2*lam)
      tt = c*GG(v[i],lam)/gg(v[i],lam)
      u = runif(1)
    }
  }
  return(v)
}

hist(tteDiurnal(100000,1),freq=F,breaks=100)
