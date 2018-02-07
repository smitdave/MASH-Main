
################# First Method - CDF Inversion Using Newton's Method ###################
#################                 Less Efficient                     ###################

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


##################### Second Method - Rejection Method #####################
#####################           More Efficient         ##################### 

par(mfrow=c(1,1))

## need to define pdf f1, dominating pdf f2, and constant ST c*f2 > f1
## the smaller the constant, the more efficient the method

## f1
gg = function(t,lam){
  lam*(1+cos(2*pi*t))*exp(-lam*(t+sin(2*pi*t)/(2*pi)))
}

## f2
GG = function(t,lam){
  lam*exp(-lam*t)
}

## to find minimum c to optimize efficiency, need to solve
## f1 <= c*f2 for minimal value of c; in this case,
## this becomes the maximum value of the function
## 1+cos(2*pi*t)-2*pi*sin(2*pi*t)-sin^2(2*pi*t)

g = function(t,lam){
  (1+cos(2*pi*t))*exp(-lam*sin(2*pi*t)/(2*pi))
}

gp = function(t){
 cos(2*pi*t)+1+2*pi*sin(2*pi*t)-sin(2*pi*t)^2
}

gpp = function(t){
  2*pi*(-sin(2*pi*t)+2*pi*cos(2*pi*t)+2*sin(2*pi*t)*cos(2*pi*t))
}

xval = newton(gp,gpp,2,10^-4)
gpp(xval)
lam = 1
c = g(xval,lam)

t = seq(0,7,.01)
## plotting to show dominance with constant c = 2.0495
plot(t,gg(t,1),type="l")
lines(t,c*GG(t,1))
#polygon(c(t,rev(t)),c(gg(t,1),rev(c*GG(t,1))),col="red")


tteDiurnal = function(N,lam){
  v = rep(0,N)
  for(i in 1:N){
    c = 2.0495
    vtemp = 0
    tt = 2
    u = 1
    while(tt*u > 1){
      v[i] = rexp(1,lam)
      tt = c*GG(v[i],lam)/gg(v[i],lam)
      u = runif(1)
    }
  }
  return(v)
}

count = c(0,hist(tteDiurnal(100000,1),freq=F,breaks=100)$count)
plot(seq(0,7,7/(length(count)-1)),cumsum(count)/sum(count),type="l",ylab="empirical tteCDF",ylim=c(0,1))
