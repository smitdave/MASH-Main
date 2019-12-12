xs = c(7.4,9.3,28.7)/100
hbar = c(.454,.958,1.45)
Ps = log10(c(313,732,203))

f = function(h){
  A = 19900
  B = .8354
  L = .2819
  return(log10(A*(1-B*exp(-L*h))))
}

g = function(x){
  gamma = x[1]
  I0 = x[2]
  L = x[3]
  A = 19900
  B = .8354
  M = .2819
  fh = A*(1-B*exp(-M*hbar))
  return(sum((Ps-fh*(1+exp(-L*I0))/(1+exp(L*(xs*Ps/(gamma*(1-xs))-I0))))^2))
}

gamma = 1
I0 = 10
L = 1
nlm(g,c(gamma,I0,L))

gamma = 6.5
I0 = 2.82
L = 

plot(hbar,Ps)
plot(hbar,fh*(1+exp(-L*I0))/(1+exp(L*(xs*Ps/(gamma*(1-xs))-I0))))

plot(hbar,f(hbar))

xx = seq(-5,5,.01)
test = xx^2+rnorm(length(xx),0,1)
testf = function(y){
  a = y[1]
  b = y[2]
  return(sum((a*xx^2+b-test)^2))
}
nlm(testf,c(10,5))
plot(xx,test)
lines(xx,.9987*xx^2)
