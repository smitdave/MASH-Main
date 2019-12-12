library(fAsianOptions)

## PDG Parameters
Fmax = .8835
Fb = 3.851
Fhalf = 3.463
r = 2*10/(1-exp(-.0984))/365

## average wait of 2 days before treatment with fever
tau0=365/2
R = function(P){
  taubar = tau0*Fmax*exp(Fb*P)/(exp(Fb*Fhalf)+exp(Fb*P))
  K1 = kummerM(-h/r,taubar/r,taubar/r+2)
  K2 = kummerM(-h/r,taubar/r,taubar/r+1)
  return(taubar/(1-(r*K1)/((r+taubar)*K2)))
}

AM = 19960
BM = .6391
LM = .4046
f = function(h){
  log10(AM*(1-BM*exp(-LM*h)))
}

Pbar = function(h,I){
  f(h)*(1-sigma(I))
}


ff = function(z){
  x = z[1]
  I = z[2]
  zz[1] = h*(1-x)-R(Pbar(h,I))*x
  zz[2] = x*Pbar(h,I)-gamma*(1-x)*I
}