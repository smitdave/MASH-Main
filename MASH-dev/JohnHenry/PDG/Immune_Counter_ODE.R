dt = .01
tfin = 5
I = rep(0,tfin/dt)
pars = list(alpha = 5, h = .5, Pbar = 4, r = 2/3,L=.5,K=.5,h0=1,I0=5,gamma=5)

sigma = function(x,a,x0){
  1/(exp(-a*(x-x0))+1)
}

f = function(t,I,pars){
  h = pars$h #rpois(1,pars$h)
  #pars$alpha*h*pars$Pbar/pars$r*sigma(h,pars$L,pars$h0)*(1-sigma(I,pars$K,pars$I0))-pars$gamma*I
  pars$alpha*h*pars$Pbar/pars$r*(1-sigma(I,pars$K,pars$I0))-pars$gamma*I
}


h = seq(.01,tfin,.01)
Ih = rep(0,length(h))
for(j in 1:length(h)){
  
  pars$h = h[j]

  for(i in 1:(tfin/dt-1)){
    t = dt*i
    I[i+1] = I[i] + dt*f(t,I[i],pars)
  }

  Ih[j] = I[length(I)]
}

plot(h,Ih,type="l",xlab="Transmission Intensity: Force of Infection",ylab="Average Immune Level",main="Immunity across Transmission Intensities")

plot(h,sigma(Ih,pars$K,pars$I0),type="l",xlab="Transmission Intensity: Force of Infection",ylab="Average Immune Effect",ylim=c(0,1),main="Immune Effect across Transmission Intensities")
#lines(h,sigma(h,pars$L,pars$h0))

P = pars$Pbar*sigma(h,pars$L,pars$h0)*(1-sigma(Ih,pars$K,pars$I0))

plot(h,P,type="l",xlab="Transmission Intensity: Force of Infection",ylab="Average log10 Parasite Density",main="Parasite Densities across Transmission Intensities")



t = seq(0,1,dt)
plot(t,I[1:101],type="l")


#################################
#################################
#################################

x = seq(0,1,.01)
f = function(x){x*(1+e^x)}
plot(x,f(x),type="l")
lines(f(x),x)
lines(x,x,lty=2)

plot(f(x),x,type="l")
