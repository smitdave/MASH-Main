Stoch_SI = function(lambda,N,dt,Tfin){
  SI = matrix(0,nrow=Tfin/dt+1,ncol=2)
  SI[1,] = c(N-10,10)
  nTime = Tfin/dt+1
  for(i in 2:nTime){
    infectTimes = rexp(SI[i-1,1],lambda*SI[i-1,2])
    infect = sum(infectTimes<=dt)
    SI[i,1] = SI[i-1,1]-infect
    SI[i,2] = SI[i-1,2]+infect
  }
  return(SI)
}

Det_SI = function(lambda,dt,N,Tfin){
  SI = matrix(0,nrow=Tfin/dt+1,ncol=2)
  SI[1,] = c(N-10,10)
  nTime = Tfin/dt+1
  for(i in 2:nTime){
    infec = lambda*SI[i-1,1]*SI[i-1,2]
    SI[i,1] = SI[i-1,1]-infec*dt
    SI[i,2] = SI[i-1,2]+infec*dt
  }
  return(SI)
}

N = 100
dt = .01
Tfin = 100
lambda = .0005

t = seq(0,Tfin,dt)
SI = Stoch_SI(lambda,N,dt,Tfin)
lines(t,SI[,1],type="l",col="blue")
lines(t,SI[,2],type="l",col="red")

##fake data generation
dt = .1
t = seq(0,Tfin,dt)
SI = Det_SI(lambda,dt,N,Tfin)
plot(t,SI[,1],type="l",ylim=c(0,100))
lines(t,SI[,2])
