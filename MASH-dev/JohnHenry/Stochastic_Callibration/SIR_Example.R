Stoch_SIR = function(lambda,gamma,N,dt,Tfin){
  SIR = matrix(0,nrow=Tfin/dt+1,ncol=3)
  SIR[1,] = c(N-10,10,0)
  nTime = Tfin/dt+1
  for(i in 2:nTime){
    infect = 0
    recover = 0
    if(SIR[i-1,2]>0){
      infectTimes = rexp(SIR[i-1,1],lambda*SIR[i-1,2])
      infect = sum(infectTimes<=dt)
      recoverTimes = rexp(SIR[i-1,2],gamma)
      recover = sum(recoverTimes<=dt)
    }
    SIR[i,1] = SIR[i-1,1]-infect
    SIR[i,2] = SIR[i-1,2]+infect-recover
    SIR[i,3] = SIR[i-1,3]+recover
  }
  return(SIR)
}

Det_SIR = function(lambda,gamma,dt,N,Tfin){
  SIR = matrix(0,nrow=Tfin/dt+1,ncol=3)
  SIR[1,] = c(N-10,10,0)
  nTime = Tfin/dt+1
  for(i in 2:nTime){
    infect = lambda*SIR[i-1,1]*SIR[i-1,2]
    recover = gamma*SIR[i-1,2]
    SIR[i,1] = SIR[i-1,1]-infect*dt
    SIR[i,2] = SIR[i-1,2]+(infect-recover)*dt
    SIR[i,3] = SIR[i-1,3]+recover*dt
  }
  return(SIR)
}

N = 100
dt = .01
Tfin = 50
lambda = .01
gamma = .1


##fake data generation
t = seq(0,Tfin,dt)
DSIR = Det_SIR(lambda,gamma,dt,N,Tfin)
plot(t,DSIR[,1],type="l",ylim=c(0,100))
lines(t,DSIR[,2])
lines(t,DSIR[,3])

for(i in 1:10){
  SSIR = Stoch_SIR(lambda,gamma,N,dt,Tfin)
  lines(t,SSIR[,1],type="l",col="blue",ylim=c(0,100))
  lines(t,SSIR[,2],col="red")
  lines(t,SSIR[,3],col="green")
}

SIR = 0*SIR
for(i in 1:20){
  SIR = SIR + Stoch_SIR(lambda,gamma,N,dt,Tfin)
}
SIR = SIR/20

