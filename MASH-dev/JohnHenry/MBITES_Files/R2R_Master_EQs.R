# Waiting Times
TF = 6/24 # 30 minutes
TB = 3/24   # 3 hours
TR = 18/24  # 18 hours
TL = 6/24 # 30 minutes
TO = 3/24   # 1 hour

# Transitions out of F
PFF = .15
PFB = .83/(1-PFF)
PFD = 1-PFB

tF = TF/(1-PFF)
rF = 1/tF

# Transitions out of B
PBB = .05
PBF = .15/(1-PBB)
PBR = .75/(1-PBB)
PBD = 1-PBF-PBR
tB = TB/(1-PBB)
rB = 1/tB

#Transitions out of L
PLL = .15
PLO = .83/(1-PLL)
PLD = 1-PLO
tL = TL/(1-PLL)
rL = 1/tL

#Transitions out of O
POO = .05
POL = .15/(1-POO)
POB = .3/(1-POO)
POF = .45/(1-POO)
POD = 1-POL-POB-POF
tO = TO/(1-POO)
rO = 1/tO

#Transitions out of R
PRR = 0
PRF = .05/(1-PRR)
PRB = .2/(1-PRR)
PRL = .4/(1-PRR)
PRO = .35/(1-PRR)
PRD = 1-PRF-PRB-PRL-PRO
tR = TR/(1-PRR)
rR = 1/tR

########### Infinitesimal Generator for Single Feeding Cycle, QI ###################

QI = matrix(c(-rR,0,0,0,0,0,rR*PRF,-rF,rB*PBF,0,rO*POF,0,rR*PRB,rF*PFB,-rB,0,rO*POB,0,rR*PRL,0,0,-rL,rO*POL,0,rR*PRO,0,0,rL*PLO,-rO,0,0,0,rB*PBR,0,0,0),nrow=6)

## define time vector, initial condition
dt = 10^-4
tfin = 10
t = seq(0,tfin,dt)
R = 0*t
R[1] = 0
D = 0*t
D[1] = 0
v0 = c(1,0,0,0,0,0)

## compute single matrix exponential; iterate by multiplying by this "time step" matrix 
## using semigroup property, ie exp(Q*(t+dt)) = exp(Q*t)*exp(Q*dt)

Qexp = expm::expm(QI*dt)
Q = Qexp
for(i in 2:length(t)){
  temp = v0%*%Q
  R[i] = temp[6]
  D[i] = 1-sum(temp)
  Q = Q%*%Qexp
}
PS = R[length(R)]
RR = R/PS
plot(t,RR,type="l")
TS = sum(1-RR)*dt
TD = sum(1-D/(1-PS))*dt

lines(t,D/(1-PS),type="l",col="red")

dR = 0*R
dD = 0*D
for(i in 1:(length(R)-1)){
  dR[i] = (R[i+1]-R[i])/dt/PS
  dD[i] = (D[i+1]-D[i])/dt/(1-PS)
}
plot(t,dR,type="l", ylim=c(0,.6))
abline(v = TS)
lines(t,dD,type="l",col="red")
abline(v = TD,col="red")
