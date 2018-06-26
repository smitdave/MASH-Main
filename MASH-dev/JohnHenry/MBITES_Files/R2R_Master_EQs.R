# Waiting Times
tF = 1/2/24 # 30 minutes
tB = 3/24   # 3 hours
tR = 18/24  # 18 hours
tL = 1/2/24 # 30 minutes
tO = 1/24   # 1 hour

# Transitions out of F
PFF = .05
PFB = .93/(1-PFF)
PFD = 1-PFB

rF = 1/TF*(1-PFF)

# Transitions out of B
PBB = .1
PBF = .1/(1-PBB)
PBR = .75/(1-PBB)
PBD = 1-PBF-PBR

rB = 1/TB*(1-PBB)

#Transitions out of L
PLL = .05
PLO = .93/(1-PLL)
PLD = 1-PLO

rL = 1/TL*(1-PLL)

#Transitions out of O
POO = .1
POL = .1/(1-POO)
POB = .4/(1-POO)
POF = .35/(1-POO)
POD = 1-POL-POB-POF

rO = 1/TO*(1-POO)

#Transitions out of R
PRR = 0
PRF = .05
PRB = .2
PRL = .4
PRO = .35
PRD = 1-PRF-PRB-PRL-PRO

rR = 1/TR*(1-PRR)

########### Infinitesimal Generator for Single Feeding Cycle, QI ###################

QI = matrix(c(-rR,0,0,0,0,0,rR*PRF,-rF,rB*PBF,0,rO*POF,0,rR*PRB,rF*PFB,-rB,0,rO*POB,0,rR*PRL,0,0,-rL,rO*POL,0,rR*PRO,0,0,rL*PLO,-rO,0,0,0,rB*PBR,0,0,0),nrow=6)
t = seq(0,15,.01)
R = 0*t
R[1] = 0
v0 = c(1,0,0,0,0,0)
for(i in 2:length(t)){
  temp = v0%*%expm::expm(QI*t[i])
  R[i] = temp[6]
}
plot(t,R/R[length(R)],type="l")
sum(1-R/R[length(R)])*.01
R[length(R)]

dR = 0*R
for(i in 1:(length(R)-1)){
  dR[i] = (R[i+1]-R[i])/.01/R[length(R)]
}
z = seq(0,15,.01)
plot(z,dR,type="l")
