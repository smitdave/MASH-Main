# Waiting Times
tF = 1/2/24 # 30 minutes
tB = 3/24   # 3 hours
tR = 18/24  # 18 hours
tL = 1/2/24 # 30 minutes
tO = 1/24   # 1 hour

# Transitions out of F
PFF = .05
PFB = .93
PFD = 1-PFB-PFF

rFF = PFF/TF
rFB = PFB/TF
rFD = PFD/TF

# Transitions out of B
PBB = .1
PBF = .1
PBR = .75
PBD = 1-PBF-PBR-PBB

rBB = PBB/TB
rBF = PBF/TB
rBR = PBR/TB
rBD = PBD/TB

#Transitions out of L
PLL = .2
PLO = .75
PLD = 1-PLO-PLL

rLL = PLL/TL
rLO = PLO/TL
rLD = PLD/TL

#Transitions out of O
POO = .05
POL = .15
POB = .35
POF = .4
POD = 1-POL-POB-POF-POO

rOO = POO/TO
rOL = POL/TO
rOB = POB/TO
rOF = POF/TO
rOD = POD/TO

#Transitions out of R
PRF = .05
PRB = .1
PRL = .5
PRO = .35
PRD = 1-PRF-PRB-PRL-PRO
PRR = 0

rRR = 1/TR
rRF = PRF/TR
rRB = PRB/TR
rRL = PRL/TR
rRO = PRO/TR
rRD = PRD/TR

M = t(matrix(c(-rFF,rFB,0,0,0,rBF,-rBB,rBR,0,0,rRF,rRB,-rRR,rRL,rRO,0,0,0,-rLL,rLO,rOF,rOB,0,rOL,-rOO),ncol=5))
P = t(matrix(c(PFF,PFB,0,0,0,PBF,PBB,PBR,0,0,PRF,PRB,PRR,PRL,PRO,0,0,0,PLL,PLO,POF,POB,0,POL,POO),ncol=5))
expm::expm(M)
t = seq(0,5,.01)
RM = t*0
FM = RM
BM = RM
LM = RM
OM = RM
v = c(0,0,1,0,0)
for(i in 1:length(t)){
  temp = expm::expm(M*i/length(t))%*%v
  FM[i] = temp[1]
  BM[i] = temp[2]
  RM[i] = temp[3]
  LM[i] = temp[4]
  OM[i] = temp[5]
}

plot(t,FM,type="l")
lines(t,BM)
lines(t,RM)
lines(t,LM)
lines(t,OM)

plot(t,FM+BM+RM+LM+OM,type="l")
