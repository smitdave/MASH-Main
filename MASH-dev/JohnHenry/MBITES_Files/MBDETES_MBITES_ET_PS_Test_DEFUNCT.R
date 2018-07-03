# Waiting Times
tF = 1/2/24 # 30 minutes
tB = 3/24   # 3 hours
tR = 18/24  # 18 hours
tL = 1/2/24 # 30 minutes
tO = 1/24   # 1 hour

# Transitions out of F 
F2F = .05
F2B = .93/(1-F2F)
F2D = 1-F2B

rFB = F2B/tF
rFD = F2D/tF

# Transitions out of B 
B2B = .1 
B2F = .1/(1-B2B)
B2R = .75/(1-B2B)
B2D = 1-B2F-B2R

rBF = B2F/tB
rBR = B2R/tB
rBD = B2D/tB

#Transitions out of L 
L2L = .05
L2O = .93/(1-L2L)
L2D = 1-L2O

rLO = L2O/tL
rLD = L2D/tL

#Transitions out of O 
O2O = .1
O2L = .1/(1-O2O)
O2B = .4/(1-O2O)
O2F = .35/(1-O2O)
O2D = 1-O2L-O2B-O2F

rOL = O2L/tO
rOB = O2B/tO
rOF = O2F/tO
rOD = O2D/tO

#Transitions out of R
R2F = .05
R2B = .2
R2L = .4
R2O = .35
R2D = 1-R2F-R2B-R2L-R2O

rRF = R2F/tR
rRB = R2B/tR
rRL = R2L/tR
rRO = R2O/tR
rRD = R2D/tR

PS = function(){
  
  PRL = rRL/(rRL+rRO+rRF+rRD+rRB)
  PRO = rRO/(rRL+rRO+rRF+rRD+rRB)
  PRF = rRF/(rRL+rRO+rRF+rRD+rRB)
  PRD = rRD/(rRL+rRO+rRF+rRD+rRB)
  PRB = rRB/(rRL+rRO+rRF+rRD+rRB)
  
  PLD = rLD/(rLD+rLO)
  PLO = rLO/(rLD+rLO)
  TL = 1/(rLD+rLO)
  
  POL = rOL/(rOL+rOD+rOF+rOB)
  POD = rOD/(rOL+rOD+rOF+rOB)
  POF = rOF/(rOL+rOD+rOF+rOB)
  POB = rOB/(rOL+rOD+rOF+rOB)
  TO = 1/(rOL+rOD+rOF)
  
  PFB = rFB/(rFB+rFD)
  PFD = rFD/(rFB+rFD)
  TF = 1/(rFB+rFD)
  
  PBD = rBD/(rBD+rBF+rBR)
  PBF = rBF/(rBD+rBF+rBR)
  PBR = rBR/(rBD+rBF+rBR)
  TB = 1/(rBD+rBF+rBR)
  
  PS = PBR/(1-PFB*PBF)*(PRB+PRF*PFB+(PRO+PRL*PLO)*(POB+POF*PFB)/(1-POL*PLO))
  return(PS)
}

ET = function(condition='condition'){
  
  if(condition=='Survive'){
    rRD = 0
    rLD = 0
    rOD = 0
    rFD = 0
    rBD = 0
  }
  
  if(condition == 'Die'){
    rBR = 0
  }
  
  PRL = rRL/(rRL+rRO+rRF+rRD+rRB)
  PRO = rRO/(rRL+rRO+rRF+rRD+rRB)
  PRF = rRF/(rRL+rRO+rRF+rRD+rRB)
  PRD = rRD/(rRL+rRO+rRF+rRD+rRB)
  PRB = rRB/(rRL+rRO+rRF+rRD+rRB)
  TR = 1/(rRL+rRO+rRF+rRD+rRB)
  
  PLD = rLD/(rLD+rLO)
  PLO = rLO/(rLD+rLO)
  TL = 1/(rLD+rLO)
  
  POL = rOL/(rOL+rOD+rOF+rOB)
  POD = rOD/(rOL+rOD+rOF+rOB)
  POF = rOF/(rOL+rOD+rOF+rOB)
  POB = rOB/(rOL+rOD+rOF+rOB)
  TO = 1/(rOL+rOD+rOF+rOB)
  
  PFB = rFB/(rFB+rFD)
  PFD = rFD/(rFB+rFD)
  TF = 1/(rFB+rFD)
  
  PBD = rBD/(rBD+rBF+rBR)
  PBF = rBF/(rBD+rBF+rBR)
  PBR = rBR/(rBD+rBF+rBR)
  TB = 1/(rBD+rBF+rBR)
  
  TRL = (TL+PLO*TO)/(1-POL*PLO) + PLO*((POF+POB*PBF)*TF+(POF*PFB+POB)*TB)/(1-PFB*PBF)
  TRO = (TO+PLO*TO)/(1-POL*PLO) + ((POF+POB*PBF)*TF+(POF*PFB+POB)*TB)/(1-PFB*PBF)
  TRF = (TF+PFB*TB)/(1-PFB*PBF)
  TRB = (TB+PBF*TF)/(1-PFB*PBF)
  
  ET = PRL*TRL+PRO*TRO+PRF*TRF+PRB*TRB+TR
  
  return(ET)
}

PS()
ET('Survive')

LS = function(){
  psi1 = PFB*PBR/(1-PFB*PBF)
  psiS = PS()
  L = (1-)
}