## RX
rRL = 1
rRO = 1
rRF = 1
rRD = .1
rRB = 1
## LX
rLD = .1
rLO = 1
## OX
rOL = 1
rOD = .1
rOF = 1
## FX
rFB = 1
rFD = .1
## BX
rBD = .1
rBF = 1
rBR = 1


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
  
  PLD = rLD/(rLD+rLO)
  PLO = rLO/(rLD+rLO)
  TL = 1/(rLD+rLO)
  
  POL = rOL/(rOL+rOD+rOF)
  POD = rOD/(rOL+rOD+rOF)
  POF = rOF/(rOL+rOD+rOF)
  TO = 1/(rOL+rOD+rOF)
  
  PFB = rFB/(rFB+rFD)
  PFD = rFD/(rFB+rFD)
  TF = 1/(rFB+rFD)
  
  PBD = rBD/(rBD+rBF+rBR)
  PBF = rBF/(rBD+rBF+rBR)
  PBR = rBR/(rBD+rBF+rBR)
  TB = 1/(rBD+rBF+rBR)
  
  ET = PRB*(TB/(1-PBF*PFB)+PBF*TB/(1-PBF*PFB)) + PRF*(TF/(1-PBF*PFB)+PFB*T/(1-PBF*PFB)) +
    PRO*(TO/(1-POL*PLO)+POL*TL/(1-POL*PLO) + POF/(1-POL*PLO)*(TF/(1-PBF*PFB)+PFB*TB/(1-PBF*PFB)))+
    PRL*(TL/(1-POL*PLO)+PLO*TO/(1-POL*PLO)+PLO*POF/(1-POL*PLO)*(TF/(1-PBF*PFB)+PFB*TB/(1-PBF*PFB)))
  
  return(ET)
}

PS = function(){
  
  PRL = rRL/(rRL+rRO+rRF+rRD+rRB)
  PRO = rRO/(rRL+rRO+rRF+rRD+rRB)
  PRF = rRF/(rRL+rRO+rRF+rRD+rRB)
  PRD = rRD/(rRL+rRO+rRF+rRD+rRB)
  PRB = rRB/(rRL+rRO+rRF+rRD+rRB)
  
  PLD = rLD/(rLD+rLO)
  PLO = rLO/(rLD+rLO)
  TL = 1/(rLD+rLO)
  
  POL = rOL/(rOL+rOD+rOF)
  POD = rOD/(rOL+rOD+rOF)
  POF = rOF/(rOL+rOD+rOF)
  TO = 1/(rOL+rOD+rOF)
  
  PFB = rFB/(rFB+rFD)
  PFD = rFD/(rFB+rFD)
  TF = 1/(rFB+rFD)
  
  PBD = rBD/(rBD+rBF+rBR)
  PBF = rBF/(rBD+rBF+rBR)
  PBR = rBR/(rBD+rBF+rBR)
  TB = 1/(rBD+rBF+rBR)
  
  PS = (1-PRD)*PBR/(1-PBF*PFB)*(PRB+PRF*PFB+PRO*POF*PFB/(1-POL*PLO)+PRL*PLO*POF*PFB/(1-POL*PLO))
  return(PS)
}

n = 1:20
plot(n,PS()^(n-1)*(1-PS()),type="l",ylim=c(0,1))
lines(n,cumsum(PS()^(n-1)*(1-PS())),lty=2)
