ET = function(){
  tRB = (tB+PBF*PFB*tF)/(1-PBF*PFB)
  tRF = (tF+tB)/(1-PBF*PFB)
  tRO = (tO+POL*PLO*tL)/(1-POL*PLO)+POF/(POF+POB)*tRF+POB/(POF+POB)*tRB
  tRL = (tO+tL)/(1-POL*PLO)+POF/(POF+POB)*tRF+POB/(POF+POB)*tRB
  Ttot = PRB*tRB+PRF*tRF+PRO*tRO+PRL*tRL+tR
  return(Ttot)
}

PS = function(){
  PBR/(1-PBF*PFB)*(PRB+PRF*PFB+(PRO+PRL*PLO)*(POB+POF*PFB)/(1-POL*PLO))
}

## numerically solve expected time in system for first bout, given proprotion
## which begin in state F PF (PB = 1-PF)
## zS is cdf of survival, zD is cdf of death. (zD ~ Coxian)
Q1 = matrix(c(-rF,rB*PBF,0,0,rF*PFB,-rB,0,0,rF*PFD,rB*PBD,0,0,0,rB*PBR,0,0),nrow=4)
Q1E = expm::expm(Q1*dt)
dQ = Q1E
zD = 0*t
zS = 0*t
PF = .5
pfb = c(PF,1-PF,0,0)
for(i in 2:length(t)){
  temp = pfb%*%Q1E
  zD[i] = temp[3]
  zS[i] = temp[4]
  Q1E = Q1E%*%dQ
}

PS1 = zS[length(zS)]
zD = zD/zD[length(zD)]
zS = zS/zS[length(zS)]
TD1 = sum(1-zD)*dt
TS1 = sum(1-zS)*dt

##written once TT, TD, and PP are known (time to successfully complete a bout, time to die
##in a bout, and probability of surviving a bout)

Lifespan = function(){
  (1-PS1)*TD1 + PS1*(TS1 + TD + TT*PP/(1-PP))
}

##human biting rate, given the fraction of bloodmeals on humans Q
Q = .9
HBR = function(){
  Q*PS1/(1-PP)/Lifespan()
}