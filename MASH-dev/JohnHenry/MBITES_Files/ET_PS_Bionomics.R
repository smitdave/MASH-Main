##Analytic Expected Time per Bout
AET = function(){
  tRB = (tB+PBF*PFB*tF)/(1-PBF*PFB)
  tRF = (tF+tB)/(1-PBF*PFB)
  tRO = (tO+POL*PLO*tL)/(1-POL*PLO)+POF/(POF+POB)*tRF+POB/(POF+POB)*tRB
  tRL = (tO+tL)/(1-POL*PLO)+POF/(POF+POB)*tRF+POB/(POF+POB)*tRB
  Ttot = PRB*tRB+PRF*tRF+PRO*tRO+PRL*tRL+tR
  return(Ttot)
}

##Analytic Probability of Survival
APS = function(){
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

##Computed probablity of surviving the first bout
PS1 = zS[length(zS)]
zD = zD/zD[length(zD)]
zS = zS/zS[length(zS)]
##Computed expected time in first bout given death
TD1 = sum(1-zD)*dt
##Computed expected time in first bout given survival
TS1 = sum(1-zS)*dt

##Lifespan once TS, TS1, TD1, TD, and PS, PS1 are known
##TS  := time to successfully complete a bout,
##TS1 := time to successfully complete the first bout,
##TD  := time to die, 
##TD1 := time to die in the first bout,
##PS  := probability of surviving a typical bout,
##PS1 := probability of surviving the first bout

Lifespan = function(){
  (1-PS1)*TD1 + PS1*(TS1 + TD + TS*PS/(1-PS))
}

##human biting rate, given:
##Q := the fraction of bloodmeals on humans
Q = .5
HBR = function(){
  Q*PS1/(1-PS)/Lifespan()
}

##Expected number of viable eggs laid in a mosquito lifetime, G, given:
##nu := mean clutch size
nu = 100
G = function(){
  nu*PS1*((PRL+PRO)/(1-PS)+(POB*POF)*(PRL*PLO+PRO)/(1-PLO*POL))
}

##Vectorial capacity, given:
##Q  := fraction of blood meals taken from humans
##nu := mean clutch size
##n  := extrinsic incubation period n
Q = .5
n = 21
VC = function(){
  G()*(Q*PS1/(1-PS))^2*exp(-n/Lifespan())/Lifespan()
}

##Basic reproductive number, given:
##b := transmission efficiency from mosquitoes to humans
##c := transmission efficiency from humans to mosquitoes
##r := human rate of recovery
b = .1
c = .1
r = .005
R0 = function(){
  VC()*b*c/r
}

