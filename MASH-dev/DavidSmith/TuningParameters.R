

PAR = list(
  # Waiting Times
  tF =  8/24, 
  tB =  3/24,   
  tR = 30/24,  
  tL =  8/24, 
  tO =  3/24, 
  
  # Transitions out of F 
  P_FF = .10,
  P_FB = .84,
  
  # Tr    ansitions out of B 
  P_BF = .15,
  P_BB = .05, 
  P_BR = .75,
  
  #Transitions out of L 
  P_LL = .10,
  P_LO = .84,
  
  #Transitions out of O 
  P_OL = .15,
  P_OO = .05,
  P_OB = .30,
  P_OF = .45,
  
  #Transitions out of R
  P_RF = .05,
  P_RB = .20,
  P_RL = .40,
  P_RO = .35
) 

PAR1=PAR
PAR1$P_RL = .75
PAR1$P_RO = 0
PAR1$P_OB = 0
PAR1$P_OF = 0.75

PAR2=PAR
PAR2$P_RL = 0
PAR2$P_RO = 0.75 
PAR2$P_OB = 0.75
PAR2$P_OF = 0

print(c(feed = T_RR(PAR), life=T_RR(PAR)/-log(Psi_RR(PAR)), S=1/(1-Psi_RR(PAR))))
print(c(feed = T_RR(PAR1), life=T_RR(PAR1)/-log(Psi_RR(PAR1)), S=1/(1-Psi_RR(PAR1))))
print(c(feed = T_RR(PAR2), life=T_RR(PAR2)/-log(Psi_RR(PAR2)), S=1/(1-Psi_RR(PAR2))))




