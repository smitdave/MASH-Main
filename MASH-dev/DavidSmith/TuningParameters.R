

PAR1 = list(
  # Waiting Times
  tF = 6/24,   # 30 minutes
  tB = 3/24,   # 3 hours
  tR = 18/24,  # 18 hours
  tL = 6/24,   # 30 minutes
  tO = 3/24,   # 1 hour
  
  # Transitions out of F 
  P_FF = .05,
  P_FB = .93,
  
  # Transitions out of B 
  P_BF = .1,
  P_BB = .1, 
  P_BR = .75,
  
  #Transitions out of L 
  P_LL = .05,
  P_LO = .93,
  
  #Transitions out of O 
  P_OL = .1,
  P_OO = .1,
  P_OB = .4,
  P_OF = .35,
  
  #Transitions out of R
  P_RF = .05,
  P_RB = .2,
  P_RL = .4,
  P_RO = .35
) 

P.RR = Psi_RR(PAR1)
T.RR = T_RR(PAR1)
T.RR/-log(P.RR)
T.RR


```{r}
PAR1 = list(
  # Waiting Times
  tF = 2/24, # 2 hours
  tB = 6/24, # 3 hours
  tR = 1,    # 18 hours
  tL = 2/24, # 30 minutes
  tO = 6/24, # 1 hour
  
  # Transitions out of F 
  P_FF = .1,
  P_FB = .88,
  
  # Transitions out of B 
  P_BF = .1,
  P_BB = .1, 
  P_BR = .75,
  
  #Transitions out of L 
  P_LL = .05,
  P_LO = .93,
  
  #Transitions out of O 
  P_OL = .1,
  P_OO = .1,
  P_OB = .4,
  P_OF = .35,
  
  #Transitions out of R
  P_RF = .05,
  P_RB = .2,
  P_RL = .4,
  P_RO = .35
) 
```

```{r}
P.RR = Psi_RR(PAR)
T.RR = T_RR(PAR1)
T.RR/-log(P.RR)
T.RR
```