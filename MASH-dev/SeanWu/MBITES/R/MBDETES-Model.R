###############################################################################
#         __  _______  ____  _______________________
#        /  |/  / __ )/ __ \/ ____/_  __/ ____/ ___/
#       / /|_/ / __  / / / / __/   / / / __/  \__ \
#      / /  / / /_/ / /_/ / /___  / / / /___ ___/ /
#     /_/  /_/_____/_____/_____/ /_/ /_____//____/
#
#     MBDETES - Model (differential equations) functions
#     MBITES Team
#     August 2018
#
###############################################################################

#' MBDETES: Model Specification
#'
#' @section Background:
#'
#' Under some restricted conditions on waiting times, MBITES is what we would get if we translated MBITES into a stochastic model, under the Gillespie algorithm. We can, therefore, use MBDETES to compute expectations for MBITES under these same contitions.
#'
#' The utilties in MBDETES-Auxiliary.R and MBDETES-Model.R outputs state transitions as described in the following.
#'
#'  * Let \eqn{0\leq P_{X,Y}\leq 1} denote the probability of making a transition from state \eqn{X} to state \eqn{Y} after one bout
#'  * The probability of dying, \eqn{0 \leq P_{X,D} \leq 1} must satifsy the condition \deqn{\sum_{Y\neq D}P_{X,Y} =1-P_{X,D}}
#'  * Since there is the probability of repeating a state, we must compute the probability of making a transition out of the state and ending up alive and in a new state. We let this quantity be \eqn{\Psi_{X,Y}}
#'  * Let \eqn{T_{X}} the average time spent in a bout of type \eqn{X}.
#'  * Let \eqn{T_{X,Y}} denote the expected waiting time to make the transition from \eqn{X} to \eqn{Y}.
#'
#' @section Differential equations:
#'
#' In some sense, we can rewrite MBDETES as a conservation equation in the following way:
#'  \deqn{\left[ \begin{array}{c}dF/dt \\ dB/dt \\ dR/dt \\ dL/dt \\ dO/dt \\ dD/dt\end{array}\right]=
#'  \left[ \begin{array}{ccccc}
#'  P_{F,F}-1 & P_{F,B} & 0& 0 & 0 & P_{F,D} \\
#'  P_{B,F} & P_{B,B}-1 & P_{B,R} & 0 &0 & P_{B,D} \\
#'  P_{R,F} & P_{R,B} & -1 & P_{R,L} & P_{R,O}& P_{R,D} \\
#'  0 & 0 & 0 & P_{L,L}-1& P_{L,O} & P_{L,D} \\
#'  P_{O,F} & P_{O,B} & 0 & P_{O,L} & P_{O,O}-1 & P_{O,D} \\
#'  P_{F,D} & P_{B,D} & P_{R,D} & P_{L,D} & P_{O,D} & 0
#'  \end{array}\right] \left[ \begin{array}{c}F/t_F\\B/t_B\\R/t_R\\L/t_R\\O/t_O \\D   \\ \end{array}\right]}
#'
#'
#' @name MBDETES-Model
NULL
#> NULL


###############################################################################
# MBDETES transition probabilities
###############################################################################

#' MBDETES: Model Parameters
#'
#' Return a named list of parameters for MBDETES.
#'
#'
#' @param tF sojourn time in blood feeding search bout
#' @param tB sojourn time in blood feeding attempt bout
#' @param tR sojourn time in post-prandial resting bout
#' @param tL sojourn time in egg laying search bout
#' @param tO sojourn time in egg laying attempt bout
#' @param P_FF blood feeding search to search transition probability
#' @param P_FB blood feeding search to attempt transition probability
#' @param P_BF blood feeding attempt to search transition probability
#' @param P_BB blood feeding attempt to attempt transition probability
#' @param P_BR blood feeding attempt to post-prandial resting transition probability
#' @param P_LL
#' @param P_LO
#' @param P_OL
#' @param P_OO
#' @param P_OB
#' @param P_OF
#' @param P_RF post-prandial resting to blood feeding search transition probability
#' @param P_RB post-prandial resting to blood feeding attempt transition probability
#' @param P_RL post-prandial resting to egg laying search transition probability
#' @param P_RO post-prandial resting to egg laying attempt transition probability
#'
#' @export
MBDETES_Parameters <- function(
  # Waiting Times
  tF = 6/24, # 30 minutes
  tB = 3/24,   # 3 hours
  tR = 18/24,  # 18 hours
  tL = 6/24, # 30 minutes
  tO = 3/24,   # 1 hour

  # Transitions out of F
  P_FF = .15,
  P_FB = .83,

  # Transitions out of B
  P_BF = .15,
  P_BB = .05,
  P_BR = .75,

  #Transitions out of L
  P_LL = .15,
  P_LO = .83,

  #Transitions out of O
  P_OL = .15,
  P_OO = .05,
  P_OB = .3,
  P_OF = .45,

  #Transitions out of R
  P_RF = .05,
  P_RB = .2,
  P_RL = .4,
  P_RO = .35
){
  PAR <- list(
    tF = tF, tB = tB, tR = tR, tL = tL, tO = tO,P_FF = P_FF, P_FB = P_FB,
    P_BF = P_BF, P_BB = P_BB, P_BR = P_BR, P_LL = P_LL, P_LO = P_LO,
    P_OL = P_OL, P_OO = P_OO, P_OB = P_OB, P_OF = P_OF,
    P_RF = P_RF, P_RB = P_RB, P_RL = P_RL, P_RO = P_RO
  )
  return(MBDETES_MortalityPars(PAR))
}

#' MBDETES: Mortality Probabilities
#'
#' Calculate {all states} -> D probabilities.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#' @return the input list augmented with mortality probabilities.
#'
#' @export
MBDETES_MortalityPars <- function(PAR){with(PAR,{
  PAR$P_FD = 1-P_FB-P_FF
  PAR$P_BD = 1-P_BF-P_BB-P_BR
  PAR$P_RD = 1-P_RF-P_RB-P_RL-P_RO
  PAR$P_LD = 1-P_LL-P_OO
  PAR$P_OD = 1-P_OF-P_OB-P_OL-P_OO
  PAR
})}


###############################################################################
# MBDETES holding times
###############################################################################

#' MBDETES: Probability of F -> B Transition
#'
#' The transition is calculated according to the recursion \eqn{\Psi_{F,B} = P_{F,B} + P_{F,F} \Psi_{F,B} = \frac{P_{F,B}}{1-P_{F,F}}}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
Psi_FB <- function(PAR){with(PAR,{P_FB/(1-P_FF)})}

#' MBDETES: Holding Time of F -> B Transition
#'
#' The holding time in F, conditional on transitioning to B is calculated according to \eqn{T_{F,B} =  T_F + P_{F,F} T_{F,B} = \frac{T_F}{1-P_{F,F}}}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
T_FB <- function(PAR){with(PAR,{tF/(1-P_FF)})}

#' MBDETES: Probability of B -> R Transition
#'
#' The transition is calculated according to the recursion \eqn{\Psi_{B,R} = P_{B,R} + \left(P_{B,B} + P_{B,F} \Psi_{F,B} \right) \Psi_{B,R}}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
Psi_BR <- function(PAR){with(PAR,{
  P_BR/(1-P_BB-P_BF*Psi_FB(PAR))
})}

#' MBDETES: Holding Time of B -> R Transition
#'
#' The holding time in B, conditional on transitioning to R is calculated according to \eqn{T_{B,R} = T_B + P_{B,B} T_{B,R} + P_{B,F} \Psi_{F,B} \left(T_{F,B} +  T_{B,R} \right) =  \frac{T_B +  P_{B,F} \Psi_{F,B} T_{F,B}}{1-P_{B,B} - P_{B,F} \Psi_{F,B}}}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
T_BR <- function(PAR){with(PAR,{
  (tB+P_BF*Psi_FB(PAR)*T_FB(PAR))/(1-P_BB-P_BF*Psi_FB(PAR))
})}

#' MBDETES: Holding Time of F -> R Transition
#'
#' The holding time in F, conditional on transitioning to R is such that the time from $B$ to $R$ includes the loop back into $F$, so the time from $F$ to $R$ is simply the sum of the two times \code{\link{T_FB}} and \code{\link{T_BR}}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
T_FR <- function(PAR){with(PAR,{
  T_FB(PAR) + T_BR(PAR)
})}

#' MBDETES: Probability of L -> O Transition
#'
#' The transition is calculated according to the recursion \eqn{\Psi_{L,O}= P_{L,O} + P_{L,L} \Psi_{L,O} = \frac{P_{L,O}}{1-P_{L,L}}}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
Psi_LO <- function(PAR){with(PAR,{
  P_LO/(1-P_LL)
})}

#' MBDETES: Holding Time of L -> O Transition
#'
#' The holding time in L, conditional on transitioning to O is calculated according to \eqn{T_{L,O}  = T_L + P_{L,L} T_{L,O} = \frac{T_L}{1-P_{L,L}}}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
T_LO <- function(PAR){with(PAR,{
  tL/(1-P_LL)
})}

#' MBDETES: Probability of O -> F Transition
#'
#' The probability of going from \eqn{O} to \eqn{F} or \eqn{B} is \deqn{\Psi_{O,F|B} = P_{O,F} + P_{O,B} + \left(P_{O,O} + P_{O,L} \Psi_{L,O} \right) \Psi_{O,F|B}}
#' \deqn{\Psi_{O,F|B} = \frac{P_{O,F}+ P_{O,B}}{1-P_{O,O} - P_{O,L} \Psi_{L,O}}}
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
Psi_OF <- function(PAR){with(PAR,{
  P_OF/(1-P_OO-P_OL*Psi_LO(PAR))
})}

#' MBDETES: Probability of O -> B Transition
#'
#' The probability of going from \eqn{O} to \eqn{F} or \eqn{B} is \deqn{\Psi_{O,F|B} = P_{O,F} + P_{O,B} + \left(P_{O,O} + P_{O,L} \Psi_{L,O} \right) \Psi_{O,F|B}}
#' \deqn{\Psi_{O,F|B} = \frac{P_{O,F}+ P_{O,B}}{1-P_{O,O} - P_{O,L} \Psi_{L,O}}}
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
Psi_OB <- function(PAR){with(PAR,{
  P_OB/(1-P_OO-P_OL*Psi_LO(PAR))
})}

#' MBDETES: Holding Time of L -> O or B Transition
#'
#' The holding time in L, conditional on transitioning to O or B, is given by:
#' \deqn{T_{O,F|B} = T_O + P_{O,O} T_{O,F|B} + P_{O,L} \Psi_{L,O} \left(T_L + T_{O,F|B} \right)}
#' \deqn{T_{O,F|B}= \frac{T_O  + P_{O,L} \Psi_{L,O} T_{L,O}}{1 - P_{O,O} - P_{O,L} \Psi_{L,O}}}
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
T_OFB <- function(PAR){with(PAR,{
  (tO + P_OL*Psi_LO(PAR)*T_LO(PAR))/(1-P_OO-P_OL*Psi_LO(PAR))
})}

#' MBDETES: Probability of F -> R Transition
#'
#' The probability of going from \eqn{F} to \eqn{R}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
Psi_FR <- function(PAR){with(PAR,{
  Psi_FB(PAR)*Psi_BR(PAR)
})}

#' MBDETES: Probability of O -> R Transition
#'
#' The probability of going from \eqn{O} to \eqn{R}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
Psi_OR <- function(PAR){with(PAR,{
  Psi_OF(PAR)*Psi_FR(PAR)+Psi_OB(PAR)*Psi_BR(PAR)
})}

#' MBDETES: Probability of L -> R Transition
#'
#' The probability of going from \eqn{L} to \eqn{R}.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
Psi_LR <- function(PAR){with(PAR,{
  Psi_LO(PAR)*Psi_OR(PAR)
})}

#' MBDETES: Probability of R -> R Transition
#'
#' The probability of going from \eqn{R} to \eqn{R} (survival through one feeding cycle).
#' \deqn{\Psi_{R,R} = \sum_{X \neq \left\{R,D\right\}} P_{R,X} \Psi_{X,R}}
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
Psi_RR <- function(PAR){with(PAR,{
  P_RF*Psi_FR(PAR)+
  P_RB*Psi_BR(PAR)+
  P_RL*Psi_LR(PAR)+
  P_RO*Psi_OR(PAR)
})}

#' MBDETES: Holding Time of O -> R Transition
#'
#' The holding time in O, conditional on transitioning to R.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
T_OR <- function(PAR){with(PAR,{
  T_OFB(PAR) + P_OB/(P_OB+P_OF)*T_BR(PAR)+ P_OF/(P_OB+P_OF)*T_FR(PAR)
})}

#' MBDETES: Holding Time of L -> R Transition
#'
#' The holding time in L, conditional on transitioning to R.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
T_LR <- function(PAR){with(PAR,{
  T_LO(PAR) + T_OR(PAR)
})}

#' MBDETES: Holding Time of R -> R Transition
#'
#' The holding time in R, conditional on transitioning to R (one surccessful feeding cycle).
#' \deqn{T_{R,R} = \sum_{X \neq \left\{R,D\right\}} P_{R,X} \Psi_{X,R} T_{X,R}}
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#'
T_RR <- function(PAR){with(PAR,{
  (tR +
  P_RF*T_FR(PAR)+
  P_RB*T_BR(PAR)+
  P_RL*T_LR(PAR)+
  P_RO*T_OR(PAR))
})}


###############################################################################
# MBDETES ODE models
###############################################################################

#' MBDETES: Feeding Cycle (R -> R) ODE Model Derivatives
#'
#' Evaluate derivatives for feeding cycle model.
#'
#' @param t time at which gradient of model is requested
#' @param X state vector of model
#' @param P output of \code{\link{MBDETES_Parameters}}
#'
#' @export
MBDETES_R2R_ODE <- function(t,X,P){with(as.list(c(P,X)),{
  dR = -R/tR
  dL = P_RL*R/tR + P_OL*O/tO - (1-P_LL)*L/tL
  dO = P_RO*R/tR + P_LO*L/tL - (1-P_OO)*O/tO
  dF = P_RF*R/tR + P_OF*O/tO + P_BF*B/tB - (1-P_FF)*FF/tF
  dB = P_RB*R/tR + P_OB*O/tO + P_FB*FF/tF - (1-P_BB)*B/tB
  dR2 = P_BR*B/tB
  dD  = P_LD*L/tL + P_OD*O/tO + P_FD*FF/tF + P_BD*B/tB + P_RD*R/tR
  list(c(dR,dL,dO,dF,dB,dR2))
})}

#' MBDETES: Feeding Cycle (R -> R) ODE Model Trajectory
#'
#' Calculate sample trajectory for feeding cycle model.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#' @param mxT maximum time of numerical solution
#' @param dt time-step at which solutions are requested
#'
#' @export
MBDETES_R2R_solve <- function(PAR, mxT=20, dt=0.001){
  inits = c(R=1,L=0,O=0,FF=0,B=0,R2=0)
  tm = seq(0,mxT,by=dt)
  data.frame(deSolve::lsoda(inits,tm,MBDETES_R2R_ODE,PAR,atol=1e-10))
}

#' MBDETES: Cohort ODE Model Derivatives
#'
#' Evaluate derivatives for cohort model.
#'
#' @param t time at which gradient of model is requested
#' @param X state vector of model
#' @param P output of \code{\link{MBDETES_Parameters}}
#'
#' @export
MBDETES_cohort_ODE <- function(t,X,P){with(as.list(c(P,X)),{
  dF = P_RF*R/tR + P_OF*O/tO + P_BF*B/tB - (1-P_FF)*FF/tF
  dB = P_RB*R/tR + P_OB*O/tO + P_FB*FF/tF - (1-P_BB)*B/tB
  dR = P_BR*B/tB - R/tR
  dL = P_RL*R/tR + P_OL*O/tO - (1-P_LL)*L/tL
  dO = P_RO*R/tR + P_LO*L/tL - (1-P_OO)*O/tO
  dOO = (P_OF + P_OB)*O/tO
  dRR = P_BR*B/tB
  list(c(dF,dB,dR,dL,dO,dOO,dRR))

})}

#' MBDETES: Cohort ODE Model Trajectory
#'
#' Calculate sample trajectory for cohort model.
#'
#' @param PAR output of \code{\link{MBDETES_Parameters}}
#' @param pF initial fraction of cohort in blood feeding search state
#' @param mxT maximum time of numerical solution
#' @param dt time-step at which solutions are requested
#'
#' @export
MBDETES_cohort_solve <- function(PAR, pF=1, mxT=100, dt=0.001){
  inits = c(FF=pF,B=1-pF,R=0,L=0,O=0,OO=0,RR=0)
  tm = seq(0,mxT,by=dt)
  data.frame(deSolve::lsoda(inits,tm,MBDETES_cohort_ODE,PAR,atol=1e-10))
}
