# ###############################################################################
# #         __  _______  ____  _______________________
# #        /  |/  / __ )/ __ \/ ____/_  __/ ____/ ___/
# #       / /|_/ / __  / / / / __/   / / / __/  \__ \
# #      / /  / / /_/ / /_/ / /___  / / / /___ ___/ /
# #     /_/  /_/_____/_____/_____/ /_/ /_____//____/
# #
# #     MBDETES - Model (differential equations) functions
# #     MBITES Team
# #     August 2018
# #
# ###############################################################################
#
#
# ###############################################################################
# #
# ###############################################################################
#
# #' MBDETES: Model Parameters
# #'
# #' Return a named list of parameters for MBDETES.
# #'
# #'
# #' @param tF sojourn time in blood feeding search bout
# #' @param tB sojourn time in blood feeding attempt bout
# #' @param tR sojourn time in post-prandial resting bout
# #' @param tL sojourn time in egg laying search bout
# #' @param tO sojourn time in egg laying attempt bout
# #' @param P_FF blood feeding search to search transition probability
# #' @param P_FB blood feeding search to attempt transition probability
# #' @param P_BF blood feeding attempt to search transition probability
# #' @param P_BB blood feeding attempt to attempt transition probability
# #' @param P_BR blood feeding attempt to post-prandial resting transition probability
# #' @param P_LL
# #' @param P_LO
# #' @param P_OL
# #' @param P_OO
# #' @param P_OB
# #' @param P_OF
# #' @param P_RF post-prandial resting to blood feeding search transition probability
# #' @param P_RB post-prandial resting to blood feeding attempt transition probability
# #' @param P_RL post-prandial resting to egg laying search transition probability
# #' @param P_RO post-prandial resting to egg laying attempt transition probability
# #'
# #' @export
# MBDETES_Parameters <- function(
#   # Waiting Times
#   tF = 6/24, # 30 minutes
#   tB = 3/24,   # 3 hours
#   tR = 18/24,  # 18 hours
#   tL = 6/24, # 30 minutes
#   tO = 3/24,   # 1 hour
#
#   # Transitions out of F
#   P_FF = .15,
#   P_FB = .83,
#
#   # Transitions out of B
#   P_BF = .15,
#   P_BB = .05,
#   P_BR = .75,
#
#   #Transitions out of L
#   P_LL = .15,
#   P_LO = .83,
#
#   #Transitions out of O
#   P_OL = .15,
#   P_OO = .05,
#   P_OB = .3,
#   P_OF = .45,
#
#   #Transitions out of R
#   P_RF = .05,
#   P_RB = .2,
#   P_RL = .4,
#   P_RO = .35
# ){
#   return(list(
#     tF = tF, tB = tB, tR = tR, tL = tL, tO = tO,P_FF = P_FF, P_FB = P_FB,
#     P_BF = P_BF, P_BB = P_BB, P_BR = P_BR, P_LL = P_LL, P_LO = P_LO,
#     P_OL = P_OL, P_OO = P_OO, P_OB = P_OB, P_OF = P_OF,
#     P_RF = P_RF, P_RB = P_RB, P_RL = P_RL, P_RO = P_RO,
#   ))
# }
#
#
# PARD = function(PAR){with(PAR,{
#   PAR$P_FD = 1-P_FB-P_FF
#   PAR$P_BD = 1-P_BF-P_BB-P_BR
#   PAR$P_RD = 1-P_RF-P_RB-P_RL-P_RO
#   PAR$P_LD = 1-P_LL-P_OO
#   PAR$P_OD = 1-P_OF-P_OB-P_OL-P_OO
#   PAR
# })}
#
# PAR = PARD(PAR)
#
#
# Psi_FB = function(PAR){with(PAR,{P_FB/(1-P_FF)})}
#
# T_FB = function(PAR){with(PAR,{tF/(1-P_FF)})}
#
# Psi_BR = function(PAR){with(PAR,{
#   P_BR/(1-P_BB-P_BF*Psi_FB(PAR))
# })}
#
# T_BR = function(PAR){with(PAR,{
#   (tB+P_BF*Psi_FB(PAR)*T_FB(PAR))/(1-P_BB-P_BF*Psi_FB(PAR))
# })}
#
# T_FR = function(PAR){with(PAR,{
#   T_FB(PAR) + T_BR(PAR)
# })}
#
# Psi_LO = function(PAR){with(PAR,{
#   P_LO/(1-P_LL)
# })}
#
# T_LO = function(PAR){with(PAR,{
#   tL/(1-P_LL)
# })}
#
# Psi_OF = function(PAR){with(PAR,{
#   P_OF/(1-P_OO-P_OL*Psi_LO(PAR))
# })}
#
# Psi_OB = function(PAR){with(PAR,{
#   P_OB/(1-P_OO-P_OL*Psi_LO(PAR))
# })}
#
# T_OFB = function(PAR){with(PAR,{
#   (tO + P_OL*Psi_LO(PAR)*T_LO(PAR))/(1-P_OO-P_OL*Psi_LO(PAR))
# })}
#
# Psi_FR = function(PAR){with(PAR,{
#   Psi_FB(PAR)*Psi_BR(PAR)
# })}
#
# Psi_OR = function(PAR){with(PAR,{
#   Psi_OF(PAR)*Psi_FR(PAR)+Psi_OB(PAR)*Psi_BR(PAR)
# })}
#
# Psi_LR = function(PAR){with(PAR,{
#   Psi_LO(PAR)*Psi_OR(PAR)
# })}
#
# Psi_RR = function(PAR){with(PAR,{
#   P_RF*Psi_FR(PAR)+
#   P_RB*Psi_BR(PAR)+
#   P_RL*Psi_LR(PAR)+
#   P_RO*Psi_OR(PAR)
# })}
#
# T_OR = function(PAR){with(PAR,{
#   T_OFB(PAR) + P_OB/(P_OB+P_OF)*T_BR(PAR)+ P_OF/(P_OB+P_OF)*T_FR(PAR)
# })}
#
# T_LR = function(PAR){with(PAR,{
#   T_LO(PAR) + T_OR(PAR)
# })}
#
# T_RR = function(PAR){with(PAR,{
#   (tR +
#   P_RF*T_FR(PAR)+
#   P_RB*T_BR(PAR)+
#   P_RL*T_LR(PAR)+
#   P_RO*T_OR(PAR))
# })}
#
# require(deSolve)
# MBDETES_R2R_ODE = function(t,X,P){with(as.list(c(P,X)),{
#   dR = -R/tR
#   dL = P_RL*R/tR + P_OL*O/tO - (1-P_LL)*L/tL
#   dO = P_RO*R/tR + P_LO*L/tL - (1-P_OO)*O/tO
#   dF = P_RF*R/tR + P_OF*O/tO + P_BF*B/tB - (1-P_FF)*FF/tF
#   dB = P_RB*R/tR + P_OB*O/tO + P_FB*FF/tF - (1-P_BB)*B/tB
#   dR2 = P_BR*B/tB
#   dD  = P_LD*L/tL + P_OD*O/tO + P_FD*FF/tF + P_FD*B/tB + P_RD*R/tR
#   list(c(dR,dL,dO,dF,dB,dR2))
# })}
#
# MBDETES_R2R_solve = function(PAR, mxT=20, dt=0.001){
#   inits = c(R=1,L=0,O=0,FF=0,B=0,R2=0)
#   tm = seq(0,mxT,by=dt)
#   data.frame(lsoda(inits,tm,MBDETES_R2R_ODE,PAR,atol=1e-10))
# }
# R2R=MBDETES_R2R_solve(PAR)
#
# R2=R2R[,7]
# tt=R2R[,1]
# highSum=sum(diff(R2)*tt[-1])/max(R2)
# lowSum=sum(diff(R2)*tt[-length(tt)])/max(R2)
# (lowSum+highSum)/2
#
# R2R_pdf = diff(R2)/max(R2)
# tt_pdf = (tt[-1]+tt[-length(tt)])/2
# ix = which(tt_pdf<5)
# plot(tt_pdf[ix], R2R_pdf[ix], type = "l", xlab = "Time (Days)", ylab = "Feeding Cycle Duration")
#
# MBDETES_cohort_ODE = function(t,X,P){with(as.list(c(P,X)),{
#   dF = P_RF*R/tR + P_OF*O/tO + P_BF*B/tB - (1-P_FF)*FF/tF
#   dB = P_RB*R/tR + P_OB*O/tO + P_FB*FF/tF - (1-P_BB)*B/tB
#   dR = P_BR*B/tB-R/tR
#   dL = P_RL*R/tR + P_OL*O/tO - (1-P_LL)*L/tL
#   dO = P_RO*R/tR + P_LO*L/tL - (1-P_OO)*O/tO
#   dOO = (P_OF + P_OB)*O/tO
#   dRR = P_BR*B/tB
#   list(c(dF,dB,dR,dL,dO,dOO,dRR))
# })}
#
# MBDETES_cohort_solve = function(PAR, pF=1, mxT=100, dt=0.001){
#   inits = c(FF=pF,B=1-pF,R=0,L=0,O=0,OO=0,RR=0)
#   tm = seq(0,mxT,by=dt)
#   data.frame(lsoda(inits,tm,MBDETES_cohort_ODE,PAR,atol=1e-10))
# }
# cohort=MBDETES_cohort_solve(PAR,pF=.5,dt=0.01)
# tt=cohort[,1]
# alive = rowSums(cohort[,2:6])
# eggs = cohort[,7]
# bloodmeals = cohort[,8]
#
# highSum=-sum(diff(alive)*tt[-1])
# lowSum=-sum(diff(alive)*tt[-length(tt)])
# (lowSum+highSum)/2
#
# par(mfrow=c(2,2), mar = c(5,4,2,2))
# plot(tt, alive, type = "l", xlab = "Age (days)", ylab = "Surviving")
# #lines(tt, exp(-tt/9.78), col = "red")
# plot(tt[-1], diff(eggs), col = "blue", type = "l", xlab = "Age (days)", ylab= "Laying Rate")
# plot(tt[-1], diff(bloodmeals), col = "red", type = "l", xlab = "Age (days)", ylab = "Feeding Rate")
# plot(tt_pdf[ix], R2R_pdf[ix], type = "l", xlab = "Time (days)", ylab = "Feeding Cycle Length")
