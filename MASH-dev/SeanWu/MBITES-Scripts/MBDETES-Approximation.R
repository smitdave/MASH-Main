###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBDETES simulations
#     MBITES Team
#     August 2018
#
###############################################################################


###############################################################################
# MBDETES with MBDETES parameters
###############################################################################

rm(list=ls());gc()
library(MBITES)

PAR <- MBDETES_Parameters()

R2R=MBDETES_R2R_solve(PAR)

R2=R2R[,7]
tt=R2R[,1]
highSum=sum(diff(R2)*tt[-1])/max(R2)
lowSum=sum(diff(R2)*tt[-length(tt)])/max(R2)
(lowSum+highSum)/2

R2R_pdf = diff(R2)/max(R2)
tt_pdf = (tt[-1]+tt[-length(tt)])/2
R2R_mean <- weighted.mean(tt_pdf,R2R_pdf)
ix = which(tt_pdf<5)

cohort=MBDETES_cohort_solve(PAR,pF=.5,dt=0.01)
tt=cohort[,1]
alive = rowSums(cohort[,2:6])
eggs = cohort[,7]
bloodmeals = cohort[,8]

highSum=-sum(diff(alive)*tt[-1])
lowSum=-sum(diff(alive)*tt[-length(tt)])
(lowSum+highSum)/2

# binomics plots for cohort
par(mfrow=c(2,2), mar = c(5,4,2,2))

# survival function of cohort (lifespan plot in MBITES)
alive_mean <- weighted.mean(tt,alive)
plot(tt, alive, type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("Cohort Survival Time (mean: ",round(alive_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, tt), c(0, alive), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = alive_mean,lwd=2.5,lty=2,col="firebrick3")

# egg laying rate
egg_mean <- weighted.mean(tt[-1],diff(eggs))
plot(tt[-1], diff(eggs),type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("Egg Laying Rate (mean: ",round(egg_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, tt[-1]), c(0, diff(eggs)), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = egg_mean,lwd=2.5,lty=2,col="firebrick3")

# blood feeding rate
blood_mean <- weighted.mean(tt[-1],diff(bloodmeals))
plot(tt[-1], diff(bloodmeals),type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("Blood Feeding Rate (mean: ",round(blood_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, tt[-1]), c(0, diff(bloodmeals)), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = blood_mean,lwd=2.5,lty=2,col="firebrick3")

# length of feeding (gonotrophic) cycle (blood meal interval in MBITES)
plot(tt_pdf[ix], R2R_pdf[ix], type = "l", xlab = "Time (Days)", ylab = "Density",main=paste0("Feeding Cycle Duration (mean: ",round(R2R_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, tt_pdf[ix]), c(0, R2R_pdf[ix]), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = R2R_mean,lwd=2.5,lty=2,col="firebrick3")
par(mfrow=c(1,1))


###############################################################################
# Mapping MBITES to MBDETES parameter space
###############################################################################

# landscapes
landscape <- vector(mode = "list",length = 3)

# site characteristics
for(i in 1:3){
  landscape[[i]]$id = i
  landscape[[i]]$xy = c(1,1)
  landscape[[i]]$type = 1L
  landscape[[i]]$tileID = 1L
  landscape[[i]]$move = rep(0.5,2)
  landscape[[i]]$move_id = (1:3)[-i]
  landscape[[i]]$haz = 0
  # null resources
  landscape[[i]]$feed = NULL
  landscape[[i]]$aqua = NULL
}


PAR_map <- list()

PAR_map$B <- BFAB_PAR()


# BFAB_PAR2MBITES <- function()
B_succeed <- PAR_map$B$A

# site 1 has both resources
landscape[[1]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)
landscape[[1]]$aqua[[1]] = list(w=1,lambda=1)

# site 2 has only blood feeding resource
landscape[[2]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)

# site 3 has only aquatic habitat resource
landscape[[3]]$aqua[[1]] = list(w=1,lambda=1)

surviveH <- 1 - PAR_map$B$C1
probeH <- PAR_map$B$C2 / surviveH

surviveZ <- 1 - PAR_map$B$C4
feedZ <- PAR_map$B$C5 / surviveZ

surviveprobeH <- 1 - PAR_map$B$D1
feedH <- PAR_map$B$D2 / surviveprobeH
