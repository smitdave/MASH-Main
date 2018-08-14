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

# PAR <- MBDETES_Parameters()

PAR <- MBDETES_StateTransitions_Interp(BFAB_PAR = BFAB_PAR(),
                                        ELAB_PAR = ELAB_PAR(),
                                        BFSB_PAR = BFSB_PAR(),
                                        ELSB_PAR = ELSB_PAR())

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
MBITES_PARS <- list()

# B and R interpolating params to MBITES params
PAR_map$B <- BFAB_PAR()

MBITES_PARS$B_succeed <- PAR_map$B$A

# site 1 has both resources
landscape[[1]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)
landscape[[1]]$aqua[[1]] = list(w=1,lambda=1)

# site 2 has only blood feeding resource
landscape[[2]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)

# site 3 has only aquatic habitat resource
landscape[[3]]$aqua[[1]] = list(w=1,lambda=1)

MBITES_PARS$surviveH <- 1 - PAR_map$B$C1
MBITES_PARS$probeH <- PAR_map$B$C2 / MBITES_PARS$surviveH

MBITES_PARS$surviveZ <- 1 - PAR_map$B$C4
MBITES_PARS$feedZ <- PAR_map$B$C5 / MBITES_PARS$surviveZ

MBITES_PARS$surviveprobeH <- 1 - PAR_map$B$D1
MBITES_PARS$feedH <- PAR_map$B$D2 / MBITES_PARS$surviveprobeH

Epars <- MBDETES_PrPPRFlight_optim(E = PAR_map$B$E)
MBITES_PARS$PPR_a <- Epars$par[1]
MBITES_PARS$PPR_b <- Epars$par[2]

MBITES_PARS$B_surv <-PAR_map$B$F1 #holds for both F1 and F2

Gpars <- MBDETES_PrRefeed_optim(G = PAR_map$B$G)
MBITES_PARS$rf_a <- Gpars$par[1]
MBITES_PARS$rf_b <- Gpars$par[2]

# H1,H2,H3
# H1 and H2 are the same right now.
# H3 is not.
MBITES_PARS$boutFail_p <- 1/8
MBITES_PARS$disperse <- 0.2

# O interpolating params to MBITES params
PAR_map$O <- ELAB_PAR()

MBITES_PARS$O_succeed <- PAR_map$O$A

MBITES_PARS$O_surv <- PAR_map$O$D1

# F interpolating params to MBITES params
PAR_map$F <- BFSB_PAR()

MBITES_PARS$Bs_succeed <-PAR_map$F$A
MBITES_PARS$Bs_surv <- PAR_map$F$D1

# L interpolating params to MBITES params
PAR_map$L <- ELSB_PAR()

MBITES_PARS$Os_succeed <- PAR_map$L$A
MBITES_PARS$Os_surv <- PAR_map$L$D1


###############################################################################
# Make human initialization object
###############################################################################

nHumans = 2

humans = data.frame(
  tileID = rep(1,nHumans),
  # make sure the humans are at the sites with blood feeding resources
  siteID = 1:2,
  feedingID = rep(1,nHumans),
  w = rep(0.9,2)
)


###############################################################################
# Make mosquito initialization object
###############################################################################

nMosquitos = 50

mosquitos = data.frame(
  tileID = rep(1,nMosquitos),
  # make sure the mosquitos emerge from aquatic habitats
  siteID =rep(c(1,3),length.out=nMosquitos),
  female = rep(T,nMosquitos)
)


###############################################################################
# Run MBITES
###############################################################################

directory <- "/Users/slwu89/Desktop/mbites/trivial/"

# initialize methods
MBITES_Setup_MBDETES()

PATHOGEN_Setup(pathogen_model = "null")

# we want detailed output of blood hosts from the mosquito
trackBloodHost()
trackOviposition()

# set parameters
MBITES:::Parameters$set_parameters(disperse = MBITES_PARS$disperse,boutFail_p = MBITES_PARS$boutFail_p,
                                   Bs_surv = MBITES_PARS$Bs_surv,Os_surv = MBITES_PARS$Os_surv,
                                   B_surv = MBITES_PARS$B_surv,O_surv = MBITES_PARS$O_surv,
                                   Bs_succeed = MBITES_PARS$Bs_succeed,Os_succeed = MBITES_PARS$Os_succeed,
                                   B_succeed = MBITES_PARS$B_succeed,O_succeed = MBITES_PARS$O_succeed,
                                   surviveH = MBITES_PARS$surviveH,probeH = MBITES_PARS$probeH,
                                   surviveZ = MBITES_PARS$surviveZ,feedZ = MBITES_PARS$feedZ,
                                   surviveprobeH = MBITES_PARS$surviveprobeH,feedH = MBITES_PARS$feedH,
                                   PPR_a = MBITES_PARS$PPR_a,PPR_b = MBITES_PARS$PPR_b,
                                   rf_a = MBITES_PARS$rf_a,rf_b = MBITES_PARS$rf_b,
                                   S_u = 0)

# initialize a tile
Tile_Initialize(landscape)
Human_NULL_Initialize(humans)
MBITES_Initialize(mosquitos)

# run simulation
set_output(directory = directory,runID = 1)

simulation(tMax = 365,pretty = TRUE)
hardreset()


###############################################################################
# Analysis
###############################################################################

library(jsonlite)
library(ggplot2)

# where the files can be found
output_dir <- paste0(directory,"run1")

mosquitos_df <- fromJSON(paste0(output_dir,"/mosquito_F_1.json"), flatten = TRUE)
mosquitos_df <- mosquitos_df[-which(sapply(mosquitos_df$id,is.null)),]
humans_df <- fromJSON(paste0(output_dir,"/human_1.json"), flatten = TRUE)
humans_df <- humans_df[-which(sapply(humans_df$id,is.null)),]


###############################################################################
# MBDETES approximation
###############################################################################

MBDETES <- new.env()

MBDETES$PAR <- PAR

# R2R model (feeding cycle)
MBDETES$R2R <- MBDETES_R2R_solve(MBDETES$PAR)

MBDETES$R2 <- MBDETES$R2R[,7]
MBDETES$tt <- MBDETES$R2R[,1]

MBDETES$R2R_pdf <- diff(MBDETES$R2)/max(MBDETES$R2)
MBDETES$tt_pdf <- (MBDETES$tt[-1]+MBDETES$tt[-length(MBDETES$tt)])/2
MBDETES$R2R_mean <- weighted.mean(MBDETES$tt_pdf,MBDETES$R2R_pdf)
MBDETES$ix <- which(MBDETES$tt_pdf<5)

# cohort model
MBDETES$cohort <- MBDETES_cohort_solve(MBDETES$PAR,pF=1,dt=0.01)
MBDETES$tt <- MBDETES$cohort[,1]
MBDETES$alive <- rowSums(MBDETES$cohort[,2:6])
MBDETES$eggs <- MBDETES$cohort[,7]
MBDETES$bloodmeals <- MBDETES$cohort[,8]


###############################################################################
# MBDETES vs. MBITES comparison
###############################################################################

# egg laying and blood feeding rates plots
par(mfrow=c(2,2))

# MBDETES egg laying rate
egg_mean <- weighted.mean(MBDETES$tt[-1],diff(MBDETES$eggs))
plot(MBDETES$tt[-1], diff(MBDETES$eggs),type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("MBDETES Egg Laying Rate (mean: ",round(egg_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, MBDETES$tt[-1]), c(0, diff(MBDETES$eggs)), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = egg_mean,lwd=2.5,lty=2,col="firebrick3")

# MBITES egg laying rate
egg_mbites <- Bionomics_ovipositionRate(mosquitos_df)

eggRate <- density(egg_mbites$ages)
# egg_mean <- weighted.mean(eggRate$x,eggRate$y)
egg_mean <- mean(egg_mbites$ages)
hist(egg_mbites$ages,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),
     xlab = "Age (days)", ylab = "Density", main = paste0("MBITES Egg Laying Rate Rate (mean: ",round(mean(egg_mbites$ages),3),")"))
abline(v = egg_mean,lwd=2.5,lty=2,col="firebrick3")



# MDETES blood feeding rate
blood_mean <- weighted.mean(MBDETES$tt[-1],diff(MBDETES$bloodmeals))
plot(MBDETES$tt[-1], diff(MBDETES$bloodmeals),type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("MBDETES Blood Feeding Rate (mean: ",round(blood_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, MBDETES$tt[-1]), c(0, diff(MBDETES$bloodmeals)), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = blood_mean,lwd=2.5,lty=2,col="firebrick3")

# MBITES blood feeding rate
blood_mbites <- Bionomics_bloodfeedingRate(mosquitos_df)

bloodRate <- density(blood_mbites,from=0)

hist(blood_mbites,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),
     xlab = "Age (days)", ylab = "Density", main = paste0("MBITES Blood Feeding Rate (mean: ",round(mean(blood_mbites),3),")"))
abline(v = blood_mean,lwd=2.5,lty=2,col="firebrick3")

par(mfrow=c(1,1))

# lifespan and feeding cycle interval plots
par(mfrow=c(2,2))

# MBDETES lifespan (survivor function)
alive_mean <- weighted.mean(MBDETES$tt,MBDETES$alive)
plot(MBDETES$tt, MBDETES$alive, type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("MBDETES Survival Time (mean: ",round(alive_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, MBDETES$tt), c(0, MBDETES$alive), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = alive_mean,lwd=2.5,lty=2,col="firebrick3")

# MBITES lifespan
lf <- Bionomics_lifespan(mosquitos_df)
mean_lf <- mean(lf$lifespan)

hist(lf$lifespan,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),
     xlab = "Age (days)", ylab = "Density", main = paste0("MBITES Survival Time (mean: ",round(mean_lf,3),")"))
abline(v = mean_lf,lwd=2.5,lty=2,col="firebrick3")

# MBDETES blood feeding interval
plot(MBDETES$tt_pdf[MBDETES$ix], MBDETES$R2R_pdf[MBDETES$ix], type = "l", xlab = "Time (Days)", ylab = "Density",main=paste0("MBDETES Feeding Cycle Duration (mean: ",round(MBDETES$R2R_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, MBDETES$tt_pdf[MBDETES$ix]), c(0, MBDETES$R2R_pdf[MBDETES$ix]), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = MBDETES$R2R_mean,lwd=2.5,lty=2,col="firebrick3")

# MBITES blood feeding interval
bmi <- Bionomics_bloodIntervals(mosquitos_df,who = "all")
mean_bmi <- mean(bmi$rest_intervals,na.rm = T)

hist(bmi$rest_intervals,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),
     xlab = "Time (days)", ylab = "Density", main = paste0("MBITES Feeding Cycle Duration (mean: ",round(mean_bmi,3),")"))
abline(v = mean_bmi,lwd=2.5,lty=2,col="firebrick3")


par(mfrow=c(1,1))
