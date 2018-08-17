###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Trivial Landscape
#     MBITES Team
#     June 2018
#
###############################################################################

rm(list = ls());gc()
library(MBITES)


###############################################################################
# Make landscape initialization object
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
  landscape[[i]]$haz = 0.005
  # null resources
  landscape[[i]]$feed = NULL
  landscape[[i]]$aqua = NULL
}

# site 1 has both resources
landscape[[1]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)
landscape[[1]]$aqua[[1]] = list(w=1,lambda=1)

# site 2 has only blood feeding resource
landscape[[2]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)

# site 3 has only aquatic habitat resource
landscape[[3]]$aqua[[1]] = list(w=1,lambda=1)


###############################################################################
# Make human initialization object
###############################################################################

nHumans = 2

humans = data.frame(
  tileID = rep(1,nHumans),
  # make sure the humans are at the sites with blood feeding resources
  siteID = 1:2,
  feedingID = rep(1,nHumans),
  w = rep(0.9,nHumans)
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
PPR <- MBDETES_PrPPRFlight_optim(E = BFAB_PAR()$E)
rf <- MBDETES_PrRefeed_optim(G = BFAB_PAR()$G)

# MBITES:::Parameters$set_parameters(Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.98,O_surv = 0.98,
#                                    Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
#                                    S_u = 0,disperse = 0.2,boutFail_p = 1/8,
#                                    rf_a = rf$par[1],rf_b = rf$par[2],
#                                    PPR_a = PPR$par[1],PPR_b = PPR$par[2])

# a good paramter set
MBITES:::Parameters$set_parameters(Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.99,O_surv = 0.99,
                                   Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
                                   S_u = 0,disperse = 0.2)


# initialize a tile
Tile_Initialize(landscape)
Human_NULL_Initialize(humans)
MBITES_Initialize(mosquitos)

# run simulation
set_output(directory = directory,runID = 3)

simulation(tMax = 365*5,pretty = TRUE)
hardreset()


###############################################################################
# Analysis
###############################################################################

library(jsonlite)
library(ggplot2)

# where the files can be found
output_dir <- paste0(directory,"run2")

mosquitos_df <- fromJSON(paste0(output_dir,"/mosquito_F_2.json"), flatten = TRUE)
mosquitos_df <- mosquitos_df[-which(sapply(mosquitos_df$id,is.null)),]
humans_df <- fromJSON(paste0(output_dir,"/human_2.json"), flatten = TRUE)
humans_df <- humans_df[-which(sapply(humans_df$id,is.null)),]


###############################################################################
# basic bionomics
###############################################################################

# # lifespan
# lf <- Bionomics_lifespan(mosquitos_df)
# mean_lf <- mean(lf$lifespan)
#
# ggplot() + geom_histogram(data = lf, aes(lifespan), fill = "steelblue", bins = 20) +
#   geom_vline(xintercept = mean_lf,col="firebrick3",size=1.15) +
#   ggtitle(paste0("Mosquito Lifespans (mean: ",round(mean_lf,3),")")) + xlab("Days") + ylab("Frequency") + theme_bw()
#
# # human blood hosts
# bh <- Bionomics_humanBloodHost(mosquitos_df,who = "human")
# mean_bh <- mean(bh$humanHost)
#
# ggplot() + geom_histogram(data = bh, aes(humanHost), fill = "steelblue", bins = 20) +
#   geom_vline(xintercept = mean_bh,col="firebrick3",size=1.15) +
#   ggtitle(paste0("Number of Human Blood Hosts per mosquito (mean: ",round(mean_bh,3),")")) + xlab("Number of Hosts") + ylab("Frequency") + theme_bw()
#
# # blood meal intervals
# bmi <- Bionomics_bloodIntervals(mosquitos_df,who = "human")
# mean_bmi <- mean(bmi$bmIntervals)
#
# ggplot() + geom_histogram(data = bmi, aes(bmIntervals), fill = "steelblue", bins = 20) +
#   geom_vline(xintercept = mean_bmi,col="firebrick3",size=1.15) +
#   ggtitle(paste0("Human Blood Meal Interval (mean: ",round(mean_bmi,3),")")) + xlab("Time") + ylab("Frequency") + theme_bw()
#
# # vectorial capacity (might not make a whole lot of sense to look at the histogram, just mean value)
# vc <- Bionomics_vectorialCapacity(mosquitos = mosquitos_df,humans = humans_df,EIP = 10,spatial = T)
# vc_df <- data.frame(vc=sapply(vc,function(x){x$VC}))
# mean_vc <- mean(vc_df$vc)
#
# ggplot() + geom_histogram(data = vc_df, aes(vc), fill = "steelblue", bins = 20) +
#   geom_vline(xintercept = mean_vc,col="firebrick3",size=1.15) +
#   ggtitle(paste0("Vectorial Capacity (mean: ",round(mean_vc,3),")")) + xlab("Secondary Bites") + ylab("Frequency") + theme_bw()
#
# # lifetime egg production
# egg <- Bionomics_lifetimeOviposition(mosquitos_df,TRUE)
# egg_df <- data.frame(egg=egg$lifetime)
# mean_egg <- mean(egg_df$egg)
#
# ggplot() + geom_histogram(data = egg_df, aes(egg), fill = "steelblue", bins = 20) +
#   geom_vline(xintercept = mean_egg,col="firebrick3",size=1.15) +
#   ggtitle(paste0("Lifetime Egg Production (mean: ",round(mean_egg,3),")")) + xlab("Eggs") + ylab("Frequency") + theme_bw()
#
# # oviposition intervals and successful events
# oviposit <- Bionomics_ovipositionInterval(mosquitos_df)
#
# oviposit_interval <- data.frame(interval=oviposit$interval)
# mean_oviposit_interval <- mean(oviposit_interval$interval)
#
# oviposit_num <- data.frame(number=oviposit$numOviposit)
# mean_oviposit_num <- mean(oviposit_num$number)
#
# ggplot() + geom_histogram(data = oviposit_interval, aes(interval), fill = "steelblue", bins = 20) +
#   geom_vline(xintercept = mean_oviposit_interval,col="firebrick3",size=1.15) +
#   ggtitle(paste0("Interval between Successful Oviposition (mean: ",round(mean_oviposit_interval,3),")")) + xlab("Time") + ylab("Frequency") + theme_bw()
#
# ggplot() + geom_histogram(data = oviposit_num, aes(number), fill = "steelblue", bins = 20) +
#   geom_vline(xintercept = mean_oviposit_num,col="firebrick3",size=1.15) +
#   ggtitle(paste0("Number of Successful Oviposition Events (mean: ",round(mean_oviposit_num,3),")")) + xlab("Number of Events") + ylab("Frequency") + theme_bw()


###############################################################################
# MBDETES approximation
###############################################################################

M <- Bionomics_StateTransition(mosquitos_df)

MBDETES <- new.env()

PAR <- list(
  # timing
  tF = 6/24, # 30 minutes
  tB = 3/24,   # 3 hours
  tR = 18/24,  # 18 hours
  tL = 6/24, # 30 minutes
  tO = 3/24,   # 1 hour
  # F2X
  P_FF = M["F","F"], P_FB = M["F","B"], P_FD = M["F","D"],
  # B2X
  P_BF = M["B","F"], P_BB = M["B","B"], P_BR = M["B","R"], P_BD = M["B","D"],
  # R2X
  P_RF = M["R","F"], P_RB = M["R","B"], P_RL = M["R","L"], P_RO = M["R","O"], P_RD = M["R","D"],
  # L2X
  P_LL = M["L","L"], P_LO = M["L","O"], P_LD = M["L","D"],
  # O2X
  P_OL = M["O","L"], P_OO = M["O","O"], P_OB = M["O","B"], P_OF = M["O","F"], P_OD = M["O","D"]
)

# MBDETES$PAR <- MBDETES_Parameters()
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
MBDETES$tt <- MBDETES$cohort[,"time"]
MBDETES$alive <- rowSums(MBDETES$cohort[,2:6])
MBDETES$eggs <- MBDETES$cohort[,"OO"]
MBDETES$bloodmeals <- MBDETES$cohort[,"RR"]

# calculate distributions

# egg laying rate
MBDETES$eggrate_pdf <- diff(MBDETES$eggs)/max(MBDETES$eggs)
MBDETES$eggrate_tt <- (MBDETES$tt[-1]+MBDETES$tt[-length(MBDETES$tt)])/2
MBDETES$eggrate_mean <- weighted.mean(MBDETES$eggrate_tt,MBDETES$eggrate_pdf)

# blood feeding rate
MBDETES$bloodrate_pdf <- diff(MBDETES$bloodmeals)/max(MBDETES$bloodmeals)
MBDETES$bloodrate_tt <- (MBDETES$tt[-1]+MBDETES$tt[-length(MBDETES$tt)])/2
MBDETES$bloodrate_mean <- weighted.mean(MBDETES$bloodrate_tt,MBDETES$bloodrate_pdf)


###############################################################################
# MBDETES vs. MBITES comparison
###############################################################################

# egg laying and blood feeding rates plots
par(mfrow=c(4,2))

# MBDETES egg laying rate
plot(MBDETES$eggrate_tt, MBDETES$eggrate_pdf, type = "l", xlab = "Time (Days)", ylab = "Density",main=paste0("MBDETES Egg Laying Rate (mean: ",round(MBDETES$eggrate_mean,2),")"),lwd=2,col="steelblue")
polygon(c(0, MBDETES$eggrate_tt), c(0, MBDETES$eggrate_pdf), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = MBDETES$eggrate_mean,lwd=2.5,lty=2,col="firebrick3")

# MBITES egg laying rate
eggrate_mbites <- Bionomics_ovipositionRate(mosquitos_df)
egg_mean <- mean(eggrate_mbites$ages)
hist(eggrate_mbites$ages,probability = TRUE,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),
     xlab = "Age (days)", ylab = "Density", main = paste0("MBITES Egg Laying Rate (mean: ",round(mean(eggrate_mbites$ages),2),")"),
     xlim = c(0,ceiling(max(MBDETES$eggrate_tt))))
abline(v = egg_mean,lwd=2.5,lty=2,col="firebrick3")

# MDETES blood feeding rate
plot(MBDETES$bloodrate_tt, MBDETES$bloodrate_pdf,type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("MBDETES Blood Feeding Rate (mean: ",round(MBDETES$bloodrate_mean,2),")"),lwd=2,col="steelblue")
polygon(c(0, MBDETES$bloodrate_tt), c(0, MBDETES$bloodrate_pdf), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = MBDETES$bloodrate_mean,lwd=2.5,lty=2,col="firebrick3")

# MBITES blood feeding rate
blood_mbites <- Bionomics_bloodfeedingRate(mosquitos_df)
# bloodRate <- density(blood_mbites,from=0)
blood_mean <- mean(blood_mbites)
hist(blood_mbites,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),
     xlab = "Age (days)", ylab = "Density", main = paste0("MBITES Blood Feeding Rate (mean: ",round(mean(blood_mbites),2),")"),
     xlim = c(0,ceiling(max(MBDETES$bloodrate_tt))))
abline(v = blood_mean,lwd=2.5,lty=2,col="firebrick3")

# MBDETES lifespan (survivor function)
alive_mean <- weighted.mean(MBDETES$tt,MBDETES$alive)
plot(MBDETES$tt, MBDETES$alive, type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("MBDETES Survival Time (mean: ",round(alive_mean,2),")"),lwd=2,col="steelblue")
polygon(c(0, MBDETES$tt), c(0, MBDETES$alive), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = alive_mean,lwd=2.5,lty=2,col="firebrick3")

# MBITES lifespan
lf <- Bionomics_lifespan(mosquitos_df)
mean_lf <- mean(lf$lifespan)

hist(lf$lifespan,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),
     xlab = "Age (days)", ylab = "Density", main = paste0("MBITES Survival Time (mean: ",round(mean_lf,2),")"),
     xlim = c(0,ceiling(max(MBDETES$tt))))
abline(v = mean_lf,lwd=2.5,lty=2,col="firebrick3")

# MBDETES blood feeding interval
plot(MBDETES$tt_pdf[MBDETES$ix], MBDETES$R2R_pdf[MBDETES$ix], type = "l", xlab = "Time (Days)", ylab = "Density",main=paste0("MBDETES Feeding Cycle Duration (mean: ",round(MBDETES$R2R_mean,2),")"),lwd=2,col="steelblue")
polygon(c(0, MBDETES$tt_pdf[MBDETES$ix]), c(0, MBDETES$R2R_pdf[MBDETES$ix]), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = MBDETES$R2R_mean,lwd=2.5,lty=2,col="firebrick3")

# MBITES blood feeding interval
bmi <- Bionomics_bloodIntervals(mosquitos_df,who = "all")
mean_bmi <- mean(bmi$rest_intervals)

hist(bmi$rest_intervals,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),
     xlab = "Time (days)", ylab = "Density", main = paste0("MBITES Feeding Cycle Duration (mean: ",round(mean_bmi,2),")"),
     xlim = c(0,ceiling(max(MBDETES$tt_pdf[MBDETES$ix]))))
abline(v = mean_bmi,lwd=2.5,lty=2,col="firebrick3")

par(mfrow=c(1,1))


###############################################################################
# 4 panel plot
###############################################################################

par(mfrow=c(2,2))

# EGG LAYING RATE

# mbites
egg_mbites_hist <- hist(eggrate_mbites$ages,breaks = 100,plot = FALSE)
eggrate_mbites <- Bionomics_ovipositionRate(mosquitos_df)
egg_mean <- mean(eggrate_mbites$ages)

# mbdetes
MBDETES$eggrate_pdf <- diff(MBDETES$eggs)/max(egg_mbites_hist$density)
MBDETES$eggrate_tt <- (MBDETES$tt[-1]+MBDETES$tt[-length(MBDETES$tt)])/2
MBDETES$eggrate_mean <- weighted.mean(MBDETES$eggrate_tt,MBDETES$eggrate_pdf)

# plot
plot(MBDETES$eggrate_tt, MBDETES$eggrate_pdf, type = "l", xlab = "Time (Days)", ylab = "Density",
     main=paste0("Egg Laying Rate \n MBDETES mean: ",round(MBDETES$eggrate_mean,2),", MBITES mean: ",round(egg_mean,2)),
     lwd=2,col="steelblue")
polygon(c(0, MBDETES$eggrate_tt), c(0, MBDETES$eggrate_pdf), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = MBDETES$eggrate_mean,lwd=2.5,lty=2,col="steelblue")

hist(eggrate_mbites$ages,probability = TRUE,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),add=TRUE)
abline(v = egg_mean,lwd=2.5,lty=2,col="firebrick3")

# BLOOD FEEDING RATE

# mbites
blood_mbites_hist <- hist(blood_mbites,breaks = 100,plot = FALSE)
blood_mbites <- Bionomics_bloodfeedingRate(mosquitos_df)
blood_mean <- mean(blood_mbites)

# mbdetes
MBDETES$bloodrate_pdf <- diff(MBDETES$bloodmeals)/max(blood_mbites_hist$density)
MBDETES$bloodrate_tt <- (MBDETES$tt[-1]+MBDETES$tt[-length(MBDETES$tt)])/2
MBDETES$bloodrate_mean <- weighted.mean(MBDETES$bloodrate_tt,MBDETES$bloodrate_pdf)

# plot
plot(MBDETES$bloodrate_tt, MBDETES$bloodrate_pdf,type = "l", xlab = "Age (days)", ylab = "Density",
     main=paste0("Blood Feeding Rate \n MBDETES mean: ",round(MBDETES$bloodrate_mean,2),", MBITES mean: ",round(blood_mean,2)),
     lwd=2,col="steelblue")
polygon(c(0, MBDETES$bloodrate_tt), c(0, MBDETES$bloodrate_pdf), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = MBDETES$bloodrate_mean,lwd=2.5,lty=2,col="steelblue")

hist(blood_mbites,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),add=TRUE)
abline(v = blood_mean,lwd=2.5,lty=2,col="firebrick3")

# LIFESPAN

# mbites
lifespan <- Bionomics_lifespan(mosquitos_df)
lifespan_mbites_hist <- hist(lf$lifespan,breaks=100,plot=FALSE)
lifespan_mbites_hist$counts<-lifespan_mbites_hist$counts/max(lifespan_mbites_hist$counts)
# lifespan_mbites_hist$density <- lifespan_mbites_hist$density/max(lifespan_mbites_hist$density)
mean_lifespan <- mean(lifespan$lifespan)

# mbdetes
lifespan_mbdetes_mean <- weighted.mean(MBDETES$tt,MBDETES$alive)

# plot
plot(MBDETES$tt, MBDETES$alive, type = "l", xlab = "Age (days)", ylab = "Density",
     main=paste0("Survival Time \n MBDETES mean: ",round(lifespan_mbdetes_mean,2),", MBITES mean: ",round(mean_lifespan,2)),
     lwd=2,col="steelblue")
polygon(c(0, MBDETES$tt), c(0, MBDETES$alive), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = lifespan_mbdetes_mean,lwd=2.5,lty=2,col="steelblue")

# hist(lf$lifespan,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),add=TRUE)
plot(lifespan_mbites_hist,add=T,col = adjustcolor("firebrick3",alpha.f = 0.5))
abline(v = mean_lifespan,lwd=2.5,lty=2,col="firebrick3")

# BLOOD FEEDING INTERVAL

# mbdetes
MBDETES$tt <- MBDETES$R2R[,1]
MBDETES$R2R_pdf <- diff(MBDETES$R2)/max(MBDETES$R2)
MBDETES$R2R_pdf <- MBDETES$R2R_pdf/sum(MBDETES$R2R_pdf)
MBDETES$tt_pdf <- (MBDETES$tt[-1]+MBDETES$tt[-length(MBDETES$tt)])/2
MBDETES$R2R_mean <- weighted.mean(MBDETES$tt_pdf,MBDETES$R2R_pdf)
MBDETES$ix <- which(MBDETES$tt_pdf<5)

# mbites
bmi <- Bionomics_bloodIntervals(mosquitos_df,who = "all")
bmi_mbites_hist <- hist(bmi$rest_intervals,breaks = 100,plot=FALSE)
bmi_mbites_hist$counts <- bmi_mbites_hist$counts/sum(bmi_mbites_hist$counts)
bmi_mbites_hist$counts <- bmi_mbites_hist$counts*1e-2
mean_bmi <- mean(bmi$rest_intervals)

plot(MBDETES$tt_pdf[MBDETES$ix], MBDETES$R2R_pdf[MBDETES$ix], type = "l", xlab = "Time (Days)", ylab = "Density",
     main=paste0("Feeding Cycle Duration \n MBDETES mean: ",round(MBDETES$R2R_mean,2),", MBITES mean: ",round(mean_bmi,2)),
     lwd=2,col="steelblue")
polygon(c(0, MBDETES$tt_pdf[MBDETES$ix]), c(0, MBDETES$R2R_pdf[MBDETES$ix]), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = MBDETES$R2R_mean,lwd=2.5,lty=2,col="steelblue")

# hist(bmi$rest_intervals,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),add=T)
plot(bmi_mbites_hist,add=T,col = adjustcolor("firebrick3",alpha.f = 0.5))
abline(v = mean_bmi,lwd=2.5,lty=2,col="firebrick3")

par(mfrow=c(1,1))
