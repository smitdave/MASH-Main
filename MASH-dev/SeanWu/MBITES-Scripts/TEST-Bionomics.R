###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Test Bionomics
#     MBITES Team
#     August 2018
#
###############################################################################

rm(list = ls());gc()
library(MBITES)


###############################################################################
# load data & calculate bionomics
###############################################################################

library(jsonlite)

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/trivial/"
# run 1 was where mosy who died on first bout get lifespan 0, run 2 was where we sample their tte based on that first bout
run <- "3"
output_dir <- paste0(directory,"run",run)

mosquitos_df <- fromJSON(paste0(output_dir,"/mosquito_F_",run,".json"), flatten = TRUE)
mosquitos_df <- mosquitos_df[-which(sapply(mosquitos_df$id,is.null)),]

bionomics <- Bionomics_MBDETES_Approx(mosquitos_df)


###############################################################################
# 4 panel plot
###############################################################################

par(mfrow=c(2,2))

# EGG LAYING RATE
egg_breaks <- 50

# mbites
egg_mbites_hist <- hist(bionomics$MBITES$dist_eggs$ages,breaks = egg_breaks,plot = FALSE)

# mbdetes
egg_mbdetes_pdf <- diff(bionomics$MBDETES$cohort_eggs)/max(egg_mbites_hist$density)
egg_mbdetes_tt <- bionomics$MBDETES$dist_eggs_tt
egg_mbdetes_mean <- weighted.mean(egg_mbdetes_tt,egg_mbdetes_pdf)

# plotting maximum
egg_max <- formatC(max(egg_mbdetes_pdf,egg_mbites_hist$density), format = "e")
egg_max_ceiling <- ceiling(as.numeric(strsplit(egg_max,"e")[[1]][1]))
egg_max <- as.numeric(paste0(egg_max_ceiling,"e",strsplit(egg_max,"e")[[1]][2]))

# plot
plot(egg_mbdetes_tt , egg_mbdetes_pdf, type = "l", xlab = "Time (Days)", ylab = "Density",
     main=paste0("Egg Laying Rate \n MBDETES mean: ",round(egg_mbdetes_mean,2),", MBITES mean: ",round(bionomics$MBITES$dist_eggs_mean,2)),
     lwd=2,col="steelblue",ylim=c(0,egg_max))
polygon(c(0, egg_mbdetes_tt), c(0, egg_mbdetes_pdf), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = egg_mbdetes_mean,lwd=2.5,lty=2,col="steelblue")

hist(bionomics$MBITES$dist_eggs$ages,probability = TRUE,breaks = egg_breaks,col = adjustcolor("firebrick3",alpha.f = 0.5),add=TRUE)
abline(v = bionomics$MBITES$dist_eggs_mean,lwd=2.5,lty=2,col="firebrick3")

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



















# humans_df <- fromJSON(paste0(output_dir,"/human_",run,".json"), flatten = TRUE)
# humans_df <- humans_df[-which(sapply(humans_df$id,is.null)),]


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


# cohort model
MBDETES$cohort <- MBDETES_cohort_solve(MBDETES$PAR,pF=1,dt=0.01)
MBDETES$tt <- MBDETES$cohort[,"time"]
MBDETES$alive <- rowSums(MBDETES$cohort[,2:6])

# MBITES lifespan
lf <- Bionomics_lifespan(mosquitos_df)
mean_lf <- mean(lf$lifespan)

lf_cdf <- ecdf(lf$lifespan)
lf_s <- 1 - lf_cdf(MBDETES$tt)
lf_mean <- weighted.mean(MBDETES$tt,lf_s)

# par(mfrow=c(1,2))

# MBDETES lifespan (survivor function)
alive_mean <- weighted.mean(MBDETES$tt,MBDETES$alive)
plot(MBDETES$tt, MBDETES$alive, type = "l", xlab = "Age (days)", ylab = "Density",
     main = paste0("Survival Time \n MBDETES mean: ",round(alive_mean,2),", MBITES mean: ",round(lf_mean,2)),lwd=2,col="steelblue")
polygon(c(0, MBDETES$tt), c(0, MBDETES$alive), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = alive_mean,lwd=2.5,lty=2,col="steelblue")



lines(MBDETES$tt,lf_s,col="firebrick3")
polygon(c(0, MBDETES$tt), c(0, lf_s), border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
abline(v = lf_mean,lwd=2.5,lty=2,col="firebrick3")

# hist(lf$lifespan,probability = T,breaks = 100,col = adjustcolor("firebrick3",alpha.f = 0.5),
#      xlab = "Age (days)", ylab = "Density", main = paste0("MBITES Survival Time (mean: ",round(mean_lf,2),")"),
#      xlim = c(0,ceiling(max(MBDETES$tt))))
# abline(v = mean_lf,lwd=2.5,lty=2,col="firebrick3")

# par(mfrow=c(1,1))

# figure out discrepancy
1-with(MBDETES$PAR, P_FF + P_FB)
sum(fequal(lf$lifespan,0))/length(lf$lifespan)
