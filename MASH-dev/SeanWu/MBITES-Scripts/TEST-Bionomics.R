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
# BEGIN: 4 PANEL PLOT
###############################################################################

par(mfrow=c(2,2))

###############################################################################
# LIFESPAN
###############################################################################

plot(bionomics$MBDETES$cohort_tt, bionomics$MBDETES$cohort_alive, type = "l", xlab = "Age (days)", ylab = "Density",
     main = paste0("Survival Time \n MBDETES mean: ",round(bionomics$MBDETES$dist_surv_mean,2),", MBITES mean: ",round(bionomics$MBITES$dist_surv_mean,2)),
     lwd=2,col="steelblue")
polygon(c(0, bionomics$MBDETES$cohort_tt), c(0, bionomics$MBDETES$cohort_alive), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = bionomics$MBDETES$dist_surv_mean,lwd=2.5,lty=2,col="steelblue")

lines(bionomics$MBDETES$cohort_tt,bionomics$MBITES$dist_surv_s,col="firebrick3")
polygon(c(0, bionomics$MBDETES$cohort_tt), c(0, bionomics$MBITES$dist_surv_s), border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
abline(v = bionomics$MBITES$dist_surv_mean,lwd=2.5,lty=2,col="firebrick3")


###############################################################################
# EGG LAYING RATE
###############################################################################
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

###############################################################################
# BLOOD FEEDING RATE
###############################################################################
blood_breaks <- 100

# mbites
blood_mbites_hist <- hist(bionomics$MBITES$dist_blood,breaks = blood_breaks,plot = FALSE)

# mbdetes
blood_mbdetes_pdf <- diff(bionomics$MBDETES$cohort_bloodmeals)/max(blood_mbites_hist$density)
blood_mbdetes_tt <- bionomics$MBDETES$dist_blood_tt
blood_mbdetes_mean <- weighted.mean(blood_mbdetes_tt,blood_mbdetes_pdf)

# plotting maximum
blood_max <- formatC(max(blood_mbdetes_pdf,blood_mbites_hist$density), format = "e")
blood_max_ceiling <- ceiling(as.numeric(strsplit(blood_max,"e")[[1]][1]))
blood_max <- as.numeric(paste0(blood_max_ceiling,"e",strsplit(blood_max,"e")[[1]][2]))

# plot
plot(blood_mbdetes_tt, blood_mbdetes_pdf,type = "l", xlab = "Age (days)", ylab = "Density",
     main=paste0("Blood Feeding Rate \n MBDETES mean: ",round(blood_mbdetes_mean,2),", MBITES mean: ",round(bionomics$MBITES$dist_blood_mean,2)),
     lwd=2,col="steelblue")
polygon(c(0, blood_mbdetes_tt), c(0, blood_mbdetes_pdf), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = blood_mbdetes_pdf,lwd=2.5,lty=2,col="steelblue")

hist(bionomics$MBITES$dist_blood,probability = T,breaks = blood_breaks,col = adjustcolor("firebrick3",alpha.f = 0.5),add=TRUE)
abline(v = bionomics$MBITES$dist_blood_mean,lwd=2.5,lty=2,col="firebrick3")

###############################################################################
# BLOOD FEEDING INTERVAL
###############################################################################
bloodInt_breaks <- 100

# mbites
bloodInt_mbites_hist <- hist(bionomics$MBITES$dist_bloodInt$rest_intervals,breaks = bloodInt_breaks,plot = FALSE)
bloodInt_mbites_hist$counts <- bloodInt_mbites_hist$counts/sum(bloodInt_mbites_hist$counts)
bloodInt_mbites_hist$counts <- bloodInt_mbites_hist$counts*1e-2

plot(bionomics$MBDETES$R2R_tt_pdf[bionomics$MBDETES$R2R_ix], bionomics$MBDETES$R2R_pdf[bionomics$MBDETES$R2R_ix],
     type = "l", xlab = "Time (Days)", ylab = "Density",
     main=paste0("Feeding Cycle Duration \n MBDETES mean: ",round(bionomics$MBDETES$R2R_mean,2),", MBITES mean: ",round(bionomics$MBITES$dist_bloodInt_mean,2)),
     lwd=2,col="steelblue")
polygon(c(0, bionomics$MBDETES$R2R_tt_pdf[bionomics$MBDETES$R2R_ix]),
        c(0, bionomics$MBDETES$R2R_pdf[bionomics$MBDETES$R2R_ix]), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = bionomics$MBDETES$R2R_mean,lwd=2.5,lty=2,col="steelblue")

plot(bloodInt_mbites_hist,add=T,col = adjustcolor("firebrick3",alpha.f = 0.5))
abline(v = bionomics$MBITES$dist_bloodInt_mean,lwd=2.5,lty=2,col="firebrick3")


par(mfrow=c(1,1))

###############################################################################
# END: 4 PANEL PLOT
###############################################################################
