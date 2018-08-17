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
# load data
###############################################################################

library(jsonlite)

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/trivial/"
# run 1 was where mosy who died on first bout get lifespan 0, run 2 was where we sample their tte based on that first bout
run <- "2"
output_dir <- paste0(directory,"run",run)

mosquitos_df <- fromJSON(paste0(output_dir,"/mosquito_F_",run,".json"), flatten = TRUE)
mosquitos_df <- mosquitos_df[-which(sapply(mosquitos_df$id,is.null)),]
humans_df <- fromJSON(paste0(output_dir,"/human_",run,".json"), flatten = TRUE)
humans_df <- humans_df[-which(sapply(humans_df$id,is.null)),]


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

par(mfrow=c(1,2))

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

par(mfrow=c(1,1))

# figure out discrepancy
1-with(MBDETES$PAR, P_FF + P_FB)
sum(fequal(lf$lifespan,0))/length(lf$lifespan)
