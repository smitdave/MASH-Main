###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Bionomics - Take MBITES output and run MBDETES
#     MBITES Team
#     August 2018
#
###############################################################################

#' Bionomics: Calculate MBITES Bionomics and run Approximating MBDETES Simulation
#'
#' @param mosquitos a processed data frame of mosquito output (use \code{\link[jsonlite]{fromJSON}}) to read in MBITES output
#'
#' @export
Bionomics_MBDETES_Approx <- function(mosquitos){

  cat("calculating state transitions matrix ... \n")

  MBDETES <- new.env()
  MBITES <- new.env()
  MBDETES$M <- Bionomics_StateTransition(mosquitos)

  cat("\nrunning MBDETES numerical simulation ... \n")

  MBDETES$parameters <- list(
    # timing
    tF = 6/24, # 30 minutes
    tB = 3/24,   # 3 hours
    tR = 18/24,  # 18 hours
    tL = 6/24, # 30 minutes
    tO = 3/24,   # 1 hour
    # F2X
    P_FF = MBDETES$M["F","F"], P_FB = MBDETES$M["F","B"], P_FD = MBDETES$M["F","D"],
    # B2X
    P_BF = MBDETES$M["B","F"], P_BB = MBDETES$M["B","B"], P_BR = MBDETES$M["B","R"], P_BD = MBDETES$M["B","D"],
    # R2X
    P_RF = MBDETES$M["R","F"], P_RB = MBDETES$M["R","B"], P_RL = MBDETES$M["R","L"], P_RO = MBDETES$M["R","O"], P_RD = MBDETES$M["R","D"],
    # L2X
    P_LL = MBDETES$M["L","L"], P_LO = MBDETES$M["L","O"], P_LD = MBDETES$M["L","D"],
    # O2X
    P_OL = MBDETES$M["O","L"], P_OO = MBDETES$M["O","O"], P_OB = MBDETES$M["O","B"], P_OF = MBDETES$M["O","F"], P_OD = MBDETES$M["O","D"]
  )

  # R2R model (feeding cycle)
  MBDETES$sim_R2R <- MBDETES_R2R_solve(MBDETES$parameters)

  MBDETES$R2R_R2 <- MBDETES$sim_R2R[,7]
  MBDETES$R2R_tt <- MBDETES$sim_R2R[,1]

  MBDETES$R2R_pdf <- diff(MBDETES$R2R_R2)/max(MBDETES$R2R_R2)
  MBDETES$R2R_tt_pdf <- (MBDETES$R2R_tt[-1]+MBDETES$R2R_tt[-length(MBDETES$R2R_tt)])/2
  MBDETES$R2R_mean <- weighted.mean(MBDETES$R2R_tt_pdf,MBDETES$R2R_pdf)
  MBDETES$R2R_ix <- which(MBDETES$R2R_tt_pdf<5)

  # cohort model
  MBDETES$sim_cohort <- MBDETES_cohort_solve(MBDETES$parameters,pF=1,dt=0.01)
  MBDETES$cohort_tt <- MBDETES$sim_cohort[,"time"]
  MBDETES$cohort_alive <- rowSums(MBDETES$sim_cohort[,2:6])
  MBDETES$cohort_eggs <- MBDETES$sim_cohort[,"OO"]
  MBDETES$cohort_bloodmeals <- MBDETES$sim_cohort[,"RR"]

  cat("calculating survival distributions ... \n")

  # MBDETES mean survival time
  MBDETES$dist_surv_mean <- weighted.mean(MBDETES$cohort_tt,MBDETES$cohort_alive)

  # MBITES lifespan CDF, survival function, mean survival time
  MBITES$dist_surv <- Bionomics_lifespan(mosquitos)
  MBITES$dist_surv_cdf <- ecdf(MBITES$dist_surv$lifespan)
  MBITES$dist_surv_s <- 1 - MBITES$dist_surv_cdf(MBDETES$cohort_tt)
  MBITES$dist_surv_mean <- weighted.mean(MBDETES$cohort_tt,MBITES$dist_surv_s)

  cat("calculating oviposition rate distributions ... \n")

  # MBDETES oviposition rate
  MBDETES$dist_eggs <- diff(MBDETES$cohort_eggs)/max(MBDETES$cohort_eggs)
  MBDETES$dist_eggs_tt <- (MBDETES$cohort_tt[-1]+MBDETES$cohort_tt[-length(MBDETES$cohort_tt)])/2
  MBDETES$dist_eggs_mean <- weighted.mean(MBDETES$dist_eggs_tt,MBDETES$dist_eggs)

  # MBITES oviposition rate
  MBITES$dist_eggs <- Bionomics_ovipositionRate(mosquitos)
  MBITES$dist_eggs_mean <- mean(MBITES$dist_eggs$ages)

  cat("calculating blood feeding rate distributions ... \n")

  # MBDETES blood feeding rate
  MBDETES$dist_blood <- diff(MBDETES$cohort_bloodmeals)/max(MBDETES$cohort_bloodmeals)
  MBDETES$dist_blood_tt <- (MBDETES$cohort_tt[-1]+MBDETES$cohort_tt[-length(MBDETES$cohort_tt)])/2
  MBDETES$dist_blood_mean <- weighted.mean(MBDETES$dist_blood_tt,MBDETES$dist_blood)

  # MBITES blood feeding rate
  MBITES$dist_blood <- Bionomics_bloodfeedingRate(mosquitos)
  MBITES$dist_blood_mean <- mean(MBITES$dist_blood)

  cat("calculating blood feeding interval distributions ... \n")

  # MBITES blood feeding interval
  MBITES$dist_bloodInt <- Bionomics_bloodIntervals(mosquitos,who = "all")
  MBITES$dist_bloodInt_mean <- mean(MBITES$dist_bloodInt$rest_intervals)

  cat("done!\n")

  return(list(MBDETES=MBDETES,MBITES=MBITES))
}
