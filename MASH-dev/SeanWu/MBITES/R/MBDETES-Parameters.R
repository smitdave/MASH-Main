###############################################################################
#         __  _______  ____  _______________________
#        /  |/  / __ )/ __ \/ ____/_  __/ ____/ ___/
#       / /|_/ / __  / / / / __/   / / / __/  \__ \
#      / /  / / /_/ / /_/ / /___  / / / /___ ___/ /
#     /_/  /_/_____/_____/_____/ /_/ /_____//____/
#
#     MBDETES - Parameters
#     MBITES Team
#     August 2018
#
###############################################################################


#' MBDETES: Parameters for Transitions from B
#'
#' Produce named list of parameters for \code{\link{BFAB_B2X}} and \code{\link{BFAB_R2X}}.
#' This function is useful for finding parameter sets that map between
#' MBITES and MBDETES parameter spaces.
#'
#' @param A probability of successful launch (maps to \code{B_succeed})
#' @param B0 probability of empty host queue (not possible to directly set)
#' @param B1 weight on human hosts (add to sites on a tile through \code{\link{Human_NULL_Initialize}})
#' @param B2 weight on zoo hosts (add to sites on a tile through \code{\link{Tile_Initialize}})
#' @param C1 probability of death during human host approach (maps to 1 - \code{surviveH})
#' @param C2 probability to not be deterred during probing a human host (maps to \code{surviveH}*\code{probeH})
#' @param C4 probability of death during zoo host approach (maps to 1 - \code{surviveZ})
#' @param C5 probability to successfully begin blood feeding a zoo host (maps to \code{surviveZ}*\code{feedZ})
#' @param C7 probability of death when encountering a blood trap (pending traps in MBITES ...)
#' @param D1 probability of death during probing a human host (maps to 1 - \code{surviveprobeH})
#' @param D2 probability to be successfully begin blood feeding on a human (maps to \code{surviveprobeH}*\code{feedH})
#' @param E probability to survive post-prandial resting flight after a blood meal (calculate with \code{\link{MBDETES_PrPPRFlight}}, uses parameters \code{PPR_a}, \code{PPR_b}, \code{bm_a}, \code{bm_b})
#' @param F1 probability to survive resting (\code{updateState}) given a successful blood meal (calculate with \code{MBDETES_PrSurvive(site,"B")}, uses parameters \code{B_surv}, and the local \code{\link{Site}} object's hazard)
#' @param F2 probability to survive resting (\code{updateState}) given an unsuccessful blood meal attempt (calculate with \code{MBDETES_PrSurvive(site,"B")}, uses parameters \code{B_surv}, and the local \code{\link{Site}} object's hazard)
#' @param G probability to refeed following resting (calculate with \code{\link{MBDETES_PrRefeed}}, uses parameters \code{rf_a}, \code{rf_b}, \code{bm_a}, \code{bm_b})
#' @param H1 probability to leave the current \code{\link{Site}} and go on a blood feeding search given a successful blood meal attempt (calculate with \code{1 - MBDETES_PrLeave(site,"feed",FALSE)}, uses parameters \code{boutFail_p}, \code{disperse})
#' @param H2 probability to leave the current \code{\link{Site}} and go on an oviposition search given a successful blood meal attempt (calculate with \code{1 - MBDETES_PrLeave(site,"aqua",FALSE)}, uses parameters \code{boutFail_p}, \code{disperse})
#' @param H3 probability to leave the current \code{\link{Site}} and go on a blood feeding search given an unsuccessful blood meal attempt (calculate with \code{1 - MBDETES_PrLeave(site,"feed",TRUE)}, uses parameters \code{boutFail_p}, \code{disperse})
#'
#' @export
BFAB_PAR <- function(
  A=  1,
  B0= 0.00, B1= 0.90,  B2= 0.10, #B3 = 1-B0-B1-B2
  C1= 0.01, C2= 0.85,            #C3 = 1-C1-D2
  C4= 0.01, C5= 0.85,            #C6 = 1-C4-C5
  C7= 0.50,                      #C8 = 1-C7
  D1= 0.01, D2= 0.9,            #D3 = 1-D1-D2
  E = 0.99,
  F1= 0.99, F2= 0.99,
  G = 0.20,
  H1= 0.80, H2= 0.80, H3= 0.7
){
list(A=A, B0=B0, B1=B1, B2=B2, B3=1-sum(B0,B1,B2),
           C1=C1, C2=C2, C3=1-sum(C1,C2), C4=C4,
           C5=C5, C6=1-sum(C4,C5), C7=C7, C8=1-C7,
           D1=D1, D2=D2, D3=1-sum(D1,D2), E=E,
           F1=F1, F2=F2, G=G, H1=H1, H2=H2, H3=H3)
}


#' MBDETES: Parameters for Transitions from O
#'
#' Produce named list of parameters for \code{\link{ELAB_O2X}}.
#' This function is useful for finding parameter sets that map between
#' MBITES and MBDETES parameter spaces.
#'
#' @param A probability of successful launch (maps to \code{O_succeed})
#' @param B0 probability of empty habitats (can create \code{\link{Site}} objects with no aquatic habitats through \code{\link{Tile_Initialize}})
#' @param B1 probability to approach an aquatic habitat (no traps currently in MBITES so must equal 1)
#' @param C1 probability of death during aquatic habitat approach (no current MBITES mapping ...)
#' @param C2 probability to not be deterred during aquatic habitat approach (no current MBITES mapping ...)
#' @param C4 probability of death when encountering an ovitrap (pending traps in MBITES ...)
#' @param D1 probability to survive resting (\code{\link{upstateState}}) given successful oviposition (calculate with \code{MBDETES_PrSurvive(site,"O")}, uses parameters \code{O_surv}, and the local \code{\link{Site}} object's hazard)
#' @param D2 probability to survive resting (\code{\link{upstateState}}) given unsuccessful oviposition (calculate with \code{MBDETES_PrSurvive(site,"O")}, uses parameters \code{O_surv}, and the local \code{\link{Site}} object's hazard)
#' @param E probability of skip oviposition (no skip oviposition in MBITES yet so must equal 0)
#' @param F1 only used if skip-oviposition is implementd (but E must be 0)
#' @param F2 probability to leave the current \code{\link{Site}} and go on a blood feeding search given an successful oviposition (calculate with \code{1 - MBDETES_PrLeave(site,"feed",FALSE)}, uses parameters \code{boutFail_p}, \code{disperse})
#' @param F3 probability to leave the current \code{\link{Site}} and go on a oviposition search given an unsuccessful oviposition attempt (calculate with \code{1 - MBDETES_PrLeave(site,"aqua",TRUE)}, uses parameters \code{boutFail_p}, \code{disperse})
#'
#' @export
ELAB_PAR <- function(
  A=  .83,
  B0= 0.0,  B1= 1, #B2=1-B0-B1
  C1= 0.0,  C2= 1,    #C3=1-C1-C2
  C4= 0.00,           #C5=1-C4
  D1= 0.95,
  D2= 0.95,
  E = 0,
  F1= 0.7,
  F2= 0.8,
  F3=0.7
){
  list(A=A, B0=B0, B1=B1, B2=1-sum(B0,B1),
       C1=C1, C2=C2, C3=1-sum(C1,C2),
       C4=C4, C5=1-C4,
       D1=D1, D2=D2, E=E,
       F1=F1, F2=F2, F3=F3)
}


#' MBDETES: Parameters for Transitions from F
#'
#' Produce named list of parameters for \code{\link{BFSB_F2X}}.
#' This function is useful for finding parameter sets that map between
#' MBITES and MBDETES parameter spaces.
#'
#' @param A probability of successful launch (maps to \code{Bs_succeed})
#' @param D1 probability to survive resting (\code{\link{upstateState}}) (calculate with \code{MBDETES_PrSurvive(site,"F")}, uses parameters \code{Bs_surv}, and the local \code{\link{Site}} object's hazard)
#' @param F1 probability to leave the current \code{\link{Site}} and go on a blood feeding search (calculate with \code{1 - MBDETES_PrLeave(site,"feed",FALSE)}, uses parameters \code{boutFail_p}, \code{disperse})
#'
#' @export
BFSB_PAR <- function(
  A=.94,
  D1=0.98,
  F1=0.8
){
  list(A=A,D1=D1,D2=D1,F1=F1)
}


#' MBDETES: Parameters for Transitions from L
#'
#' Produce named list of parameters for \code{\link{ELSB_L2X}}.
#' This function is useful for finding parameter sets that map between
#' MBITES and MBDETES parameter spaces.
#'
#' @param A probability of successful launch (maps to \code{Os_succeed})
#' @param D1 probability to survive resting (\code{\link{upstateState}}) (calculate with \code{MBDETES_PrSurvive(site,"L")}, uses parameters \code{Os_surv}, and the local \code{\link{Site}} object's hazard)
#' @param F1 probability to leave the current \code{\link{Site}} and go on a oviposition search (calculate with \code{1 - MBDETES_PrLeave(site,"aqua",FALSE)}, uses parameters \code{boutFail_p}, \code{disperse})
#'
#' @export
ELSB_PAR <- function(
  A=.94,
  D1=0.98,
  F1=0.8
){
  list(A=A,D1=D1,D2=D1,F1=F1)
}

#' MBDETES: State Transition Matrix from Interpolating Parameters
#'
#' From the parameters in the A,B,F1,F2,etc space, produce a named list of parameters
#' suitable for plugging into the MBDETES differential equation models (\code{\link{MBDETES_R2R_solve}} and \code{\link{MBDETES_cohort_solve}}).
#'
#' @param BFAB_PAR from \code{\link{BFAB_PAR}}, gives paramters for B and R transitions
#' @param ELAB_PAR from \code{\link{ELAB_PAR}}, gives paramters for O transitions
#' @param BFSB_PAR from \code{\link{BFSB_PAR}}, gives paramters for F transitions
#' @param ELSB_PAR from \code{\link{ELSB_PAR}}, gives paramters for L transitions
#' @param tF sojourn time in blood feeding search bout
#' @param tB sojourn time in blood feeding attempt bout
#' @param tR sojourn time in post-prandial resting bout
#' @param tL sojourn time in egg laying search bout
#' @param tO sojourn time in egg laying attempt bout
#'
#' @export
MBDETES_StateTransitions_Interp <- function(BFAB_PAR,ELAB_PAR,BFSB_PAR,ELSB_PAR,
  tF = 6/24, # 30 minutes
  tB = 3/24,   # 3 hours
  tR = 18/24,  # 18 hours
  tL = 6/24, # 30 minutes
  tO = 3/24   # 1 hour
){

  # create and fill the matrix
  M = matrix(0,nrow=6,ncol=6,dimnames=list(c("F","B","R","L","O","D"),c("F","B","R","L","O","D")))
  M[1,] = BFSB_F2X(BFSB_PAR)
  M[2,] = BFAB_B2X(BFAB_PAR)
  M[3,] = BFAB_R2X(BFAB_PAR)
  M[4,] = ELSB_L2X(ELSB_PAR)
  M[5,] = ELAB_O2X(ELAB_PAR)
  M[6,6] = 1 # death is absorbing

  PAR <- list(
    # timing
    tF = tF, tB = tB, tR = tR, tL = tL, tO = tO,
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

  return(PAR)
}
