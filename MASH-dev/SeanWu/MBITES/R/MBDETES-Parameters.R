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
#' @param D2 probability to be deterred prior to starting blood feeding a human host (maps to 1 - \code{feedH})
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
  H1= 0.80, H2= 0.80, H3= 0.8
){
list(A=A, B0=B0, B1=B1, B2=B2, B3=1-B0-B1-B2,
           C1=C1, C2=C2, C3=1-C1-C2, C4=C4,
           C5=C5, C6=1-C4-C5, C7=C7, C8=1-C7,
           D1=D1, D2=D2, D3=1-D1-D2, E=E,
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
#' @param D2 probability to survive resting (\code{\link{upstateState}}) given successful oviposition (calculate with \code{MBDETES_PrSurvive(site,"O")}, uses parameters \code{O_surv}, and the local \code{\link{Site}} object's hazard)
#' @param E probability of skip oviposition (no skip oviposition in MBITES yet so must equal 0)
#' @param F1 only used if skip-oviposition is implementd (but E must be 0)
#' @param F2 probability to leave the current \code{\link{Site}} and go on a blood feeding search given an unsuccessful blood meal attempt (calculate with \code{1 - MBDETES_PrLeave(site,"feed",FALSE)}, uses parameters \code{boutFail_p}, \code{disperse})
#' @param F3 probability to leave the current \code{\link{Site}} and go on a oviposition search given an unsuccessful blood meal attempt (calculate with \code{1 - MBDETES_PrLeave(site,"aqua",TRUE)}, uses parameters \code{boutFail_p}, \code{disperse})
#'
#' @export
ELAB_PAR <- function(
  A=  .83,
  B0= 0.0,  B1= 1, #B2=1-B0-B1
  C1= 0.0,  C2= 1,    #C3=1-C1-C2
  C4= 0.00,           #C5=1-C4
  D1= 0.95,
  D2= 0.9,
  E = 0,
  F1= 0.5,
  F2= 0.4,
  F3=0.7
){
  list(A=A, B0=B0, B1=B1, B2=1-B0-B1,
       C1=C1, C2=C2, C3=1-C1-C2,
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
  D1,
  F1
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
  D1,
  F1
){
  list(A=A,D1=D1,D2=D1,F1=F1)
}
