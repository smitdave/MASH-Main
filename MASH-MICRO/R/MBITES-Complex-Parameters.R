###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Parameters
#   MASH-MICRO Team
#   September 2017
#
###############################################################################

#################################################################
# M-BITES Parameters
#################################################################

#' Generate Parameters for M-BITES Complex Module
#'
#' Generate named list of parameters used throughout MICRO/M-BITES. All arguments have default values which are listed below before the definition.
#'
#' @param F_time mean time elapsed (in days) during blood feeding search bout
#' @param B_time mean time elapsed (in days) during blood feeding attempt bout
#' @param R_time mean time elapsed (in days) during post-prandial resting bout
#' @param L_time mean time elapsed (in days) during egg laying search bout
#' @param O_time mean time elapsed (in days) during egg laying attempt bout
#' @param M_time mean time elapsed (in days) during mating bout
#' @param S_time mean time elapsed (in days) during sugar feeding attempt bout
#' @param gammaShape shape parameter for Gamma distributed dwell times; if gammaShape = 1 same as exponential, variance decreases as gammaShape becomes large (see \code{\link{mbites_timingGamma}})
#' @param F_succeed blood feeding search bout probability of success
#' @param B_succeed blood feeding attempt bout probability of success
#' @param L_succeed egg laying search bout probability of success
#' @param O_succeed egg laying attempt bout probability of success
#' @param M_succeed mating bout probability of success
#' @param S_succeed sugar feeding attempt bout probability of success
#' @param F_surv blood feeding search bout baseline survival probaility (see \code{\link{mbitesGeneric_surviveFlight}})
#' @param B_surv blood feeding attempt bout baseline survival probaility (see \code{\link{mbitesGeneric_surviveFlight}})
#' @param R_surv post-prandial resting bout baseline survival probaility (see \code{\link{mbitesGeneric_surviveFlight}})
#' @param L_surv egg laying search bout baseline survival probaility (see \code{\link{mbitesGeneric_surviveFlight}})
#' @param O_surv egg laying attempt bout baseline survival probaility (see \code{\link{mbitesGeneric_surviveFlight}})
#' @param M_surv mating bout baseline survival probaility (see \code{\link{mbitesGeneric_surviveFlight}})
#' @param S_surv sugar feeding attempt bout baseline survival probaility (see \code{\link{mbitesGeneric_surviveFlight}})
#' @param F_wts blood feeding search bout landing spot weights (see \code{\link{mbites_get_WTS}})
#' @param B_wts blood feeding attempt bout landing spot weights (see \code{\link{mbites_get_WTS}})
#' @param R_wts post-prandial resting bout landing spot weights (see \code{\link{mbites_get_WTS}})
#' @param L_wts egg laying search bout landing spot weights (see \code{\link{mbites_get_WTS}})
#' @param O_wts egg laying attempt bout landing spot weights (see \code{\link{mbites_get_WTS}})
#' @param M_wts mating bout landing spot weights (see \code{\link{mbites_get_WTS}})
#' @param S_wts sugar feeding attempt bout landing spot weights (see \code{\link{mbites_get_WTS}})
#' @param InAndOut unnormalized transition matrix of landing probabilities
#' @param surviveH human host encounter survival probability of initial encounter (proceed to probe)
#' @param probeH human host encounter probability undeterred and begin probing
#' @param surviveprobeH human host encounter survival probability of probing
#' @param feedH human host encounter probability to successfully blood feed
#' @param surviveZ non-human host encounter survival probability of initial encounter (proceed to feed)
#' @param feedZ non-human host encounter probability to successfully blood feed
#' @param bm.a shape param for bloodmeal size (see \code{\link{mbitesGeneric_rBloodMealSize}})
#' @param bm.b shape param for bloodmeal size (see \code{\link{mbitesGeneric_rBloodMealSize}})
#' @param REFEED control boolean for refeeding behavior during resting bout
#' @param rf.a exp param for refeeding as function of bmSize (see \code{\link{mbitesGeneric_pReFeed}})
#' @param rf.b exp param for refeeding as function of bmSize (see \code{\link{mbitesGeneric_pReFeed}})
#' @param OVERFEED control boolean for overfeeding during \code{\link{mbitesBRO_BloodMeal}}
#' @param of.a exp param for overfeeding as function of bmSize (see \code{\link{mbitesGeneric_pOverFeed}})
#' @param of.b exp param for overfeeding as function of bmSize (see \code{\link{mbitesGeneric_pOverFeed}})
#' @param SENESCE control boolean for senesce mortality during \code{\link{mbitesGeneric_surviveFlight}}
#' @param sns.a exp param for senescence (see \code{\link{mbitesGeneric_pSenesce}})
#' @param sns.b exp param for senescence (see \code{\link{mbitesGeneric_pSenesce}})
#' @param TATTER control boolean for wing tattering during \code{\link{mbitesGeneric_surviveFlight}}
#' @param ttsz.p zero-inflation for tattering damage (see \code{\link{mbitesGeneric_rTatterSize}})
#' @param ttsz.a shape param for tattering damage (see \code{\link{mbitesGeneric_rTatterSize}})
#' @param ttsz.b shape param for tattering damage (see \code{\link{mbitesGeneric_rTatterSize}})
#' @param ttr.a exp param for tattering survival (see \code{\link{mbitesGeneric_pTatter}})
#' @param ttr.b exp param for tattering survival (see \code{\link{mbitesGeneric_pTatter}})
#' @param S.u per-bout energy expenditure
#' @param S.a shape parameter of per-bout probability of survival as function of energy reserves
#' @param S.b shape parameter of per-bout probability of survival as function of energy reserves
#' @param S.sa shape parameter of probability to queue sugar bout as function of energy reserves
#' @param S.sb shape parameter of probability to queue sugar bout as function of energy reserves
#' @param bs.m mean of normally-distributed egg batch size (used in \code{\link{mbitesGeneric_rBatchSizeNorm}})
#' @param bs.v standard deviation of normally-distributed egg batch size (used in \code{\link{mbitesGeneric_rBatchSizeNorm}})
#' @param maxBatch maximum egg batch size (used in \code{\link{mbitesGeneric_rBatchSizeBms}})
#' @param emt.m mean of normally-distributed egg batch maturation time (used in \code{\link{mbitesGeneric_rEggMaturationTimeNorm}})
#' @param emt.v standard deviation of normally-distributed egg batch maturation time (used in \code{\link{mbitesGeneric_rEggMaturationTimeNorm}})
#' @param eggT minimum time to egg maturation
#' @param eggP minimum provision to produce eggs
#' @param energyPreG pre-gonotrophic energy requirement
#' @param preGsugar amount of energy a sugar meal contributes to energyPreG (pre-gonotrophic energy requirement)
#' @param preGblood amount of energy a blood meal contributes to energyPreG (pre-gonotrophic energy requirement)
#' @param PfEIP entomological inoculation period for Plasmodium falciparum during \code{MosquitoFemale$probing()}
#' @param Q human blood index (used in \code{\link{mbitesCohort_chooseHost}})
#' @return a named list of parameters
#' @examples
#' MBITES.Complex.Parameters()
#' @export
MBITES.Complex.Parameters <- function(

  # dwell time parameters
  F_time                  = 1,
  B_time                  = 0.75,
  R_time                  = 1.5,
  L_time                  = 0.75,
  O_time                  = 1,
  M_time                  = 1.5,
  S_time                  = 0.5,

  gammaShape              = 8,

  # success parameters
  F_succeed               = 0.99,
  B_succeed               = 0.99,
  L_succeed               = 0.99,
  O_succeed               = 0.99,
  M_succeed               = 0.95,
  S_succeed               = 0.95,

  # survival parameters
  F_surv                  = 0.95,
  B_surv                  = 0.98,
  R_surv                  = 0.98,
  L_surv                  = 0.80,
  O_surv                  = 0.98,
  M_surv                  = 0.98,
  S_surv                  = 0.98,

  # landing spot weights
  F_wts                   = rep(1,5),
  B_wts                   = rep(1,5),
  R_wts                   = rep(1,5),
  L_wts                   = rep(1,5),
  O_wts                   = rep(1,5),
  M_wts                   = rep(1,5),
  S_wts                   = rep(1,5),

  InAndOut                = matrix(data = c(4,2,2,1,6,
                                            2,1,1,1,4,
                                            1,1,1,1,2,
                                            0,0,0,0,1,
                                            1,1,2,1,0),
                                            nrow = 5,ncol = 5,byrow = TRUE,dimnames = list(c("i","w","v","r","l"),c("i","w","v","r","l"))),
  # Host Encounter
  # human host
  surviveH                = 1, # survival probability for initial encounter (survive to probe)
  probeH                  = 1, # probability that undeterred during probing
  surviveprobeH           = 1, # survival probability for host probing
  feedH                   = 1, # probability to successfully feed

  #animal host
  surviveZ                = 1, # survival probability for initial encounter (survive to feed)
  feedZ                   = 1, # probability to successfully feed

  # BloodMeal
  bm.a                    = 7.5, #shape param for bloodmeal size
  bm.b                    = 2.5, #shape param for bloodmeal size

  # ReFeed
  REFEED                  = FALSE,
  rf.a                    = 60, #exp param for refeeding as function of bmSize
  rf.b                    = 5e3, #exp param for refeeding as function of bmSize

  # Overfeed
  OVERFEED                = FALSE,
  of.a                    = 5, #exp param for overfeeding as function of bmSize
  of.b                    = 5e3, #exp param for overfeeding as function of bmSize

  # SENESCE
  SENESCE                 = FALSE,
  sns.a                   = 0.085, #exp param for senescence
  sns.b                   = 100, #exp param for senescence

  # TATTER
  TATTER                  = FALSE,
  ttsz.p                  = 0.5, #zero-inflation for tattering damage
  ttsz.a                  = 5, #shape param for tattering damage
  ttsz.b                  = 95, #shape param for tattering damage
  ttr.a                   = 15, #exp param for tattering survival
  ttr.b                   = 500, #exp param for tattering survival

  # SUGAR
  S.u                     = 1/7,
  S.a                     = 20,
  S.b                     = 10,
  S.sa                    = 15,
  S.sb                    = 5,

  # Reproduction & Development
  bs.m                    = 30, # used in mbitesGeneric_rBatchSizeNorm
  bs.v                    = 5, # used in mbitesGeneric_rBatchSizeNorm
  maxBatch                = 30, # maximum egg batch size (used in mbitesGeneric_rBatchSizeBms)
  emt.m                   = 3, # used in mbitesGeneric_rEggMaturationTimeNorm
  emt.v                   = 1, # used in mbitesGeneric_rEggMaturationTimeNorm
  eggT                    = 0, # minimum time to egg maturation
  eggP                    = 0, # minimum provision to produce eggs
  energyPreG              = 0, # pre-gonotrophic energy requirement
  preGsugar               = 0, # sugar units to fill pre-gonotrophic energy requirement
  preGblood               = 0, # blood units to fill pre-gonotrophic energy requirement

  # Pathogen Transmission
  PfEIP                   = 12,

  # MBITES-Complex-Cohort Parameters
  Q                       = 0.9 # human blood index
){

  out = list(

    # dwell time parameters
    F_time                  = F_time,
    B_time                  = B_time,
    R_time                  = R_time,
    L_time                  = L_time,
    O_time                  = O_time,
    M_time                  = M_time,
    S_time                  = S_time,

    gammaShape              = gammaShape,

    # success parameters
    F_succeed               = F_succeed,
    B_succeed               = B_succeed,
    L_succeed               = L_succeed,
    O_succeed               = O_succeed,
    M_succeed               = M_succeed,
    S_succeed               = S_succeed,

    # survival parameters
    F_surv                  = F_surv,
    B_surv                  = B_surv,
    R_surv                  = R_surv,
    L_surv                  = L_surv,
    O_surv                  = O_surv,
    M_surv                  = M_surv,
    S_surv                  = S_surv,

    # landing spot weights
    F_wts                   = F_wts,
    B_wts                   = B_wts,
    R_wts                   = R_wts,
    L_wts                   = L_wts,
    O_wts                   = O_wts,
    M_wts                   = M_wts,
    S_wts                   = S_wts,

    InAndOut                = InAndOut,
    # Host Encounter
    # human host
    surviveH                = surviveH,
    probeH                  = probeH,
    surviveprobeH           = surviveprobeH,
    feedH                   = feedH,

    #animal host
    surviveZ                = surviveZ,
    feedZ                   = feedZ,

    # BloodMeal
    bm.a                    = bm.a,
    bm.b                    = bm.b,

    # ReFeed
    REFEED                  = REFEED,
    rf.a                    = rf.a,
    rf.b                    = rf.b,

    # Overfeed
    OVERFEED                = OVERFEED,
    of.a                    = of.a,
    of.b                    = of.b,

    # SENESCE
    SENESCE                 = SENESCE,
    sns.a                   = sns.a,
    sns.b                   = sns.b,

    # TATTER
    TATTER                  = TATTER,
    ttsz.p                  = ttsz.p,
    ttsz.a                  = ttsz.a,
    ttsz.b                  = ttsz.b,
    ttr.a                   = ttr.a,
    ttr.b                   = ttr.b,

    # SUGAR
    S.u                     = S.u,
    S.a                     = S.a,
    S.b                     = S.b,
    S.sa                    = S.sa,
    S.sb                    = S.sb,

    # Reproduction & Development
    bs.m                    = bs.m,
    bs.v                    = bs.v,
    maxBatch                = maxBatch,
    emt.m                   = emt.m,
    emt.v                   = emt.v,
    eggT                    = eggT,
    eggP                    = eggP,
    energyPreG              = energyPreG,
    preGsugar               = preGsugar,
    preGblood               = preGblood,

    # Pathogen Transmission
    PfEIP                   = PfEIP,

    # MBITES-Complex-Cohort Parameters
    Q                       = Q,

    # State Space
    stateSpace = c("F","B","R","L","O","M","S"),
    initState = "M",
    lspot = c("i","w","v","r","l"),
    Fstate = c(F=0,B=0,R=0,L=0,O=0,M=0,S=0)

  )

  return(out)
}
