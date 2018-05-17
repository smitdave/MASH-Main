###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Parameters
#     MBITES Team
#     February 2018
#
###############################################################################

#' MBITES Parameters Singleton
#'
#' This class is a singleton object in the \code{MBITES} package namespace that stores parameters needed for the MBITES simulation.
#' It can be accessed by \code{MBITES:::MBITES_Pars}.
#'
#'
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * id: integer identifier of site
#'  * field: im a field!
#'
MBITES_Parameters <- R6::R6Class(classname = "MBITES_Parameters",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 public = list(

                   # begin constructor
                   initialize = function(){
                     futile.logger::flog.trace("MBITES_Parameters being born at: self %s , private %s",pryr::address(self),pryr::address(private))
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("MBITES_Parameters being killed at: self %s , private %s",pryr::address(self),pryr::address(private))
                   }, # end destructor

                   # time to event samplers
                   # 'struct' of function (pointers to functions in c++)
                   ttEvent = list(
                     # time to event closures (must be given implementations before simulation starts)
                     # attempt bouts
                     BoutB = function(){stop("BoutB requires a concrete implementation!")},
                     BoutO = function(){stop("BoutO requires a concrete implementation!")},
                     BoutM = function(){stop("BoutM requires a concrete implementation!")},
                     BoutS = function(){stop("BoutS requires a concrete implementation!")},
                     # search bouts
                     BoutBs = function(){stop("BoutBs requires a concrete implementation!")},
                     BoutOs = function(){stop("BoutOs requires a concrete implementation!")},
                     BoutMs = function(){stop("BoutMs requires a concrete implementation!")},
                     BoutSs = function(){stop("BoutSs requires a concrete implementation!")},
                     # estivation
                     Estivate = function(){stop("Estivate requires a concrete implementation!")},
                     # post-prandial resting
                     ppr = function(){stop("ppr requires a concrete implementation!")}
                   )

                 ),

                 private = list(

                   aqua_model           = "emerge",

                   # Search behavior
                   disperse             = numeric(1), # P(move) even if resources present when i do checks

                   # Post-bout Landing, House Entering, and Resting
                   boutFail_p           = numeric(1), # 1/number of failed bouts until mosquito gives up and searches
                   b_wts                = numeric(5), # weights on {i,r,v,w,l}
                   o_wts                = numeric(5),
                   m_wts                = numeric(5),
                   s_wts                = numeric(5),
                   InAndOut             = matrix(0L,5,5), # weights on transitioning between resting spots

                   # Timing
                   tSwarm               = numeric(1), # mating swarm timing
                   # estivation 1
                   Emax                 = numeric(1),
                   Eb                   = numeric(1),
                   Ep                   = numeric(1),
                   eEndm                = numeric(1),
                   eEndSd               = numeric(1),
                   # estivation 2
                   estivationDay        = integer(1),

                   # Energetics
                   energyPreG           = numeric(1), # pre-gonotrophic energy requirement
                   preGsugar            = numeric(1), # sugar energy that can satisfy pre-gonotrophic energy
                   energyFromBlood_b    = numeric(1), # half-maximum parameter for mbites_energyFromBlood
                   S_u                  = numeric(1), # energy expended during a flight
                   S_sa                 = numeric(1), # pSugarBout
                   S_sb                 = numeric(1), # pSugarBout

                   # Survival
                   Bs_surv              = numeric(1),
                   Os_surv              = numeric(1),
                   Ms_surv              = numeric(1),
                   Ss_surv              = numeric(1),
                   B_surv               = numeric(1),
                   O_surv               = numeric(1),
                   M_surv               = numeric(1),
                   S_surv               = numeric(1),

                   # Bout success
                   Bs_succeed           = numeric(1),
                   Os_succeed           = numeric(1),
                   Ms_succeed           = numeric(1),
                   Ss_succeed           = numeric(1),
                   B_succeed            = numeric(1),
                   O_succeed            = numeric(1),
                   M_succeed            = numeric(1),
                   S_succeed            = numeric(1),

                   # Host Encounter
                   surviveH                = numeric(1), # survival probability for initial encounter (survive to probe)
                   probeH                  = numeric(1), # probability that undeterred during probing
                   surviveprobeH           = numeric(1), # survival probability for host probing
                   feedH                   = numeric(1), # probability to successfully feed
                   surviveZ                = numeric(1), # survival probability for initial encounter (survive to feed)
                   feedZ                   = numeric(1), # probability to successfully feed

                   PPR_a                = numeric(1), # mbites_pPPRFlight
                   PPR_b                = numeric(1), # mbites_pPPRFlight
                   S_a                  = numeric(1), # mbites_pEnergySurvival
                   S_b                  = numeric(1), # mbites_pEnergySurvival

                   TATTER               = logical(1), # control tattering
                   ttsz_p               = numeric(1), # wing tattering (size of damage)
                   ttsz_a               = numeric(1),
                   ttsz_b               = numeric(1),

                   ttr_a                = numeric(1), # wing tattering (probability of death)
                   ttr_b                = numeric(1),

                   chm_a                = numeric(1), # chemical damage to mosquito body
                   chm_b                = numeric(1),

                   SENESCE              = logical(1), # control senescence
                   sns_a                = numeric(1), # senescence parameters
                   sns_b                = numeric(1), # senescence parameters

                   # BloodMeal
                   bm_a                 = numeric(1), # Beta-distributed bloodmeal size
                   bm_b                 = numeric(1), # Beta-distributed bloodmeal size

                   OVERFEED             = logical(1), # control overfeeding
                   of_a                 = numeric(1),
                   of_b                 = numeric(1),

                   # Oogenesis
                   bloodPerEgg          = numeric(1), # egg provision: how much blood does each egg need?
                   bs_m                 = numeric(1), # mean of normally dist. egg batch
                   bs_sd                = numeric(1), # sd of normally dist. egg batch
                   maxBatch             = integer(1), # max batch size
                   emt_m                = numeric(1), # mean of normally dist. maturation time
                   emt_sd               = numeric(1), # sd of normally dist. maturation time

                   rf_a                 = numeric(1), # refeeding probability
                   rf_b                 = numeric(1)

                 )
) # end MBITES_Parameters class definition


###############################################################################
# Timing
###############################################################################

#' MBITES Parameters: Make Deterministic Time-to-Event Closure
#'
#' Generates a closure that contains the following fields:
#'  * wait: numeric value of deterministic waiting time
#'
#' The closure also contains the following functions:
#'  * tteDet(t): samples a time to event following shifted exponential distribution
#'
#' @export
make_ttEvent_Det <- function(wait){

  # check bad arguments
  if(any(sapply(as.list(match.call()),is.null))) {
    stop("please do not pass any NULL arguments")
  }

  # data for the closure
  wait  <- wait

  # deterministic tte
  tteDet <- function(t){
    t + wait
  }

  return(tteDet)
}

#' MBITES Parameters: Make a Shifted Exponential Time-to-Event Closure
#'
#' Generates a closure that contains the following fields:
#'  * rate: numeric value of rate parameter
#'  * tmin: numeric value of minimum waiting time
#'
#' The closure also contains the following functions:
#'  * tteShiftExp(t): samples a time to event following shifted exponential distribution
#'
#' @export
make_ttEvent_Exp <- function(rate, tmin){

  # check bad arguments
  if(any(sapply(as.list(match.call()),is.null))) {
    stop("please do not pass any NULL arguments")
  }

  # data for the closure
  rate  <- rate
  tmin <- tmin

  # exponentially-distributed tte
  tteShiftExp <- function(t){
    t + tmin + rexp(n=1L,rate=rate)
  }

  return(tteShiftExp)
}

#' MBITES Parameters: Make a Shifted Gamma Time-to-Event Closure
#'
#' Generates a closure that contains the following fields:
#'  * mean: numeric value of mean
#'  * cv: numeric value of coefficient of variation (should be between 0 and 1 for reasonable sampling)
#'  * tmin: numeric value of minimum waiting time
#'
#' The closure also contains the following functions:
#'  * tteShiftGamma(t): samples a time to event following shifted exponential distribution
#'
#' @export
make_ttEvent_Gamma <- function(mean, cv, tmin){

  # check bad arguments
  if(any(sapply(as.list(match.call()),is.null))) {
    stop("please do not pass any NULL arguments")
  }

  # data for the closure
  shape  <- 1/cv
  scale <- mean*cv
  tmin <- tmin

  tteShiftGamma <- function(t){
    t + tmin + rgamma(n=1L, shape=shape, scale = scale)
  }

  return(tteShiftGamma)
}

#' MBITES Parameters: Make a Shifted Diurnal Time-to-Event Closure
#'
#' Generates a closure that contains the following fields:
#'  * field: i'm a field!
#'
#' The closure also contains the following functions:
#'  * function: i'm a function!
#'
#' @export
make_ttEvent_Diurnal <- function(peak, tmin){

  if(any(sapply(as.list(match.call()),is.null))) {
    stop("please do not pass any NULL arguments")
  }

  stop("make_ttEvent_Diurnal has not been implemented yet")

}

#' MBITES Parameters: Set Time-to-Event Sampling for Attempt and Search Bout Launch Intervals
#'
#' Sets concrete implementations of time-to-event sampling for attempt and search bout launch intervals. This method is called from \code{\link{MBITES_Setup}} and should
#' not be called by users, although there is currently no way to prevent this.
#'
#' This method is bound to \code{MBITES_Parameters$set_ttEvent}
#'
set_ttEvent_MBITES_Parameters <- function(
  # Timing
  timing_model = 1L,
  # deterministic
  wait_b = NULL,
  wait_o = NULL,
  wait_m = NULL,
  wait_s = NULL,
  wait_bs = NULL,
  wait_os = NULL,
  wait_ms = NULL,
  wait_ss = NULL,
  # exp
  rate_b = NULL,
  tmin_b = NULL,
  rate_o = NULL,
  tmin_o = NULL,
  rate_m = NULL,
  tmin_m = NULL,
  rate_s = NULL,
  tmin_s = NULL,
  rate_bs = NULL,
  tmin_bs = NULL,
  rate_os = NULL,
  tmin_os = NULL,
  rate_ms = NULL,
  tmin_ms = NULL,
  rate_ss = NULL,
  tmin_ss = NULL,
  # gamma
  mean_b = NULL,
  cv_b = NULL,
  mean_o = NULL,
  cv_o = NULL,
  mean_m = NULL,
  cv_m = NULL,
  mean_s = NULL,
  cv_s = NULL,
  mean_bs = NULL,
  cv_bs = NULL,
  mean_os = NULL,
  cv_os = NULL,
  mean_ms = NULL,
  cv_ms = NULL,
  mean_ss = NULL,
  cv_ss = NULL
){
  switch(timing_model,
    "1" = {
      cat("using deterministic time-to-event functions for attempt and search bouts\n")
      # attempt bouts
      self$ttEvent$BoutB <- make_ttEvent_Det(wait_b)
      self$ttEvent$BoutO <- make_ttEvent_Det(wait_o)
      self$ttEvent$BoutM <- make_ttEvent_Det(wait_m)
      self$ttEvent$BoutS <- make_ttEvent_Det(wait_s)
      # search bouts
      self$ttEvent$BoutBs <- make_ttEvent_Det(wait_bs)
      self$ttEvent$BoutOs <- make_ttEvent_Det(wait_os)
      self$ttEvent$BoutMs <- make_ttEvent_Det(wait_ms)
      self$ttEvent$BoutSs <- make_ttEvent_Det(wait_ss)
    },
    "2" = {
      cat("using shifted exponential time-to-event functions for attempt and search bouts\n")
      # attempt bouts
      self$ttEvent$BoutB <- make_ttEvent_Exp(rate_b,tmin_b)
      self$ttEvent$BoutO <- make_ttEvent_Exp(rate_o,tmin_o)
      self$ttEvent$BoutM <- make_ttEvent_Exp(rate_m,tmin_m)
      self$ttEvent$BoutS <- make_ttEvent_Exp(rate_s,tmin_s)
      # search bouts
      self$ttEvent$BoutBs <- make_ttEvent_Exp(rate_bs,tmin_bs)
      self$ttEvent$BoutOs <- make_ttEvent_Exp(rate_os,tmin_os)
      self$ttEvent$BoutMs <- make_ttEvent_Exp(rate_ms,tmin_ms)
      self$ttEvent$BoutSs <- make_ttEvent_Exp(rate_ss,tmin_ss)
    },
    "3" = {
      cat("using shifted gamma time-to-event functions for attempt and search bouts\n")
      # attempt bouts
      self$ttEvent$BoutB <- make_ttEvent_Gamma(mean_b,cv_b,tmin_b)
      self$ttEvent$BoutO <- make_ttEvent_Gamma(mean_o,cv_o,tmin_o)
      self$ttEvent$BoutM <- make_ttEvent_Gamma(mean_m,cv_m,tmin_m)
      self$ttEvent$BoutS <- make_ttEvent_Gamma(mean_s,cv_s,tmin_s)
      # search bouts
      self$ttEvent$BoutBs <- make_ttEvent_Gamma(mean_bs,cv_bs,tmin_bs)
      self$ttEvent$BoutOs <- make_ttEvent_Gamma(mean_os,cv_os,tmin_os)
      self$ttEvent$BoutMs <- make_ttEvent_Gamma(mean_ms,cv_ms,tmin_ms)
      self$ttEvent$BoutSs <- make_ttEvent_Gamma(mean_ss,cv_ss,tmin_ss)
    },
    "4" = {
      cat("using shifted diurnal time-to-event functions for attempt and search bouts\n")
      stop("shifted diurnal time-to-event sampling has not been implemented yet\n")
    },
    {stop("invalid entry for 'timing_model'\n")}
  )

}

#' MBITES Parameters: Set Post-prandial Resting Time
#'
#' Sets concrete implementations of length of the post-prandial resting bout. This method is called from \code{\link{MBITES_Setup}} and should
#' not be called by users, although there is currently no way to prevent this.
#'
#' This method is bound to \code{MBITES_Parameters$set_ttEvent_ppr}
#'
set_ttEvent_ppr_MBITES_Parameters <- function(
  ppr_model = 1L,
  # det
  wait_ppr = NULL,
  # exp
  rate_ppr = NULL,
  tmin_ppr = NULL,
  # gamma
  mean_ppr = NULL,
  cv_ppr = NULL
){
  switch(ppr_model,
    "1" = {
      cat("using deterministic time-to-event functions for post-prandial resting bout\n")
      self$ttEvent$ppr <- make_ttEvent_Det(wait_ppr)
    },
    "2" = {
      cat("using shifted exponential time-to-event functions for post-prandial resting bout\n")
      self$ttEvent$ppr <- make_ttEvent_Exp(rate_ppr,tmin_ppr)
    },
    "3" = {
      cat("using shifted gamma time-to-event functions for post-prandial resting bout\n")
      self$ttEvent$ppr <- make_ttEvent_Gamma(mean_ppr,cv_ppr,tmin_ppr)
    },
    {stop("invalid entry for 'ppr_model'\n")}
  )
}

# set methods
MBITES_Parameters$set(which = "public",name = "set_ttEvent",
    value = set_ttEvent_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "set_ttEvent_ppr",
    value = set_ttEvent_ppr_MBITES_Parameters, overwrite = TRUE
)


###############################################################################
# Set Parameters
###############################################################################

#' MBITES Parameters: Set Parameters
#'
#' Parameter descriptions below.
#'
#' @param defaultState_F character of behavioral state that female mosquito has when it emerges
#' @param defaultState_M character of behavioral state that male mosquito has when it emerges
#' @param aqua_model character in \code{emerge,EL4P}
#' @param disperse probability for mosquito to initiate a search bout even if resources are present, called from \code{\link{mbites_checkForResources}}
#' @param boutFail_p geometric probability to give up and initiate a search after a failed bout, called from \code{\link{mbites_boutFailCheck}}
#' @param b_wts behavior-specific landing spot weights, caled from \code{\link{mbites_newSpot}}
#' @param o_wts behavior-specific landing spot weights, caled from \code{\link{mbites_newSpot}}
#' @param m_wts behavior-specific landing spot weights, caled from \code{\link{mbites_newSpot}}
#' @param s_wts behavior-specific landing spot weights, caled from \code{\link{mbites_newSpot}}
#' @param InAndOut named transition matrix between landing spots, called from \code{\link{mbites_newSpot}}
#' @param tSwarm time of day to start swarming behavior, called from \code{\link{mbites_findSwarm}}
#' @param Emax related to daily estivation probability, called from \code{\link{mbites_prEstivate}}
#' @param Eb related to daily estivation probability, called from \code{\link{mbites_prEstivate}}
#' @param Ep probability to survive estivation, called from \code{\link{mbites_checkEstivation1}}
#' @param eEndm mean day of year to wakeup from estivation, called from \code{\link{mbites_wakeUpTime}}
#' @param eEndSd sd of day of year to wakeup from estivation, called from \code{\link{mbites_wakeUpTime}}
#' @param estivationDay day of the year to deterministically initiate estivation, called from \code{\link{mbites_checkEstivation2}}
#' @param energyPreG pre-gonotrophic energy requirement, called from \code{\link{mbites_BloodEnergetics}} and \code{\link{mbites_sugarMeal}}
#' @param preGsugar amount of energy a sugar meal contributes to the pre-gonotrophic energy requirement, called from \code{\link{mbites_sugarMeal}}
#' @param energyFromBlood_b amount of energy derived per unit of blood, called from \code{\link{mbites_energyFromBlood}}
#' @param S_u amount of energy burned by a mosquito during a bout, called from \code{\link{mbites_flightBurnEnergy}}
#' @param S_sa parameter for probability to queue a sugar bout as function of a mosquito's energy, called from \code{\link{mbites_pSugarBout}}
#' @param S_sb parameter for probability to queue a sugar bout as function of a mosquito's energy, called from \code{\link{mbites_pSugarBout}}
#' @param Bs_surv blood feeding search bout baseline survival probability, called from \code{\link{surviveFlight}}
#' @param Os_surv oviposition search bout baseline survival probability, called from \code{\link{surviveFlight}}
#' @param Ms_surv mating search bout baseline survival probability, called from \code{\link{surviveFlight}}
#' @param Ss_surv sugar feeding search bout baseline survival probability, called from \code{\link{surviveFlight}}
#' @param B_surv blood feeding bout baseline survival probability, called from \code{\link{surviveFlight}}
#' @param O_surv oviposition bout baseline survival probability, called from \code{\link{surviveFlight}}
#' @param M_surv mating bout baseline survival probability, called from \code{\link{surviveFlight}}
#' @param S_surv sugar feeding bout baseline survival probability, called from \code{\link{surviveFlight}}
#' @param Bs_succeed
#' @param Os_succeed
#' @param Ms_succeed
#' @param Ss_succeed
#' @param B_succeed
#' @param O_succeed
#' @param M_succeed
#' @param S_succeed
#' @param surviveH survival probability for initial encounter (survive to probe)
#' @param probeH probability that undeterred during probing
#' @param surviveprobeH survival probability for host probing
#' @param feedH probability to successfully feed
#' @param surviveZ survival probability for initial encounter (survive to feed)
#' @param feedZ probability to successfully feed
#' @param PPR_a parameter for probability of death during post-prandial resting as function of blood meal size, called from \code{\link{mbites_pPPRFlight}}
#' @param PPR_b parameter for probability of death during post-prandial resting as function of blood meal size, called from \code{\link{mbites_pPPRFlight}}
#' @param S_a parameter for probability of death due to energy reserves, called from \code{\link{mbites_pEnergySurvival}}
#' @param S_b parameter for probability of death due to energy reserves, called from \code{\link{mbites_pEnergySurvival}}
#' @param ttsz_p zero-inflation for wing damage, called from \code{\link{mbites_WingTattering}}
#' @param ttsz_a alpha parameter for beta distributed wing damage, called from \code{\link{mbites_WingTattering}}
#' @param ttsz_b beta parameter for beta distributed wing damage, called from \code{\link{mbites_WingTattering}}
#' @param ttr_a parameter for probability of death due to wing tattering, called from \code{\link{mbites_pTatter}}
#' @param ttr_b parameter for probability of death due to wing tattering, called from \code{\link{mbites_pTatter}}
#' @param chm_a parameter for probability of death due to chemical damage, called from \code{\link{mbites_pChem}}
#' @param chm_b parameter for probability of death due to chemical damage, called from \code{\link{mbites_pChem}}
#' @param sns_a parameter for probability of death due to senescence, called from \code{\link{mbites_pSenesce}}
#' @param sns_b parameter for probability of death due to senescence, called from \code{\link{mbites_pSenesce}}
#' @param bm_a alpha parameter for beta distributed blood meal size, called from \code{\link{mbites_rBloodMealSize}}
#' @param bm_b beta parameter for beta distributed blood meal size, called from \code{\link{mbites_rBloodMealSize}}
#' @param of_a parameter for probability of death due to overfeeding, called from \code{\link{mbites_pOverFeed}}
#' @param of_b parameter for probability of death due to overfeeding, called from \code{\link{mbites_pOverFeed}}
#' @param bloodPerEgg units of blood required for each egg to develop, called from \code{\link{mbites_oogenesis2}}
#' @param bs_m mean of normally distributed egg batch size, called from \code{\link{mbites_rBatchSizeNorm}}
#' @param bs_sd standard deviation of normally distributed egg batch size, called from \code{\link{mbites_rBatchSizeNorm}}
#' @param maxBatch maximum allowable egg batch size, called from \code{\link{mbites_rBatchSizeBms}}
#' @param emt_m mean of normally distributed egg maturation time, called from \code{\link{mbites_rEggMaturationTimeNorm}}
#' @param emt_sd standard deviation of normally distributed egg maturation time, called from \code{\link{mbites_rEggMaturationTimeNorm}}
#' @param rf_a parameter for probably of refeeding as function of blood meal size, called from \code{\link{mbites_pReFeed}}
#' @param rf_b parameter for probably of refeeding as function of blood meal size, called from \code{\link{mbites_pReFeed}}
#'
set_parameters_MBITES_Parameters <- function(

  defaultState_F       = "B",
  defaultState_M       = "S",
  aqua_model           = "emerge",

  # Search behavior
  disperse             = 0, # P(move) even if resources present when i do checks

  # Post-bout Landing, House Entering, and Resting
  boutFail_p           = 1/3, # 1/number of failed bouts until mosquito gives up and searches
  b_wts                = rep(1,5), # weights on {i,r,v,w,l}
  o_wts                = rep(1,5),
  m_wts                = rep(1,5),
  s_wts                = rep(1,5),
  InAndOut             = matrix(data = c(4,2,2,1,6,
                                         2,1,1,1,4,
                                         1,1,1,1,2,
                                         0,0,0,0,1,
                                         1,1,2,1,0),
                                         nrow = 5,ncol = 5,byrow = TRUE,dimnames = list(c("i","w","v","r","l"),c("i","w","v","r","l"))), # weights on transitioning between resting spots

  # Timing
  tSwarm               = 18.5/24, # mating swarm timing
  # estivation 1
  Emax                 = NaN,
  Eb                   = NaN,
  Ep                   = NaN,
  eEndm                = NaN,
  eEndSd               = NaN,
  # estivation 2
  estivationDay        = NaN,

  # Energetics
  energyPreG           = 0, # pre-gonotrophic energy requirement
  preGsugar            = 0, # sugar energy that can satisfy pre-gonotrophic energy
  energyFromBlood_b    = 0.25, # half-maximum parameter for mbites_energyFromBlood
  S_u                  = 1/7, # energy expended during a flight
  S_sa                 = 15, # pSugarBout
  S_sb                 = 5, # pSugarBout

  # Survival (baseline survival, see MBITES-Survival)
  Bs_surv              = 0.95,
  Os_surv              = 0.90,
  Ms_surv              = 0.98,
  Ss_surv              = 0.98,

  B_surv               = 0.98,
  O_surv               = 0.98,
  M_surv               = 0.98,
  S_surv               = 0.98,

  # success
  Bs_succeed              = 0.99,
  Os_succeed              = 0.99,
  Ms_succeed              = 0.99,
  Ss_succeed              = 0.99,
  B_succeed               = 0.95,
  O_succeed               = 0.95,
  M_succeed               = 0.99,
  S_succeed               = 0.99,

  surviveH                = 1,
  probeH                  = 1,
  surviveprobeH           = 1,
  feedH                   = 1,
  surviveZ                = 1,
  feedZ                   = 1,

  PPR_a                = 5, # mbites_pPPRFlight
  PPR_b                = 1000, # mbites_pPPRFlight
  S_a                  = 20, # mbites_pEnergySurvival
  S_b                  = 10, # mbites_pEnergySurvival

  ttsz_p               = 0.5, # wing tattering (size of damage)
  ttsz_a               = 5,
  ttsz_b               = 95,

  ttr_a                = 15, # wing tattering (probability of death)
  ttr_b                = 500,

  chm_a                = 15, # chemical damage to mosquito body
  chm_b                = 500,

  sns_a                = 0.085, # senescence parameters
  sns_b                = 100, # senescence parameters

  # BloodMeal
  bm_a                 = 7.5, # Beta-distributed bloodmeal size
  bm_b                 = 2.5, # Beta-distributed bloodmeal size

  # overfeeding
  of_a                 = 5,
  of_b                 = 5e3,

  # Oogenesis
  bloodPerEgg          = 0.05, # egg provision: how much blood does each egg need?
  bs_m                 = 30, # mean of normally dist. egg batch
  bs_sd                = 5, # sd of normally dist. egg batch
  maxBatch             = 60, # max batch size
  emt_m                = 3, # mean of normally dist. maturation time
  emt_sd               = 1, # sd of normally dist. maturation time

  rf_a                 = 60, # refeeding probability
  rf_b                 = 5e3

){

  private$defaultState_F       = defaultState_F
  private$defaultState_M       = defaultState_M
  private$aqua_model           = aqua_model
  private$disperse             = disperse
  private$boutFail_p           = boutFail_p
  private$b_wts                = b_wts
  private$o_wts                = o_wts
  private$m_wts                = m_wts
  private$s_wts                = s_wts
  private$InAndOut             = InAndOut
  private$tSwarm               = tSwarm
  private$Emax                 = Emax
  private$Eb                   = Eb
  private$Ep                   = Ep
  private$eEndm                = eEndm
  private$eEndSd               = eEndSd
  private$estivationDay        = estivationDay
  private$energyPreG           = energyPreG
  private$preGsugar            = preGsugar
  private$energyFromBlood_b    = energyFromBlood_b
  private$S_u                  = S_u
  private$S_sa                 = S_sa
  private$S_sb                 = S_sb
  private$Bs_surv              = Bs_surv
  private$Os_surv              = Os_surv
  private$Ms_surv              = Ms_surv
  private$Ss_surv              = Ss_surv
  private$B_surv               = B_surv
  private$O_surv               = O_surv
  private$M_surv               = M_surv
  private$S_surv               = S_surv
  private$Bs_succeed           = Bs_succeed
  private$Os_succeed           = Os_succeed
  private$Ms_succeed           = Ms_succeed
  private$Ss_succeed           = Ss_succeed
  private$B_succeed            = B_succeed
  private$O_succeed            = O_succeed
  private$M_succeed            = M_succeed
  private$S_succeed            = S_succeed
  private$surviveH             = surviveH
  private$probeH               = probeH
  private$surviveprobeH        = surviveprobeH
  private$feedH                = feedH
  private$surviveZ             = surviveZ
  private$feedZ                = feedZ
  private$PPR_a                = PPR_a
  private$PPR_b                = PPR_b
  private$S_a                  = S_a
  private$S_b                  = S_b
  private$ttsz_p               = ttsz_p
  private$ttsz_a               = ttsz_a
  private$ttsz_b               = ttsz_b
  private$ttr_a                = ttr_a
  private$ttr_b                = ttr_b
  private$chm_a                = chm_a
  private$chm_b                = chm_b
  private$sns_a                = sns_a
  private$sns_b                = sns_b
  private$bm_a                 = bm_a
  private$bm_b                 = bm_b
  private$of_a                 = of_a
  private$of_b                 = of_b
  private$bloodPerEgg          = bloodPerEgg
  private$bs_m                 = bs_m
  private$bs_sd                = bs_sd
  private$maxBatch             = maxBatch
  private$emt_m                = emt_m
  private$emt_sd               = emt_sd
  private$rf_a                 = rf_a
  private$rf_b                 = rf_b

}

# set methods
MBITES_Parameters$set(which = "public",name = "set_parameters",
    value = set_parameters_MBITES_Parameters, overwrite = TRUE
)


###############################################################################
# Accessors
###############################################################################

get_defaultState_F_MBITES_Parameters <- function(){
  return(private$defaultState_F)
}

get_defaultState_M_MBITES_Parameters <- function(){
  return(private$defaultState_M)
}

#' get the string telling you what aquatic ecology model is being used
get_aqua_model_MBITES_Parameters <- function(){
  return(private$aqua_model)
}

#' get dispersal (parameter to move even if resources are present)
get_disperse_MBITES_Parameters <- function(){
  return(private$disperse)
}

#' get 1/number of failed bouts until mosquito gives up and searches
get_boutFail_p_MBITES_Parameters <- function(){
  return(private$boutFail_p)
}

#' MBITES Parameters: Return Resting Spot Weights for Behavioral States
#'
#' Return the specific weights for different resting spots associated with mosquito behavioral states.
#'
get_wts_MBITES_Parameters <- function(state){
  switch(state,
    B = {private$b_wts},
    O = {private$o_wts},
    M = {private$m_wts},
    S = {private$s_wts},
    {stop("illegal behavioral state entered from call to get_wts_MBITES_Parameters")}
  )
}

#' should be a named matrix because indexing is going to be on dimension names.
get_InAndOut_row_MBITES_Parameters <- function(i){
  private$InAndOut[i,]
}

get_tSwarm_MBITES_Parameters <- function(){
  return(private$tSwarm)
}

get_Emax_MBITES_Parameters <- function(){
  return(private$Emax)
}

get_Eb_MBITES_Parameters <- function(){
  return(private$Eb)
}

get_Ep_MBITES_Parameters <- function(){
  return(private$Ep)
}

get_eEndm_MBITES_Parameters <- function(){
  return(private$eEndm)
}

get_eEndSd_MBITES_Parameters <- function(){
  return(private$eEndSd)
}

get_estivationDay_MBITES_Parameters <- function(){
  return(private$estivationDay)
}

get_energyPreG_MBITES_Parameters <- function(){
  return(private$energyPreG)
}

get_preGsugar_MBITES_Parameters <- function(){
  return(private$preGsugar)
}

get_energyFromBlood_b_MBITES_Parameters <- function(){
  return(private$energyFromBlood_b)
}

get_S_u_MBITES_Parameters <- function(){
  return(private$S_u)
}

get_S_sa_MBITES_Parameters <- function(){
  return(private$S_sa)
}

get_S_sb_MBITES_Parameters <- function(){
  return(private$S_sb)
}

get_Bs_surv_MBITES_Parameters <- function(){
  return(private$Bs_surv)
}

get_Os_surv_MBITES_Parameters <- function(){
  return(private$Os_surv)
}

get_Ms_surv_MBITES_Parameters <- function(){
  return(private$Ms_surv)
}

get_Ss_surv_MBITES_Parameters <- function(){
  return(private$Ss_surv)
}

get_B_surv_MBITES_Parameters <- function(){
  return(private$B_surv)
}

get_O_surv_MBITES_Parameters <- function(){
  return(private$O_surv)
}

get_M_surv_MBITES_Parameters <- function(){
  return(private$M_surv)
}

get_S_surv_MBITES_Parameters <- function(){
  return(private$S_surv)
}

get_Bs_succeed_MBITES_Parameters <- function(){
  return(private$Bs_succeed)
}

get_Os_succeed_MBITES_Parameters <- function(){
  return(private$Os_succeed)
}

get_Ms_succeed_MBITES_Parameters <- function(){
  return(private$Ms_succeed)
}

get_Ss_succeed_MBITES_Parameters <- function(){
  return(private$Ss_succeed)
}

get_B_succeed_MBITES_Parameters <- function(){
  return(private$B_succeed)
}

get_O_succeed_MBITES_Parameters <- function(){
  return(private$O_succeed)
}

get_M_succeed_MBITES_Parameters <- function(){
  return(private$M_succeed)
}

get_S_succeed_MBITES_Parameters <- function(){
  return(private$S_succeed)
}

get_surviveH_MBITES_Parameters <- function(){
  return(private$surviveH)
}

get_probeH_MBITES_Parameters <- function(){
  return(private$probeH)
}

get_surviveprobeH_MBITES_Parameters <- function(){
  return(private$surviveprobeH)
}

get_feedH_MBITES_Parameters <- function(){
  return(private$feedH)
}

get_surviveZ_MBITES_Parameters <- function(){
  return(private$surviveZ)
}

get_feedZ_MBITES_Parameters <- function(){
  return(private$feedZ)
}

get_PPR_a_MBITES_Parameters <- function(){
  return(private$PPR_a)
}

get_PPR_b_MBITES_Parameters <- function(){
  return(private$PPR_b)
}

get_S_a_MBITES_Parameters <- function(){
  return(private$S_a)
}

get_S_b_MBITES_Parameters <- function(){
  return(private$S_b)
}

get_ttsz_p_MBITES_Parameters <- function(){
  return(private$ttsz_p)
}

get_ttsz_a_MBITES_Parameters <- function(){
  return(private$ttsz_a)
}

get_ttsz_b_MBITES_Parameters <- function(){
  return(private$ttsz_b)
}

get_ttr_a_MBITES_Parameters <- function(){
  return(private$ttr_a)
}

get_ttr_b_MBITES_Parameters <- function(){
  return(private$ttr_b)
}

get_chm_a_MBITES_Parameters <- function(){
  return(private$chm_a)
}

get_chm_b_MBITES_Parameters <- function(){
  return(private$chm_b)
}

get_sns_a_MBITES_Parameters <- function(){
  return(private$sns_a)
}

get_sns_b_MBITES_Parameters <- function(){
  return(private$sns_b)
}

get_bm_a_MBITES_Parameters <- function(){
  return(private$bm_a)
}

get_bm_b_MBITES_Parameters <- function(){
  return(private$bm_b)
}

get_of_a_MBITES_Parameters <- function(){
  return(private$of_a)
}

get_of_b_MBITES_Parameters <- function(){
  return(private$of_b)
}

get_bloodPerEgg_MBITES_Parameters <- function(){
  return(private$bloodPerEgg)
}

get_bs_m_MBITES_Parameters <- function(){
  return(private$bs_m)
}

get_bs_sd_MBITES_Parameters <- function(){
  return(private$bs_sd)
}

get_maxBatch_MBITES_Parameters <- function(){
  return(private$maxBatch)
}

get_emt_m_MBITES_Parameters <- function(){
  return(private$emt_m)
}

get_emt_sd_MBITES_Parameters <- function(){
  return(private$emt_sd)
}

get_rf_a_MBITES_Parameters <- function(){
  return(private$rf_a)
}

get_rf_b_MBITES_Parameters <- function(){
  return(private$rf_b)
}

# set methods
MBITES_Parameters$set(which = "public",name = "get_defaultState_F",
    value = get_defaultState_F_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_defaultState_M",
    value = get_defaultState_M_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_aqua_model",
    value = get_aqua_model_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_disperse",
    value = get_disperse_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_boutFail_p",
    value = get_boutFail_p_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_wts",
    value = get_wts_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_InAndOut_row",
    value = get_InAndOut_row_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_tSwarm",
    value = get_tSwarm_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Emax",
    value = get_Emax_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Eb",
    value = get_Eb_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Ep",
    value = get_Ep_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_eEndm",
    value = get_eEndm_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_eEndSd",
    value = get_eEndSd_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_estivationDay",
    value = get_estivationDay_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_energyPreG",
    value = get_energyPreG_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_preGsugar",
    value = get_preGsugar_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_energyFromBlood_b",
    value = get_energyFromBlood_b_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_S_u",
    value = get_S_u_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_S_sa",
    value = get_S_sa_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_S_sb",
    value = get_S_sb_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Bs_surv",
    value = get_Bs_surv_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Os_surv",
    value = get_Os_surv_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Ms_surv",
    value = get_Ms_surv_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Ss_surv",
    value = get_Ss_surv_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_B_surv",
    value = get_B_surv_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_O_surv",
    value = get_O_surv_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_M_surv",
    value = get_M_surv_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_S_surv",
    value = get_S_surv_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Bs_succeed",
    value = get_Bs_succeed_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Os_succeed",
    value = get_Os_succeed_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Ms_succeed",
    value = get_Ms_succeed_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_Ss_succeed",
    value = get_Ss_succeed_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_B_succeed",
    value = get_B_succeed_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_O_succeed",
    value = get_O_succeed_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_M_succeed",
    value = get_M_succeed_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_S_succeed",
    value = get_S_succeed_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_surviveH",
    value = get_surviveH_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_probeH",
    value = get_probeH_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_surviveprobeH",
    value = get_surviveprobeH_MBITES_Parameters, overwrite = TRUE
)


MBITES_Parameters$set(which = "public",name = "get_feedH",
    value = get_feedH_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_surviveZ",
    value = get_surviveZ_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_feedZ",
    value = get_feedZ_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_PPR_a",
    value = get_PPR_a_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_PPR_b",
    value = get_PPR_b_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_S_a",
    value = get_S_a_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_S_b",
    value = get_S_b_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_ttsz_p",
    value = get_ttsz_p_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_ttsz_a",
    value = get_ttsz_a_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_ttsz_b",
    value = get_ttsz_b_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_ttr_a",
    value = get_ttr_a_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_ttr_b",
    value = get_ttr_b_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_chm_a",
    value = get_chm_a_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_chm_b",
    value = get_chm_b_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_sns_a",
    value = get_sns_a_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_sns_b",
    value = get_sns_b_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_bm_a",
    value = get_bm_a_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_bm_b",
    value = get_bm_b_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_of_a",
    value = get_of_a_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_of_b",
    value = get_of_b_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_bloodPerEgg",
    value = get_bloodPerEgg_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_bs_m",
    value = get_bs_m_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_bs_sd",
    value = get_bs_sd_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_maxBatch",
    value = get_maxBatch_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_emt_m",
    value = get_emt_m_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_emt_sd",
    value = get_emt_sd_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_rf_a",
    value = get_rf_a_MBITES_Parameters, overwrite = TRUE
)

MBITES_Parameters$set(which = "public",name = "get_rf_b",
    value = get_rf_b_MBITES_Parameters, overwrite = TRUE
)


###############################################################################
# assign MBITES parameters instance in the package namespace (a bit hacky)
###############################################################################

Parameters <- MBITES_Parameters$new()
rspot <- c("i","w","v","r","l")
