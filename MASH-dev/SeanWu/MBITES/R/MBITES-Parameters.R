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
                   ttEvent              = list(
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

                   # Post-bout Landing, House Entering, and Resting
                   boutFail_p           = integer(1), # 1/number of failed bouts until mosquito gives up and searches
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


get_aqua_model_MBITES_Parameters <- function(){
  return(private$aqua_model)
}

MBITES_Parameters$set(which = "public",name = "get_aqua_model",
    value = get_aqua_model_MBITES_Parameters, overwrite = TRUE
)



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
      self$ttEvent$BoutO <- make_ttEvent_Exp(wait_o)
      self$ttEvent$BoutM <- make_ttEvent_Exp(wait_m)
      self$ttEvent$BoutS <- make_ttEvent_Exp(wait_s)
      # search bouts
      self$ttEvent$BoutBs <- make_ttEvent_Exp(wait_bs)
      self$ttEvent$BoutOs <- make_ttEvent_Exp(wait_os)
      self$ttEvent$BoutMs <- make_ttEvent_Exp(wait_ms)
      self$ttEvent$BoutSs <- make_ttEvent_Exp(wait_ss)
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
      self$ttEvent$ppr <- make_ttEvent_Det(wait_ppr)
    },
    "2" = {
      self$ttEvent$ppr <- make_ttEvent_Exp(rate_ppr,tmin_ppr)
    },
    "3" = {
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
# Queries from Mosquitoes
###############################################################################

#' MBITES Parameters: Return Resting Spot Weights for Behavioral States
#'
#' Return the specific weights for different resting spots associated with mosquito behavioral states.
#'
get_wts_MBITES_Parameters <- function(state){
  switch(private$state,
    b = {private$b_wts},
    o = {private$o_wts},
    m = {private$m_wts},
    s = {private$s_wts},
    {stop("illegal behavioral state entered from call to get_wts_MBITES_Parameters")}
  )
}

###############################################################################
# Set Parameters
###############################################################################

# # set global parameters
# set_parameters_MBITES_Parameters <- function(
#
# ){
#
#   # set timing closures
#
#
# }


# should be a named matrix because indexing is going to be on dimension names.
get_InAndOut_row_MBITES_Parameters <- function(i){
  private$InAndOut[i,]
}


###############################################################################
# assign MBITES parameters instance in the package namespace (a bit hacky)
###############################################################################

Parameters <- MBITES_Parameters$new()
rspot <- c("i","w","v","r","l")
