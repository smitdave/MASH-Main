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

                   # time to event closures (must be given implementations before simulation starts)
                   ttEvent_BoutF = function(){stop("ttEvent_BoutF requires a concrete implementation!")},
                   ttEvent_BoutO = function(){stop("ttEvent_BoutO requires a concrete implementation!")},
                   ttEvent_BoutS = function(){stop("ttEvent_BoutO requires a concrete implementation!")},

                   ttEvent_BoutBs = function(){stop("ttEvent_BoutBs requires a concrete implementation!")},
                   ttEvent_BoutOs = function(){stop("ttEvent_BoutOs requires a concrete implementation!")},
                   ttEvent_BoutSs = function(){stop("ttEvent_BoutSs requires a concrete implementation!")},

                   ttEvent_Estivate = function(){stop("ttEvent_Estivate requires a concrete implementation!")}

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

#' Make a Shifted Exponential Time-to-Event Closure
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
    tmin + rexp(n=1L,rate=rate)
  }

  return(tteShiftExp)
}

#' Make a Shifted Gamma Time-to-Event Closure
#'
#' Generates a closure that contains the following fields:
#'  * mean: numeric value of mean
#'  * cv: numeric value of coefficient of variation
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
    tmin + rgamma(n=1L, shape=shape, scale = scale)
  }

  return(tteShiftGamma)
}

#' Make a Shifted Diurnal Time-to-Event Closure
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
