###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Setup Simulation
#     MBITES Team
#     March 2018
#
###############################################################################

#' MBITES: Setup
#'
#' Setup routines should be called prior to making any actual objects (ie; before calling any initialization functions, such as \code{\link{MBITES_Initialize}}, \code{link{Tile_Initialize}}, etc).
#' The setup function is responsible for assigning concrete methods to proper interfaces in the \code{\link{Mosquito}}, \code{\link{Mosquito_Female}}, and \code{\link{Mosquito_Male}}
#' classes as well as setting up the time-to-event closures in the global \code{\link{MBITES_Parameters}} object needed to sample waiting times to next launch.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @name MBITES Setup
NULL
#> NULL

###############################################################################
# Initialize MBITES model
###############################################################################

#' MBITES: Setup Model
#'
#' See \code{\link{MBITES Setup}} for more details. Note that because of lazy evaluation of function arguments only arguments that are
#' needed for the specific models chosen need to be provided as input by the user.
#'
#' @details
#' Arguments for the setup function are listed below:
#'
#' **MBITES-Timing**: see \code{\link{MBITES-Timing}}
#'  * timing_model: integer flag for attempt & search bout inter-launch time-to-event sampling distribution. All timing models require further arguments which are given below their main heading.
#'    * 1: deterministic waiting time, see \code{\link{make_ttEvent_Det}} for more details (if selecting this model please enter required arguments below)
#'      * wait_b: deterministic waiting time to next launch for blood feeding attempt bout
#'      * wait_o: deterministic waiting time to next launch for oviposition attempt bout
#'      * wait_m: deterministic waiting time to next launch for mating attempt bout
#'      * wait_s: deterministic waiting time to next launch for sugar feeding attempt bout
#'      * wait_bs: deterministic waiting time to next launch for blood feeding search bout
#'      * wait_os: deterministic waiting time to next launch for oviposition search bout
#'      * wait_ms: deterministic waiting time to next launch for mating search bout
#'      * wait_ss: deterministic waiting time to next launch for sugar feeding search bout
#'    * 2: shifted exponential distribution, see \code{\link{make_ttEvent_Exp}} for more details (if selecting this model please enter required arguments below)
#'      * rate_b: inverse of average waiting time to next launch for blood feeding attempt bout
#'      * tmin_b: minimum waiting time prior to next launch for blood feeding attempt bout
#'      * rate_o: inverse of average waiting time to next launch for oviposition attempt bout
#'      * tmin_o: minimum waiting time prior to next launch for oviposition attempt bout
#'      * rate_m: inverse of average waiting time to next launch for mating attempt bout
#'      * tmin_m: minimum waiting time prior to next launch for mating attempt bout
#'      * rate_s: inverse of average waiting time to next launch for sugar feeding attempt bout
#'      * tmin_s: minimum waiting time prior to next launch for sugar feeding attempt bout
#'      * rate_bs: inverse of average waiting time to next launch for blood feeding search bout
#'      * tmin_bs: minimum waiting time prior to next launch for blood feeding search bout
#'      * rate_os: inverse of average waiting time to next launch for oviposition search bout
#'      * tmin_os: minimum waiting time prior to next launch for oviposition search bout
#'      * rate_ms: inverse of average waiting time to next launch for mating search bout
#'      * tmin_ms: minimum waiting time prior to next launch for mating search bout
#'      * rate_ss: inverse of average waiting time to next launch for sugar feeding search bout
#'      * tmin_ss: minimum waiting time prior to next launch for sugar feeding search bout
#'    * 3: shifted gamma distribution, note that minimum waiting time arguments still need to be provided, as given above, see \code{\link{make_ttEvent_Gamma}} for more details (if selecting this model please enter required arguments below). Note that this model requires \code{tmin_x} to be specified.
#'      * mean_b: inverse of average waiting time to next launch for blood feeding attempt bout
#'      * cv_b: coefficient of variation between mean and variance of waiting time
#'      * mean_o: inverse of average waiting time to next launch for oviposition attempt bout
#'      * cv_o: coefficient of variation between mean and variance of waiting time
#'      * mean_m: inverse of average waiting time to next launch for mating attempt bout
#'      * cv_m: coefficient of variation between mean and variance of waiting time
#'      * mean_s: inverse of average waiting time to next launch for sugar feeding attempt bout
#'      * cv_s: coefficient of variation between mean and variance of waiting time
#'      * mean_bs: inverse of average waiting time to next launch for blood feeding search bout
#'      * cv_bs: coefficient of variation between mean and variance of waiting time
#'      * mean_os: inverse of average waiting time to next launch for oviposition search bout
#'      * cv_os: coefficient of variation between mean and variance of waiting time
#'      * mean_ms: inverse of average waiting time to next launch for mating search bout
#'      * cv_ms: coefficient of variation between mean and variance of waiting time
#'      * mean_ss: inverse of average waiting time to next launch for sugar feeding search bout
#'      * cv_ss: coefficient of variation between mean and variance of waiting time
#'    * 4: shifted diurnal (sinusoidal) distribution, see \code{\link{make_ttEvent_Diurnal}} (if selecting this model please enter required arguments below)
#'      * not implemented yet
#'  * ppr_model: integer flag for post-prandial resting length sampling distribution All timing models require further arguments which are given below their main heading.
#'    * 1: deterministic waiting time, see \code{\link{make_ttEvent_Det}} for more details (if selecting this model please enter required arguments below)
#'      * wait_ppr: deterministic length of post-prandial resting bout
#'    * 2: shifted exponential distribution, see \code{\link{make_ttEvent_Exp}} for more details (if selecting this model please enter required arguments below)
#'      * rate_ppr: inverse of average length of post-prandial resting bout
#'      * tmin_ppr: minimum time of post-prandial resting bout
#'    * 3: shifted gamma distribution, note that minimum waiting time arguments still need to be provided, as given above, see \code{\link{make_ttEvent_Gamma}} for more details (if selecting this model please enter required arguments below). Note that this model requires \code{tmin_ppr} to be specified.
#'      * mean_ppr: inverse of average length of post-prandial resting bout
#'      * cv_ppr: coefficient of variation between mean and variance of waiting time
#'  * estivation_model: integer flag for the estivation model
#'    * 1: estivation turned off, see \code{\link{mbites_checkEstivationNull}}
#'    * 2: probabilistic estivation model, see \code{\link{mbites_checkEstivation1}}
#'      * Emax: onset of dry season/period of estivation; default 90, see \code{\link{mbites_prEstivate}}
#'      * Eb: scaling parameter for \code{\link{mbites_prEstivate}}; default 0.9
#'      * Ep: probability to survive estivation; default 0.5
#'      * eEndm: mean wake up day; default 180
#'      * eEndSd: standard deviation of wake up day; default 30
#'    * 3: hard cut-off estivation model, see \code{\link{mbites_checkEstivation2}}
#'      * estivationDay: a calendar day to start estivation
#'  * mating: boolean flag for mating behavior
#'    * tSwarm: time of day of mating swarm formation (bounded by [0,1], eg; noon is 12/24)
#'
#' **MBITES-BloodMeal**: see \code{\link{MBITES-BloodMeal}}
#'  * bm_a: alpha parameter of beta-distributed blood meal size
#'  * bm_b: beta parameter of beta-distributed blood meal size
#'  * overfeeding: logical flag for overfeeding behavior (increased mortality from blood feeding)
#'    * of_a: parameter in \code{\link{mbites_pOverFeed}} (probability of death from blood meal size)
#'    * of_b: parameter in \code{\link{mbites_pOverFeed}} (probability of death from blood meal size)
#'
#' **MBITES-Oogenesis**: see\code{\link{MBITES-Oogenesis}}
#'  * oogenesis_model: integer flag for model of oogenesis (egg production & development)
#'    * 1: egg batch size proportional to blood meal size (see help file for \code{MBITES-Oogenesis})
#'      * eggMaturationTime: logical flag, if \code{TRUE} use \code{\link{mbites_rEggMaturationTimeNorm}} to sample egg maturation times, otherwise use the null filler \code{\link{mbites_rEggMaturationTimeOff}}
#'        * emt_m: mean of Gaussian distributed egg maturation time
#'        * emt_sd: standard deviation of Gaussian distributed egg maturation time
#'    * 2: egg batch size commits to development (see help file for \code{MBITES-Oogenesis})
#'        * bloodPerEgg: amount of blood needed per egg
#'  * eggsize_model: integer flag for model of egg batch size
#'    * 1: sample Gaussian-distributed egg batch size (see \code{\link{mbites_rBatchSizeNorm}})
#'        * bs_m: mean of Gaussian distribution
#'        * bs_sd: standard deviation of Gaussian distribution
#'    * 2: egg batch size is function of blood meal size (see \code{\link{mbites_rBatchSizeBms}})
#'        * maxBatch: maximum possible size of an egg batch
#'  * refeeding: logical flag for refeeding behavior (this should only be used with the first model of oogenesis, see \code{\link{mbites_checkRefeed}} for details)
#'    * rf_a: parameter in \code{\link{mbites_pReFeed}}
#'    * rf_b: parameter in \code{\link{mbites_pReFeed}}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @export
MBITES_Setup <- function(

  #################
  # MBITES-Timing #
  #################

  # attempt & search bout tte
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
  cv_ss = NULL,
  # diurnal
  # ppr tte
  ppr_model = 1L,
  # det
  wait = NULL,
  # exp
  rate_ppr = NULL,
  tmin_ppr = NULL,
  # gamma
  mean_ppr = NULL,
  cv_ppr = NULL,

  # Estivation model
  estivation_model = 1L,
  Emax = NULL,
  Eb = NULL,
  Ep = NULL,
  eEndm = NULL,
  eEndSd = NULL,
  estivationDay = NULL,

  # swarming
  mating = FALSE,
  tSwarm = NULL,

  ####################
  # MBITES-BloodMeal #
  ####################

  # blood meal size
  bm_a = NULL,
  bm_b = NULL,

  # overfeeding
  overfeeding = FALSE,
  of_a = NULL,
  of_b = NULL,

  ####################
  # MBITES-Oogenesis #
  ####################
  oogenesis_model = 1L,
  eggMaturationTime = FALSE,
  emt_m = NULL,
  emt_sd = NULL,
  bloodPerEgg = NULL,
  eggsize_model = 1L,
  bs_m = NULL,
  bs_sd = NULL,
  maxBatch = NULL,
  refeeding = FALSE,
  rf_a = NULL,
  rf_b = NULL,

  blank = NULL
){

  # set flag
  MBITES:::Globals$set_SETUP(TRUE)

  #################
  # MBITES-Timing #
  #################

  # attempt & search bout tte
  MBITES:::Parameters$set_ttEvent(
    timing_model,
    # deterministic
    wait_b,
    wait_o,
    wait_m,
    wait_s,
    wait_bs,
    wait_os,
    wait_ms,
    wait_ss,
    # exp
    rate_b,
    tmin_b,
    rate_o,
    tmin_o,
    rate_m,
    tmin_m,
    rate_s,
    tmin_s,
    rate_bs,
    tmin_bs,
    rate_os,
    tmin_os,
    rate_ms,
    tmin_ms,
    rate_ss,
    tmin_ss,
    # gamma
    mean_b,
    cv_b,
    tmin_b,
    mean_o,
    cv_o,
    tmin_o,
    mean_m,
    cv_m,
    tmin_m,
    mean_s,
    cv_s,
    tmin_s,
    mean_bs,
    cv_bs,
    tmin_bs,
    mean_os,
    cv_os,
    tmin_os,
    mean_ms,
    cv_ms,
    tmin_ms,
    mean_ss,
    cv_ss,
    tmin_ss
  )
  # ppr tte
  MBITES:::Parameters$set_ttEvent_ppr(
    ppr_model,
    wait_ppr,
    rate_ppr,
    tmin_ppr,
    mean_ppr,
    cv_ppr
  )

  # Estivation
  switch(estivation_model,
    # no estivation
    "1" = {
      Mosquito_Female$set(which = "public",name = "checkEstivation",
                value = mbites_checkEstivationNull, overwrite = TRUE
      )

      Mosquito_Female$set(which = "public",name = "prEstivate",
                value = mbites_prEstivate, overwrite = TRUE
      )

      Mosquito_Female$set(which = "public",name = "wakeUpTime",
                value = mbites_wakeUpTime, overwrite = TRUE
      )
    },
    # probabilistic estivation
    "2" = {
      Mosquito_Female$set(which = "public",name = "checkEstivation",
                value = mbites_checkEstivation1, overwrite = TRUE
      )
    },
    # hard cut-off estivation
    "3" = {
      Mosquito_Female$set(which = "public",name = "checkEstivation",
                value = mbites_checkEstivation2, overwrite = TRUE
      )
      # need to set the ttEvent$Estivate function here.
    },
    {stop("invalid entry for 'estivation_model'")}
  )

  # Swarming
  if(mating){
    Mosquito_Female$set(which = "public",name = "findSwarm",
              value = mbites_findSwarm, overwrite = TRUE
    )
  } else {
    Mosquito_Female$set(which = "public",name = "findSwarm",
              value = mbites_findSwarmNull, overwrite = TRUE
    )
  }

  ####################
  # MBITES-BloodMeal #
  ####################

  if(overfeeding){
    # set methods
    Mosquito_Female$set(which = "public",name = "Overfeeding",
        value = mbites_Overfeeding, overwrite = TRUE
    )
    Mosquito_Female$set(which = "public",name = "pOverFeed",
        value = mbites_pOverFeed, overwrite = TRUE
    )
  } else {
    Mosquito_Female$set(which = "public",name = "Overfeeding",
        value = mbites_OverfeedingNull, overwrite = TRUE
    )
  }

  ####################
  # MBITES-Oogenesis #
  ####################

  # rBatchSize
  switch(eggsize_model,
    "1" = {
      Mosquito_Female$set(which = "public",name = "rBatchSize",
          value = mbites_rBatchSizeNorm, overwrite = TRUE
      )
    },
    "2" = {
      Mosquito_Female$set(which = "public",name = "rBatchSize",
          value = mbites_rBatchSizeBms, overwrite = TRUE
      )
    },
    {stop("invalid entry for 'eggsize_model'\n")}
  )

  switch(oogenesis_model,
    ###########
    # model 1 #
    ###########
    "1" = {

      # model
      Mosquito_Female$set(which = "public",name = "oogenesis",
                value = mbites_oogenesis1, overwrite = TRUE
      )

      # egg maturation
      if(eggMaturationTime){
        Mosquito_Female$set(which = "public",name = "rEggMaturationTime",
                  value = mbites_rEggMaturationTimeNorm, overwrite = TRUE
        )
      } else {
        Mosquito_Female$set(which = "public",name = "rEggMaturationTime",
                  value = mbites_rEggMaturationTimeOff, overwrite = TRUE
        )
      }

      # refeeding
      Mosquito_Female$set(which = "public",name = "checkRefeed",
                value = mbites_checkRefeed, overwrite = TRUE
      )

      Mosquito_Female$set(which = "public",name = "checkEggMaturation",
                value = mbites_checkEggMaturation, overwrite = TRUE
      )

      if(refeeding){
        Mosquito_Female$set(which = "public",name = "pReFeed",
                  value = mbites_pReFeed, overwrite = TRUE
        )
      } else {
        Mosquito_Female$set(which = "public",name = "pReFeed",
                  value = mbites_pReFeed_null, overwrite = TRUE
        )
      }
    },
    ###########
    # model 2 #
    ###########
    "2" = {
      # model
      Mosquito_Female$set(which = "public",name = "oogenesis",
                value = mbites_oogenesis2, overwrite = TRUE
      )
      # egg provision field
      Mosquito_Female$set(which = "private",name = "eggP",
                value = numeric(1), overwrite = TRUE
      )

      # refeeding
      Mosquito_Female$set(which = "public",name = "checkRefeed",
                value = mbites_checkRefeed_null, overwrite = TRUE
      )

    },
    {stop("invalid entry for 'oogenesis_model'\n")}
  )

}
