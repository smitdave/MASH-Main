###############################################################################
#         __  _______  ____  _______________________
#        /  |/  / __ )/ __ \/ ____/_  __/ ____/ ___/
#       / /|_/ / __  / / / / __/   / / / __/  \__ \
#      / /  / / /_/ / /_/ / /___  / / / /___ ___/ /
#     /_/  /_/_____/_____/_____/ /_/ /_____//____/
#
#     MBDETES - Evaluate Parameters from a Landscape
#     MBITES Team
#     June 2018
#
###############################################################################


###############################################################################
# Interface to users
###############################################################################

#' MBDETES: Calculate State Transitions Matrix
#'
#' For each \code{\link{Site}} in a landscape, calculate approximate
#' state transitions matrix by calling \code{\link{MBDETES_StateTransitions}}.
#'
#' @param tileID an integer ID of a tile (must have already called \code{\link{Tile_Initialize}} and the appropriate \code{Human_XX_Initialize} routines)
#' @export
MBDETES_Approx <- function(tileID){

  # make sure tile exists
  if(tileID > MBITES:::Globals$get_n_tiles()){
    stop("argument 'tileID' must refer to a valid tile\n")
  }

  # get tile reference
  tile = MBITES:::Globals$get_tile(tileID)
  n = tile$get_sites()$size()

  tile$get_sites()$apply(tag="clear_ActivitySpace")
  tile$get_humans()$apply(tag="oneDay_ActivitySpace")

  # calculate transitions for each site
  out = vector(mode="list",length=n)
  pb = txtProgressBar(min = 0,max = n)
  for(i in 1:n){

    site = tile$get_site(i)
    out[[i]] = MBDETES_StateTransitions(site)

    setTxtProgressBar(pb,i)
  }

  tile$get_sites()$apply(tag="clear_ActivitySpace")

  return(out)
}


###############################################################################
# BloodMeal and Oogenesis probabilities (inc. PPRFlight survival)
###############################################################################

#' MBDETES: Refeeding Probability
#'
#' Computes the integral of the probability of
#' feeding conditioned on the size of the bloodmeal,
#' multiplied by the probability of bloodmeal
#' of that size.
#'  * MBITES functions can be found in MBITES-Oogenesis.R
#'
#' @export
MBDETES_getPrRefeed <- function(){
  rf_a = MBITES:::Parameters$get_rf_a()
  rf_b = MBITES:::Parameters$get_rf_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()

  FF = function(X){
    dbeta(X,bm_a,bm_b)*(2+rf_b)/(1+rf_b) - exp(rf_a*X)/(rf_b + exp(rf_a*X))
  }
  integrate(FF, 0, 1)$value
}

#' MBDETES: Overfeeding Probability
#'
#' Computes the integral of the probability of
#' overfeeding conditioned on the size of the bloodmeal,
#' multiplied by the probability of bloodmeal
#' of that size.
#'  * MBITES functions can be found in MBITES-BloodMeal.R
#'
#' @export
MBDETES_getPrOverfeed <- function(){
  of_a = MBITES:::Parameters$get_of_a()
  of_b = MBITES:::Parameters$get_of_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()

  FF = function(X){
    dbeta(X,bm_a,bm_b)*exp(of_a*X)/(of_b + exp(of_a*X))
  }
  integrate(FF, 0, 1)$value
}

# probability to die from post-prandial period (1 - pPPR = survival prob)
MBDETES_getPrPPRFlight <- function(){
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()

  FF = function(X){
    dbeta(X,bm_a,bm_b)*exp(PPR_a*X)/(PPR_b + exp(PPR_a*X))
  }
  integrate(FF, 0, 1)$value
}

#' MBDETES: Probability of Surviving the post-prandial flight (laden mosquito)
#'
#'
#' @export
MBDETES_getSurvLaden <- function(){
  lrl = MBDETES_getRestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()
  FF = function(X){
     p = dbeta(X,bm_a,bm_b)*exp(PPR_a*X)/(PPR_b + exp(PPR_a*X))
     p*(land+leave)/(1-retry)
  }
  integrate(FF, 0, 1)$value
}

#' MBDETES: Probability of Leaving, post prandially (laden mosquito)
#'
#'
#' @export
MBDETES_getLeaveLaden <- function(){
  lrl = MBDETES_getRestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()
  FF = function(X){
     p = dbeta(X,bm_a,bm_b)*exp(PPR_a*X)/(PPR_b + exp(PPR_a*X))
     p*leave/(1-retry)
  }
  integrate(FF, 0, 1)$value
}

#' MBDETES: Probability of Surviving the post-prandial flight (unladen mosquito)
#'
#'
#' @export
MBDETES_getSurvUnladen <- function(){
  lrl = MBDETES_getRestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]
  (land+leave)/(1-retry)
}

#' MBDETES: Probability of Leaving, post prandially (unladen mosquito)
#'
#'
#' @export
MBDETES_getLeaveUnladen <- function(){
  lrl = MBDETES_getRestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]
  leave/(1-retry)
}

#' MBDETES: Resting spot probabilities
#'
#' Calculate resting spot probabilities according to a mixture distribution with
#' equal weights given to the 3 main behavioral states (B,O,S).
#'
#' @export
MBDETES_getRestingParam <- function(){
  # if(private$site$get_type()==1L){
    # use "l" because that is the rspot when a mosquito arrives at a new site
    mix_wts = rep(1,3)/3
    inandout = MBITES:::Parameters$get_InAndOut_row("l")
    probs = mix_wts[1]*(inandout* MBITES:::Parameters$get_wts("B")) +
            mix_wts[2]*(inandout* MBITES:::Parameters$get_wts("O")) +
            mix_wts[3]*(inandout* MBITES:::Parameters$get_wts("S"))

     # probs = MBITES:::Parameters$get_InAndOut_row("l") * MBITES:::Parameters$get_wts("l")
     land = sum(probs[1:3])
     retry = probs[4]
     leave = probs[5]
  # not homestead
  # } else {
  #    return("v")
  # }
  out = unname(c(land, retry, leave))
  return(out/sum(out))
}


###############################################################################
# State Transitions
###############################################################################

#' MBDETES: Blood Feeding Search Bout State Transitions
#'
#' Calculate vector of probabilities to transition between: F -> F, B, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_FstateTransitions <- function(site){
  succeed = MBITES:::Parameters$get_Bs_succeed()
  survive = MBITES:::Parameters$get_Bs_surv()

  F2B = succeed*survive
  F2F = (1-succeed)*survive
  F2D = 1-survive

  return(c(F2F, F2B, 0, 0, 0, F2D))
}


#' MBDETES: Blood Feeding Attempt Bout State Transitions
#'
#' Calculate vector of probabilities to transition between: B -> F, B, R, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_BstateTransitions <- function(site){

  # Does the mosquito even make it to an initial approach?
  approach = MBITES:::Parameters$get_B_succeed()
  survive = MBITES:::Parameters$get_B_surv()

  # check the function
  if(site$has_feed()){
    host = site$get_feed(1L)$RiskQ$typewtsQ()
  } else {
    host = rep(0,4)
  }

  # probability of taking a human bloodmeal | human chosen
  h1 = MBITES:::Parameters$get_probeH()
  h2 = MBITES:::Parameters$get_surviveprobeH()
  h3 = MBITES:::Parameters$get_feedH()
  hfeed = h1*h2*h3
  hfail = (1-h1)+h1*h2*(1-h3)

  # probability of taking a bloodmeal | other host chosen
  z1 = MBITES:::Parameters$get_surviveZ()
  z2 = MBITES:::Parameters$get_feedZ()
  zfeed = z1*z2
  zfail = z1*(1-z2)

  # probability of failing | trap chosen
  tfail = 0

  # survive an unladen flight, stay
  surviveUnladen = MBDETES_getSurvUnladen()
  stayUnladen = 1 - MBDETES_getLeaveUnladen()

  # survive a laden flight, stay
  surviveLaden = MBDETES_getSurvLaden()

  ##########################################################
  # fail the approach
  # note :: check hfeed and zfeed above for more fails
  ##########################################################
  failApproach = host[1]*hfail+host[2]*zfail+host[3]*tfail+host[4]
  bFeed = host[1]*hfeed+host[2]*zfeed

  B2R = approach*bFeed*surviveLaden
  B2B = approach*failApproach*surviveUnladen*stayUnladen*survive
  B2F1 = (1-approach)*surviveUnladen*stayUnladen*survive
  B2F2 = approach*failApproach*surviveUnladen*(1-stayUnladen)*survive
  B2F = B2F1 + B2F2
  B2D = 1-B2R-B2B-B2F
  return(c(B2F, B2B, B2R, 0, 0, B2D))
}


#' MBDETES: Resting Period State Transitions
#'
#' Calculate vector of probabilities to transition between: R-> B, F, L, O, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_RperiodTransitions <- function(site){

  # The probability of refeeding
  refeed = MBDETES_getPrRefeed()

  # stay, after a laden flight
  stayLaden = MBDETES_getLeaveLaden()

  # aquatic habitat present
  aquatic = site$has_aqua()

  # blood host present
  blood = site$has_feed()

  # survive the post-prandial resting period
  surviveRest = 1 - MBDETES_getPrPPRFlight() # FIX ME !!!

  R2B = blood*refeed*surviveRest
  R2F = (1-blood)*refeed*surviveRest
  R2L = (1-aquatic)*(1-refeed)*surviveRest
  R2O = aquatic*(1-refeed)*surviveRest
  R2D = 1-R2F-R2B-R2L-R2O
  return(c(R2F, R2B, 0, R2L, R2O, R2D))
}


#' MBDETES: Egg Laying Search Bout State Transitions
#'
#' Calculate vector of probabilities to transition between: L -> L, O, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_LstateTransitions <- function(site){
  success = MBITES:::Parameters$get_Os_succeed()
  survive = MBITES:::Parameters$get_Os_surv()

  L2O = succeed*survive
  L2L = (1-succeed)*survive
  L2D = 1-L2O-L2L

  return(c(0, 0, 0, L2L, L2O, L2D))
}


#' MBDETES: Egg Laying Attempt Bout State Transitions
#'
#' Calculate vector of probabilities to transition between: O -> F, B, L, O, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_OstateTransitions <- function(site){
  success = MBITES:::Parameters$get_O_succeed()
  survive = MBITES:::Parameters$get_O_surv()
  blood   = site$has_feed()
  stay    = 1-getLeaveUnladen()

  O2F = succeed*survive*(1-blood)
  O2B = succeed*survive*blood
  O2O = (1-succeed)*survive*stay
  O2L = (1-succeed)*survive*(1-stay)
  O2D = 1-O2F-O2B-O2O-O2L-O2D
  return(c(O2F, O2B, 0, O2L, O2O, O2D))
}


#' MBDETES: The State Transition Matrix
#'
#' Calculate state transitions matrix for one \code{\link{Site}} object.
#' Transitions are of form FBRLO to FBRLOD (the rows sum to one).
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_StateTransitions <- function(site){
  FBRLOD = matrix(0,nrow=6,ncol=6,dimnames=list(c("F","B","R","L","O","D"),c("F","B","R","L","O","D")))
  FBRLOD[1,] = MBDETES_FstateTransitions(site)
  FBRLOD[2,] = MBDETES_BstateTransitions(site)
  FBRLOD[3,] = MBDETES_RperiodTransitions(site)
  FBRLOD[4,] = MBDETES_LstateTransitions(site)
  FBRLOD[5,] = MBDETES_OstateTransitions(site)
  return(FBRLOD)
}
