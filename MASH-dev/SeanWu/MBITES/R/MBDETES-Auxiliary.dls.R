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
#' @return a list of length equal to number of sites in the tile \code{tileID} where
#'         each element is a named matrix giving transition probabilities between MBDETES
#'         states {F,B,R,L,O,D}
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
# Refeeding, Overfeeding, Survival
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

#' MBDETES: Probability to Survive Post-prandial Resting Flight
#'
#'
#' @export
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

  inandout = MBITES:::Parameters$get_InAndOut()
  inandout = inandout/rowSums(inandout)
  inandout_eigen = eigen(t(inandout))
  inandout_stationary = inandout_eigen$vectors[,1]/sum(inandout_eigen$vectors[,1])

  mix_wts = rep(1,2)/2
  probs = mix_wts[1]*(inandout_stationary* MBITES:::Parameters$get_wts("B")) +
          mix_wts[2]*(inandout_stationary* MBITES:::Parameters$get_wts("O"))

  land = sum(probs[1:3])
  retry = probs[4]
  leave = probs[5]

  out = unname(c(land, retry, leave))
  return(out/sum(out))
}

#' MBDETES: Probability of Death from Local (Site-specific) Hazards
#'
#' Mortality due to local hazards is a Bernoulli event (see \code{\link{mbites_surviveHazards}})
#' so this function just returns the probability of death.
#'
#' @export
MBDETES_getLocalHazMortality <- function(site){
  return(site$get_haz())
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

  # additional mass on D from local hazards
  localHaz = MBDETES_getLocalHazMortality(site)
  F2D = F2D + localHaz

  # normalize
  F2ALL = c(F2F, F2B, 0, 0, 0, F2D)
  F2ALL = F2ALL/sum(F2ALL)

  return(F2ALL)
}


#' MBDETES: Blood Feeding Attempt Bout State Transitions
#'
#' Calculate vector of probabilities to transition between: B -> F, B, R, D
#' This function is called from \code{\link{MBDETES_StateTransitions}}.
#'
#' @param site a \code{\link{Site}} object
#' @export
MBDETES_BstateTransitions <- function(site){

  # Does the mosquito choose  
  A = MBITES:::Parameters$get_B_succeed()


  # check the function
  if(site$has_feed()){
    host = site$get_feed(1L)$RiskQ$typewtsQ()
    host = host/sum(host)
  } else {
    host = c(0,0,0,1) 
  }
  B1=host[1]
  B2=host[2]
  B3=host[3]
  B4=host[4]

  # probability of taking a human bloodmeal | human chosen
  h1 = MBITES:::Parameters$get_surviveH()
  h2 = MBITES:::Parameters$get_probeH()
  C1 = 1-h1
  C2 = h1*h2
  C3 = h1*(1-h2)  


  h3 = MBITES:::Parameters$get_surviveprobeH()
  h4 = MBITES:::Parameters$get_feedH()
  D1 = 1-h3
  D2 = h3*h4
  D3 = h3*(1-h4) 

  # probability of taking a bloodmeal | other host chosen
  z1 = MBITES:::Parameters$get_surviveZ()
  z2 = MBITES:::Parameters$get_feedZ()
  C4 = 1-z1  
  C5 = z1*z2 
  C6 = z1*(1-z2)  

  # probability of failing | trap chosen
  C7=0.5
  C8=0.5

  # survive an unladen flight, stay
  F = MBDETES_getSurvLaden()

  G2b = MBITES:::Parameters$get_B_surv()
  G2a = MBDETES_getLocalHazMortality(site)
  G2 = G2b*G2a

  I2 = MBDETES_getLeaveUnladen()

  B2R = A*(B1*C2*D2+B2*C6)*F  
  Fail = ((1-A)+A*(B1*(D3*C2+C3)+B2*C6+B3*C8+B4)) 
  B2F = Fail*(1-G2)*I2
  B2B = Fail*(1-G2)*(1-I2) 

  # additional mass on D from local hazards
  B2D = 1-B2R-B2F-B2B  

  # normalize
  B2ALL = c(B2F, B2B, B2R, 0, 0, B2D)
  return(B2ALL)
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
  surviveRest = MBDETES_getPrPPRFlight() # FIX ME !!!

  R2B = blood*refeed*surviveRest
  R2F = (1-blood)*refeed*surviveRest
  R2L = (1-aquatic)*(1-refeed)*surviveRest
  R2O = aquatic*(1-refeed)*surviveRest
  R2D = 1-R2F-R2B-R2L-R2O

  # additional mass on D from local hazards
  localHaz = MBDETES_getLocalHazMortality(site)
  R2D = R2D + localHaz

  # normalize
  R2ALL = c(R2F, R2B, 0, R2L, R2O, R2D)
  R2ALL = R2ALL/sum(R2ALL)

  return(R2ALL)
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

  L2O = success*survive
  L2L = (1-success)*survive
  L2D = 1-L2O-L2L

  # additional mass on D from local hazards
  localHaz = MBDETES_getLocalHazMortality(site)
  L2D = L2D + localHaz

  # normalize
  L2ALL = c(0, 0, 0, L2L, L2O, L2D)
  L2ALL = L2ALL/sum(L2ALL)

  return(L2ALL)
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
  stay    = 1-MBDETES_getLeaveUnladen()

  O2F = success*survive*(1-blood)
  O2B = success*survive*blood
  O2O = (1-success)*survive*stay
  O2L = (1-success)*survive*(1-stay)
  O2D = 1-O2F-O2B-O2O-O2L

  # additional mass on D from local hazards
  localHaz = MBDETES_getLocalHazMortality(site)
  O2D = O2D + localHaz

  # normalize
  O2ALL = c(O2F, O2B, 0, O2L, O2O, O2D)
  O2ALL = O2ALL/sum(O2ALL)

  return(O2ALL)
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
  FBRLOD[6,6] = 1 # death is absorbing
  return(FBRLOD)
}
