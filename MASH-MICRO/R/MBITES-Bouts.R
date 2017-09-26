###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Bouts
#   MASH-MICRO Team
#   September, 2017
#
###############################################################################


#################################################################
# M-BITES: Dwell Times
#################################################################

#' M-BITES: Exponential Timing for \code{MosquitoFemale}
#'
#' Method for exponentially-distributed bout lengths (model mosquitoes as a Markov process).
#'  * This method is bound to \code{MosquitoFemale$timingExponential()}.
#'
mbites_timingExponential <- function(){
  if(self$isActive()){
    duration = switch(private$state,
      F = {private$FemalePopPointer$get_MBITES_PAR("F_time")},
      B = {private$FemalePopPointer$get_MBITES_PAR("B_time")},
      R = {private$FemalePopPointer$get_MBITES_PAR("R_time")},
      L = {private$FemalePopPointer$get_MBITES_PAR("L_time")},
      O = {private$FemalePopPointer$get_MBITES_PAR("O_time")},
      M = {private$FemalePopPointer$get_MBITES_PAR("M_time")},
      S = {private$FemalePopPointer$get_MBITES_PAR("S_time")}
    )
    private$tNext = private$tNow + rexp(n=1,rate=1/duration)
  }
}

#' M-BITES: Gamma Timing for \code{MosquitoFemale}
#'
#' Method for Gamma-distributed bout lengths (model mosquitoes as a semi-Markov process).
#'  * This method is bound to \code{MosquitoFemale$timingExponential()}.
#'
mbites_timingGamma <- function(){
  if(self$isActive()){
    duration = switch(private$state,
      F = {private$FemalePopPointer$get_MBITES_PAR("F_time")},
      B = {private$FemalePopPointer$get_MBITES_PAR("B_time")},
      R = {private$FemalePopPointer$get_MBITES_PAR("R_time")},
      L = {private$FemalePopPointer$get_MBITES_PAR("L_time")},
      O = {private$FemalePopPointer$get_MBITES_PAR("O_time")},
      M = {private$FemalePopPointer$get_MBITES_PAR("M_time")},
      S = {private$FemalePopPointer$get_MBITES_PAR("S_time")}
    )
    private$tNext = private$tNow + rgamma(n=1,shape=private$FemalePopPointer$get_MBITES_PAR("gammaShape"),rate=(1/duration)*private$FemalePopPointer$get_MBITES_PAR("gammaShape"))
  }
}


#################################################################
#  M-BITES: Post-bout Landing, House Entering, and Resting
#
#  HOUSE ENTERING & RESTING BEHAVIOR:
#  At the end of the search bout, attempt bout, or after Egg
#  laying a mosquito has entered the area around a feeding
#  station and either rested or attempted to rest:
#    l) Leave the area
#    r) Reattempt Without Resting;
#    v) Rest on vegetation
#    w) Rest on the Outside wall of a structure
#    i) Rest on the Inside wall of a structure
#
#################################################################

#' M-BITES: Return Site Type \code{MosquitoFemale}
#'
#' Method to return integer corresponding to site type of \code{\link{MicroSite}} this mosquito is currently at.
#'  Site Types:
#'  * 1: peri-domestic site
#'  * 0: not peri-domestic site
#'
#'  * This method is bound to \code{MosquitoFemale$get_MySiteType()}.
#'
#' @return vector of landing spot weights
mbites_get_MySiteType <- function(){
  switch(private$inPointSet,
    f = {return(private$LandscapePointer$get_FeedingSites(private$ix)$get_siteType())},
    l = {return(private$LandscapePointer$get_AquaSites(private$ix)$get_siteType())},
    m = {return(private$LandscapePointer$get_MatingSites(private$ix)$get_siteType())},
    s = {return(private$LandscapePointer$get_SugarSites(private$ix)$get_siteType())},
    {stop("illegal point set for M-BITES")}
  )
}

#' M-BITES: Return Landing Spot Weights for \code{MosquitoFemale}
#'
#' Method for return landing spot weights based on behavioral state of mosquito.
#'  * This method is bound to \code{MosquitoFemale$get_WTS()}.
#'
#' @return vector of landing spot weights
mbites_get_WTS <- function(){
  switch(private$state,
    F = private$FemalePopPointer$get_MBITES_PAR("F_wts"),
    B = private$FemalePopPointer$get_MBITES_PAR("B_wts"),
    R = private$FemalePopPointer$get_MBITES_PAR("R_wts"),
    L = private$FemalePopPointer$get_MBITES_PAR("L_wts"),
    O = private$FemalePopPointer$get_MBITES_PAR("O_wts"),
    M = private$FemalePopPointer$get_MBITES_PAR("M_wts"),
    S = private$FemalePopPointer$get_MBITES_PAR("S_wts"),
    {stop(cat("illegal behavioral state: ",private$state,"\n",sep=""))}
  )
}

#' M-BITES: Generate New Landing Spot for \code{MosquitoFemale}
#'
#' Method for return a new landing spot based on behavioral state of mosquito and weights from \code{\link{mbites_get_WTS}}.
#' New landing spots generated at the end of the search bout, attempt bout, or after oviposition a mosquito has entered
#' the area around a feeding site and either rested or attempted to rest. If the site is not a \code{\link{FeedingSite}} or peri-domestic the mosquito always rests on vegetation.
#'  * i: rest on the inside wall of a structure
#'  * w: rest on the outside wall of a structure
#'  * v: rest on vegetation
#'  * r: reattempt without resting
#'  * l: leave the area
#'
#'  * This method is bound to \code{MosquitoFemale$newSpot()}.
#'
#' @return integer value corresponding to new landing spot
mbites_newSpot <- function(){
  if(self$get_MySiteType() == 1){
    probs = private$FemalePopPointer$get_MBITES_PAR("InAndOut")[private$lspot,] * self$get_WTS()
    sample(x = private$FemalePopPointer$get_MBITES_PAR("lspot"),size = 1,prob = probs)
  } else {
    return("v")
  }
}

#' M-BITES: Attempt to Enter a House for \code{MosquitoFemale}
#'
#' Method to simulate attempted house entry for mosquito, and call appropriate events if the mosquito enters.
#'  * This method is bound to \code{MosquitoFemale$enterHouse()}.
#'
mbites_enterHouse <- function(){
  if(runif(1) < private$LandscapePointer$get_FeedingSites(private$ix)$get_enterP()){
    # mosquito is inside of house
  } else {
    # mosquito is not inside of house
    private$lspot = self$newSpot()
    self$surviveFlight()
    if(private$lspot == "i"){
      Recall()
    }
  }
}

#' M-BITES: Land After Flight \code{MosquitoFemale}
#'
#' Mosquito lands after a flight (choose a landing spot), which may cause various events.
#' This function always calls \code{\link{mbites_newSpot}} and may call \code{\link{mbites_enterHouse}}
#' Landing spots include:
#'  * i: 1 rest on the inside wall of a structure
#'  * w: 2 rest on the outside wall of a structure
#'  * v: 3 rest on vegetation
#'  * r: 4 reattempt without resting
#'  * l: 5 leave the area
#'
#'  * This method is bound to \code{MosquitoFemale$landingSpot()}.
#'
mbites_landingSpot <- function(){
  if(self$isActive()){
    oldSpot = private$lspot
    private$lspot = self$newSpot() # choose new lspot
    if(oldSpot != "i" & private$lspot == "i"){
      self$enterHouse() # enterHouse
    }
  }
}


#################################################################
#
# M-BITES: Bouts
#
#################################################################

#################################################################
# M-BITES: Blood Feeding Search Bout :: F
#################################################################

mbites_boutF <- function(){

  # mosquito transitions to attempting a blood feeding attempt if she isn't leaving the area and succeeds
  if(private$lspot != "l" & runif(1) < private$FemalePopPointer$get_MBITES_PAR("F_succeed")){
    private$stateNew = "B"
  } else {
    private$stateNew = "F"
  }

}


#################################################################
# M-BITES: Blood Feeding Attempt Bout :: B
#################################################################

mbites_boutB <- function(){

}


#################################################################
# M-BITES: Post-Prandial Resting Bout :: R
#################################################################

mbites_boutR <- function(){

}


#################################################################
# M-BITES: Egg Laying Search Bout :: L
#################################################################

mbites_boutL <- function(){

}


#################################################################
# M-BITES: Egg Laying Attempt Bout :: O
#################################################################

mbites_boutO <- function(){

}


#################################################################
# M-BITES: Sugar Feeding Attempt Bout :: S
#################################################################

#' M-BITES: Sugar Feeding Bout \code{MosquitoFemale}
#'
#' A mosquito performs a sugar feeding bout (all actions taken launch to launch when resting required).
#' Upon entering the sugar feeding behavioral state prompted by \code{\link{mbitesGeneric_queueSugarBout}}, the mosquito will move to a \code{\link{SugarSite}} and
#' sugar feed .
#'  * This method is bound to \code{MosquitoFemale$boutS()}.
#'
mbites_boutS <- function(){

  if(self$isAlive()){
    if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("S_succeed")){
      private$energy = 1

      if(!private$mature){
        private$energyPreG = private$energyPreG - private$FemalePopPointer$get_MBITES_PAR("preGsugar")
        if(private$energyPreG <= 0){
          private$mature = TRUE
        }
      }

      # if mosquito has eggs she transitions to oviposition; otherwise she goes to blood feeding
      if(private$batch > 0){
        private$stateNew = "L"
      } else {
        private$stateNew = "F"
      }

    } else {
      private$stateNew = "R"
    }
  }


}


#################################################################
# M-BITES: Mating Bout :: M
#################################################################

#' M-BITES: Mating Bout \code{MosquitoFemale}
#'
#' A mosquito performs mating bout (all actions taken launch to launch when resting required).
#' Upon entering the mating behavioral state, the mosquito will move to a \code{\link{MatingSite}} and
#' finds a mate in the \code{\link[MASHcpp]{MatingQ}}.
#'  * This method is bound to \code{MosquitoFemale$boutR()}.
#'
mbites_boutM <- function(){

  if(self$isAlive()){
    if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("M_succeed")){
      self$chooseMate()
      private$stateNew = "F"
    }
  }

}




# #################################################################
# #  Blood Feeding Search Bout :: F
# #################################################################
#
# boutF <- function(M,P){
#   if(M$lspot != "l" && rbinom(n=1,size=1,prob=P$F.s)){
#     M$stateNew = "B"
#   }
#
#   return(M)
# }
#
#
# #################################################################
# #  Blood Feeding Attempt Bout :: B
# #################################################################
#
# boutB <- function(M,P){
#   if(rbinom(1,1,P$B.s)){
#     M = chooseHost(M) # MBITES-ChooseHost.R
#   } else {
#     M$hostID = 0
#   }
#
#   if(M$hostID > 0){
#     M = humanEncounter(M,P) # MBITES-HostEncounter.R
#   } else if(M$hostID == -1){
#     M = zooEncounter(M,P) # MBITES-HostEncounter.R
#   } else if(M$hostID == 0){
#     M = nullEncounter(M,P) # MBITES-HostEncounter.R
#   } else {
#     stop("hostID of mosy id: ",M$id," not a recognized host ID")
#   }
#
#   return(M)
# }
#
#
# #################################################################
# #  Post-Prandial Resting Bout :: R
# #################################################################
#
# boutR <- function(M,P){
#   #. boutR: Mosquito resting bout
#   if(!rbinom(1,1,P$R.p)){
#     M$stateNew = "D"
#   } else {
#     if(M$female){ # female behavior
#       if(!M$mature){ # immature female
#         M$stateNew = sample(x = c("F","S"),size = 1)
#       } else { # mature female
#         M = reFeed(M,P) # MBITES-Energetics.R
#       }
#     } else { # male behavior
#       M$stateNew = "M"
#     }
#   }
#
#   return(M)
# }
#
# #################################################################
# #  Egg Laying Search Bout :: L
# #################################################################
#
# boutL <- function(M,P){
#   if(rbinom(1,1,P$L.s)){
#     M$stateNew = "O"
#   }
#
#   return(M)
# }
#
#
# #################################################################
# #  Egg Laying Attempt Bout :: O
# #################################################################
#
# layEggs <- function(M,P){
#   if(rbinom(1,1,P$O.s)){
#     makeBatches(M) # MBITES-Energetics.R
#     M$batch = 0
#     M$stateNew = "F"
#   }
#
#   return(M)
# }
#
# boutO <- function(M,P){
#   # M=fOvitrap(M)
#   if(isAlive(M)){
#     M = layEggs(M,P)
#   }
#   return(M)
# }
#
#
# #################################################################
# #  Sugar Feeding Attempt Bout :: O
# #################################################################
#
# boutS <- function(M,P){
#   with(P,{
#     if(M$female){
#
#       # female behavior
#       M$stateNew = sample(x = c("F","D"), size = 1, prob = c(S.s,1-S.s))
#       if(isAlive(M)){
#         M$energy = 1
#
#         if(!M$mature){
#           M$energyPreG = M$energyPreG - preGsugar
#           if(M$energyPreG <= 0){
#             M$mature = TRUE
#           }
#         }
#       }
#
#     } else {
#
#       # male behavior
#       M$stateNew = sample(x = c("R","D"), size = 1, prob = c(S.s,1-S.s))
#       if(isAlive(M)){
#         M$energy = 1
#       }
#
#     }
#
#     # M=fATSB(M)
#     return(M)
#   })
# }
