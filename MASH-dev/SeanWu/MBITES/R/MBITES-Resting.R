###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Resting
#     MBITES Team
#     March 2018
#
###############################################################################

#' MBITES: Resting
#'
#' @section Post-bout Landing, House Entering, and Resting:
#'
#'  House Entering & Resting Behavior:
#'  At the end of the search bout, attempt bout, or after egg
#'  laying a mosquito has entered the area around a feeding
#'  station and either rested or attempted to rest:
#'    * l: Leave the area
#'    * r: Reattempt Without Resting;
#'    * v: Rest on vegetation
#'    * w: Rest on the Outside wall of a structure
#'    * i: Rest on the Inside wall of a structure
#'
#' @name Resting
NULL
#> NULL


###############################################################################
# Resting spot
###############################################################################

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
#'  * This method is bound to \code{MosquitoFemale$restingSpot()}.
#'
mbites_restingSpot <- function(){
  if(private$state != "D"){ # if mosquito not dead
    if(self$boutFailCheck()){ # check if they will just leave
      private$rspot = "l"
      private$search = TRUE
    } else {
      old_spot = private$rspot
      self$newSpot()
      if(old_spot != "i" & private$rspot == "i"){
        self$enterHouse()
      }
    }
  }
}

#' MBITES: Bout Failure Counter
#'
#' Before choosing a resting spot, check to see how many times it has failed its bout and potentially initiate
#' a search bout. Probability of abandoning the current \code{\link{Site}} even if necessary resources
#' are present is given by a geometric distribution over the number of consecutive failures, with parameter \code{boutFail_p} (1/mean number of failed bouts until mosquito gives up and searches).
#' The \code{boutFail} counter is incremented whenever the mosquito probabilistically fails P(Bout_succeed) and is reset to 0 in the following methods:
#'  * \code{\link{mbites_BloodMeal}}
#'  * \code{\link{mbites_layEggs_Emerge}}
#'  * \code{\link{mbites_layEggs_EL4P}}
#'
#'
#'  * this method is bound to \code{Mosquito_Female$boutFailCheck}
#'
mbites_boutFailCheck <- function(){
  p = dgeom(private$boutFail,MBITES:::Parameters$get_boutFail_p())
  if(runif(1) < p){
    return(TRUE)
  } else {
    return(FALSE)
  }
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
#' @return character corresponding to new resting spot
mbites_newSpot <- function(){
  # homestead
  if(private$site$get_type()==1L){
    probs = MBITES:::Parameters$get_InAndOut_row(private$rspot) * MBITES:::Parameters$get_wts(private$state)
    MBITES:::sample(x = MBITES:::rspot,size = 1L,prob = probs)
  # not homestead
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
  if(runif(1) < private$feed_res$get_enterP()){
    # mosquito is inside of the house
  } else {
    # mosquito is not inside of house
    self$newSpot()
    self$surviveFlight()
    # if i decided to go inside again; call recursively
    if(private$rspot == "i"){
      self$enterHouse()
    }
  }
}
