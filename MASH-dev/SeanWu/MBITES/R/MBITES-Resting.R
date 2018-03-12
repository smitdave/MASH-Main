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


###############################################################################
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
###############################################################################

# mbites_enterHouse <- function(){
#   p = private$site$
# }

mbites_restingSpot <- function(){
  if(self$isActive()){ # if mosquito not dead
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


# check to see how many times i've failed my bout and gives a probability to leave
# even in the presence of resources
# returns a boolean indicating that the mosquito should go search if true, or stay if false
# this should be called from mbites_restingSpot!!!
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
# mbites_newSpot <- function(){
#
#
#
#   if(self$get_MySiteType() == 1){
#     probs = private$FemalePopPointer$get_MBITES_PAR("InAndOut")[private$lspot,] * self$get_WTS()
#     sample(x = private$FemalePopPointer$get_MBITES_PAR("lspot"),size = 1,prob = probs)
#   } else {
#     return("v")
#   }
# }





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
