###############################################################################
#       __  ___
#      /  |/  /___ _   _____
#     / /|_/ / __ \ | / / _ \
#    / /  / / /_/ / |/ /  __/
#   /_/  /_/\____/|___/\___/
#
#   MASH-MACRO
#   Movement
#   MASH Team
#   December 2017
#
###############################################################################

###############################################################################
# A trip is movement to another patch
# movement is movement within a patch
###############################################################################

#' Human: Take a Trip to a Patch
#'
#' Take a trip to a new \code{\link{MacroPatch}} and update the source and destination human biting weight
#'
#' @param patchID the id of the patch to travel to
#'
trip_Human <- function(patchID){
  self$decrement_bWeightHuman()
  private$patchID = patchID
  self$accumulate_bWeightHuman(patchID)
}


takeTrip_Human <- function(){
  travelDest = sample(n = 1,size = 1,replace = FALSE,prob = private$TilePointer$get_Patch(private$patchID)$get_travelWeight())


}

# choose my first trip when simulation begins
initialize_trip_Human <- function(){
  tDest = sample(x = 1:private$TilePointer$get_nPatch(),size = 1,replace = FALSE,prob = private$TilePointer$get_Patch(private$patchID)$get_travelWeight()) # choose where i go
  tTrip = private$TilePointer$get_tNow() + rexp(n=1,rate=private$tripFrequency) # choose when i go

  # queue the trip
  PAR = list(tDest=tDest)
  self$add2Q_takeTrip(tEvent=tTrip,PAR=PAR)
}


###############################################################################
# Trip to another patch
###############################################################################

#' Move \code{Human} Event: Add a Trip to Event Queue
#'
#' Add a trip to another patch to my event queue.
#' This method is called from \code{\link{initialize_trip_Human}}
#' This method adds event \code{\link{event_takeTrip}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_takeTrip()}
#'
#' @param tEvent time of trip
#' @param PAR \code{NULL}
#'
add2Q_takeTrip <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_takeTrip(tEvent = tEvent, PAR = PAR))
}

#' Move \code{Human} Event: Generate a Trip Event
#'
#' Generate a trip event to place in event queue.
#' This method is called from \code{\link{add2Q_takeTrip}}
#' This method is bound to \code{Human$event_takeTrip()}
#'  * tag: \code{\link{takeTrip}}
#
#' @param tEvent time of trip
#' @param PAR \code{NULL}
#'
event_takeTrip <- function(tEvent, PAR = NULL){
  return(list(tEvent = tEvent, PAR = PAR, tag = "takeTrip"))
}

#' Move \code{Human} Event: Trip Event
#'
#' Simulate a between patch trip.
#' This method is bound to \code{Human$takeTrip()}
#'  * A Bernoulli event is drawn to determine if this infection produces fever; if so \code{\link{add2Q_feverPfSI}} is called.
#'  * The end of this PfSI infection is queued by \code{\link{add2Q_endPfSI}}
#'
#' @param tEvent time of trip
#' @param PAR must be a list containing character \code{tDest}, the index of the site I am visiting
#'
takeTrip <- function(tEvent, PAR){

  self$decrement_bWeightHuman() # decrement the biting weight where I came from
  private$patchID = PAR$tDest # set my current location
  self$accumulate_bWeightHuman() # increment the biting weight where I go to

  # queue up the trip back home
  tReturn = tEvent + rexp(n=1,rate=1/private$tripDuration)
  self$add2Q_returnHome(tEvent = tReturn, PAR = NULL)

}
