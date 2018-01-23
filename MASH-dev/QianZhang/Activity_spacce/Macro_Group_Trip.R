###############################################################################
#                                        __       _     
#       ____ __________  __  ______     / /______(_)___ 
#      / __ `/ ___/ __ \/ / / / __ \   / __/ ___/ / __ \
#     / /_/ / /  / /_/ / /_/ / /_/ /  / /_/ /  / / /_/ /
#     \__, /_/   \____/\__,_/ .___/   \__/_/  /_/ .___/ 
#    /____/                /_/                 /_/      
#
#   MASH-MACRO
#   Group Trip
#   MASH Team
#   Jan 2018
#
###############################################################################


###############################################################################
# A trip is movement to another patch
# movement is movement within a patch
###############################################################################

#' Move \code{HumanPop} Method: Initialize Movement Output
#'
#' Initialize output for movement model.
#'  * This method is bound to \code{HumanPop$initialize_output_Move()}
#'
initialize_output_Move_HumanPop <- function(){
  writeLines(text = paste0(c("humanID","time","event","location"),collapse = ","),con = private$conMove, sep = "\n")
}

#' Move \code{Human} Event: Initialize Travel
#'
#' Initialize travel model when simulation begins by calling \code{\link{initialize_travel_Human}} for all humans.
#'  * This method is bound to \code{HumanPop$initialize_travel}
#'
initialize_travel_HumanPop <- function(){
  private$pop$apply(tag="initialize_travel",returnVal=FALSE)
}

#' Move \code{Human} Event: Initialize Travel
#'
#' Initialize travel model when simulation begins by queueing up a trip calling \code{\link{add2Q_takeTrip}}
#'  * This method is bound to \code{Human$initialize_travel}
#'

initialize_travel_group <- function(site_num){
  DestSites = sample(x = 1:private$TilePointer$get_nPatch(),size = site_num,replace = FALSE,
                     prob = private$TilePointer$get_Patch(private$patchID)$get_travelWeight()) # choose where the group go
  tTrip = private$TilePointer$get_tNow() + rexp(n=1,rate=private$tripFrequency) # choose when the group go
  
}

initialize_travel_Member <- function(){
  tDest = sample(x = DestSites,size = 1,replace = FALSE,prob = private$TilePointer$get_Patch(private$patchID)$get_travelWeight()) # choose where i go
  # queue the trip
  PAR = list(tDest=tDest)
  self$add2Q_takeTrip(tEvent=tTrip,PAR=PAR)
}



###############################################################################
# Trip event to another patch
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
add2Q_takeTrip <- function(tEvent, PAR){
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
event_takeTrip <- function(tEvent, PAR){
  return(list(tEvent = tEvent, PAR = PAR, tag = "takeTrip"))
}

#' Move \code{Human} Event: Trip Event
#'
#' Simulate a between patch trip.
#' This method is bound to \code{Human$takeTrip()}
#'  * Biting weight at my origin is decremented by \code{\link{decrement_bWeightHuman_Human}} and incremented at my destination by \code{\link{accumulate_bWeightHuman_Human}}
#'  * The end of this trip is queued after a duration by calling \code{\link{add2Q_returnHome}}
#'
#' @param tEvent time of trip
#' @param PAR must be a list containing character \code{tDest}, the index of the site I am visiting
#'
takeTrip <- function(tEvent, PAR){
  # track event
  writeLines(text = paste0(c(private$myID,tEvent,"takeTrip",PAR$tDest),collapse = ","),con = private$HumansPointer$get_conMove(), sep = "\n")
  
  self$decrement_bWeightHuman() # decrement the biting weight where I came from
  private$patchID = PAR$tDest # set my current location
  self$accumulate_bWeightHuman() # increment the biting weight where I go to
  
  # queue up the trip back home
  tReturn = tEvent + rexp(n=1,rate=1/private$tripDuration)
  self$add2Q_returnHome(tEvent = tReturn, PAR = NULL)
  
}


###############################################################################
# Trip home event
###############################################################################

#' Move \code{Human} Event: Add Return Home Trip to Event Queue
#'
#' Add a trip back home to my event queue.
#' This method is called from \code{\link{takeTrip}}
#' This method adds event \code{\link{event_returnHome}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_returnHome()}
#'
#' @param tEvent time to return home
#' @param PAR \code{NULL}
#'
add2Q_returnHome <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_returnHome(tEvent = tEvent, PAR = PAR))
}

#' Move \code{Human} Event: Generate a Return Home Event
#'
#' Generate a return home event to place in event queue.
#' This method is called from \code{\link{add2Q_returnHome}}
#' This method is bound to \code{Human$event_returnHome()}
#'  * tag: \code{\link{returnHome}}
#
#' @param tEvent time to return home
#' @param PAR \code{NULL}
#'
event_returnHome <- function(tEvent, PAR = NULL){
  return(list(tEvent=tEvent,PAR=PAR,tag="returnHome"))
}

#' Move \code{Human} Event: Return Home Event
#'
#' Simulate my return home
#' This method is bound to \code{Human$returnHome()}
#'  * Biting weight at my origin is decremented by \code{\link{decrement_bWeightHuman_Human}} and incremented at my destination by \code{\link{accumulate_bWeightHuman_Human}}
#'  * My next trip is queued after a duration at home by calling \code{\link{add2Q_takeTrip}}
#'
#' @param tEvent time to return home
#' @param PAR \code{NULL}
#'
returnHome <- function(tEvent, PAR){
  # track event
  writeLines(text = paste0(c(private$myID,tEvent,"returnHome",private$homePatchID),collapse = ","),con = private$HumansPointer$get_conMove(), sep = "\n")
  
  self$decrement_bWeightHuman() # decrement the biting weight where I came from
  private$patchID = private$homePatchID # go back home
  self$accumulate_bWeightHuman() # increment the biting weight where I go to
  
  # queue up my next trip
  tDest = sample(x = 1:private$TilePointer$get_nPatch(),size = 1,replace = FALSE,prob = private$TilePointer$get_Patch(private$patchID)$get_travelWeight()) # choose where i go
  tTrip = tEvent + rexp(n=1,rate=private$tripFrequency) # choose when i go
  PAR = list(tDest=tDest)
  self$add2Q_takeTrip(tEvent = tTrip,PAR = PAR)
}
