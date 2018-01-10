###############################################################################
#       ____  _________ ____
#      / __ \/ __/ ___//  _/
#     / /_/ / /_ \__ \ / /
#    / ____/ __/___/ // /
#   /_/   /_/  /____/___/
#
#   MASH-MACRO
#   PfSI SimBite
#   MASH Team
#   November 2017
#
###############################################################################

###############################################################################
# Setup
###############################################################################

#' Initialize SimBite PfSI Module
#'
#' Generate a list of parameters PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#'
#' @param DurationPf 200; duration of infection (How many days does the infection last?)
#' @return Defines a field (list) PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#'
#' @export
SimBitePfSI.Setup <- function(overwrite = TRUE){

  cat("initializing PfSI SimBite module\n")

  # add2Q_SimBitePfSI
  Human$set(which = "public",name = "add2Q_SimBitePfSI",
            value = add2Q_SimBitePfSI_Human, overwrite = overwrite
  )

  # event_SimBitePfSI: simulated bite event
  Human$set(which = "public",name = "event_SimBitePfSI",
            value = event_SimBitePfSI_Human, overwrite = overwrite
  )

  # SimBitePfSI
  Human$set(which = "public",name = "SimBitePfSI",
            value = SimBitePfSI_Human, overwrite = overwrite
  )

  # queueBites
  Human$set(which = "public",name = "queueBites_SimBitePfSI",
               value = queueBites_SimBitePfSI_Human, overwrite = overwrite
  )

}


###############################################################################
# Simulated Bite
###############################################################################

#' PfSI SimBite \code{Human} Event: Add PfSI Simulated Bite Event to Event Queue
#'
#' Add a simulated bite to a human at time given by \code{tEvent}. A \code{\link[MASHcpp]{mosquitoPfSI}} object
#' is created for the duration of this event.
#'
#'  * This method is bound to \code{Human$add2Q_SimBitePfSI}
#'
#' @param tEvent time of bite
#' @param PAR do not set to a value (default \code{NULL})
#'
add2Q_SimBitePfSI_Human <- function(tEvent, PAR = NULL){
  PAR = list()
  PAR$mosquitoPfSI = MASHcpp::mosquitoPfSI(PfID_init = -1L, MosquitoID_init = "SimBite", tInf_init = tEvent, infected_init = TRUE)
  private$EventQueue$addEvent2Q(event = self$event_SimBitePfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI SimBite \code{Human} Event: Generate PfSI Simulated Bite Event
#'
#' Generate a simulated bite event to place in the \code{\link[MASHcpp]{EventQ}};
#' this method should only be called through \code{\link{add2Q_SimBitePfSI_Human}}
#'
#'  * This method is bound to \code{Human$event_SimBitePfSI}
#'
#' @param tEvent time of bite
#' @param PAR do not set to a value (default \code{NULL})
#'
event_SimBitePfSI_Human <- function(tEvent, PAR = NULL){
  return(
    list(tEvent = tEvent, PAR = PAR, tag = "SimBitePfSI")
  )
}

#' PfSI SimBite \code{Human} Event: PfSI Simulated Bite Event
#'
#' Simulate a PfSI biting event. This event calls \code{\link{probeHost_PfSI}}
#'
#'  * This method is bound to \code{Human$SimBitePfSI}
#'
#' @param tEvent time of bite
#' @param PAR do not set to a value (default \code{NULL})
#'
SimBitePfSI_Human <- function(tEvent, PAR){
  self$probeHost_PfSI(tEvent, PAR$mosquitoPfSI)
}


###############################################################################
# Queue Events
###############################################################################

#' PfSI SimBite \code{Human} Event: Queue Bites
#'
#' Queue bites by simulating a homogeneous Poisson process and calling \code{\link{add2Q_SimBitePfSI_Human}}
#'
#'  * This method is bound to \code{Human$queueBites_SimBitePfSI}
#'
#' @param tMax maximum time to simulate
#' @param bitingRate biting rate
#'
queueBites_SimBitePfSI_Human <- function(tMax, bitingRate = 1/20){
  nBite = rpois(n=1,lambda=tMax*bitingRate)
  tBite = runif(n=nBite,min=0,max=tMax)
  for(t in tBite){
    self$add2Q_SimBitePfSI(tEvent = t)
  }
}
