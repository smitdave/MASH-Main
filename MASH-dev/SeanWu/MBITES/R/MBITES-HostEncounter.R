###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MBITES-Host Encounter
#   MBITES Team
#   March 2018
#
###############################################################################

#' MBITES: Host Encounter
#'
#'
#' Due to the different design of the R6/C++ OOP version of MASH-MICRO, all
#' host probing and infection functions are moved to their respective PATHOGEN
#' method files PATHOGEN-XX-Methods.R
#'
#' @name hostEncounter
NULL
#> NULL

###############################################################################
# Choose a Host
###############################################################################

mbites_chooseHost <- function(){
  private$hostID = private$feed_res$RiskQ$sampleQ()
}


###############################################################################
# Human Host Encounter
###############################################################################

#' MBITES-BRO: Human Host Encounter for \code{\link{MosquitoFemale}}
#'
#' After calling \code{\link{mbites_chooseHost}}, the mosquito encounters the human host and attempts to feed.
#'  * This method is bound to \code{MosquitoFemale$humanEncounter()}.
#'
mbites_humanEncounter <- function(){
  if(runif(1) < 1-MBITES:::Parameters$get_surviveH()){
    # does not survive to probe
    private$state = "D"
  # survives to probe
  } else {
    if(runif(1) < MBITES:::Parameters$get_probeH()){
      # undeterred, probes the host
      self$probeHost() # PATHOGEN-XX.R (mosy -> host transmission)
      if(runif(1) < MBITES:::Parameters$get_surviveProbeH()){
        # does not survive to blood feed
        private$state = "D"
      # survives to blood feed
      } else {
        if(runif(1) < MBITES:::Parameters$get_feedH()){
          # successfully begins blood feeding
          self$BloodMeal() # MBITES-Energetics.R
          self$feedHost() # PATHOGEN-XX.R
        } else {
          # if i did not successfully begin blood feeding, this bout was a failure
          private$boutFail = private$boutFail + 1L
        }
      }
    # if i was deterred, this bout was a failure
    } else {
      private$boutFail = private$boutFail + 1L
    }
  }
}


###############################################################################
# Zoo Host Encounter
###############################################################################

#' MBITES: Non-Human Host Encounter for \code{\link{MosquitoFemale}}
#'
#' After calling \code{\link{mbites_chooseHost}}, the mosquito encounters the non-human host and attempts to feed.
#'  * This method is bound to \code{MosquitoFemale$zooEncounter()}.
#'
mbites_zooEncounter <- function(){
  if(runif(1) < 1-MBITES:::Parameters$get_surviveZ()){
    # does not survive to feed
    private$state = "D"
  # survives to feed
  } else {
    if(runif(1) < MBITES:::Parameters$get_feedZ()){
      # successfully begins blood feeding
      self$BloodMeal() # MBITES-Energetics.R
    } else {
      # if i did not successfully begin blood feeding, this bout was a failure
      private$boutFail = private$boutFail + 1L
    }
  }
}
