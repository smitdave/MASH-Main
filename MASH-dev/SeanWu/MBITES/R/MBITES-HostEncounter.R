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
#' @section Human and non-human vertebrate host encounters:
#'
#' During a blood feeding attempt bout (\code{\link{mbites_boutB}}), if the mosquito successfully locates
#' a viable human or vertebrate blood host, it initiates a host encounter. A human host encounter (\code{\link{mbites_humanEncounter}})
#' consists of an attempt to approach, probe, and take a blood meal. A non-human host encounter (\code{\link{mbites_zooEncounter}}) consists
#' of an attempt to approach and blood feed only.
#'
#' If the mosquito is able to blood feed, the series of functions to simulate that process will be called, see \code{\link{MBITES-BloodMeal}}.
#' During probing and feeding different functions for simulation of mosquito to human and human to mosquito transmission will be
#' called; see \code{\link{PathogenGeneric}} for what these functions must simulate.
#'
#'
#' @name MBITES-HostEncounter
NULL
#> NULL

###############################################################################
# Choose a Host
###############################################################################

#' MBITES: Choose a Blood Host
#'
#' Sample a blood host from the \code{\link{make_RiskQ}} closure this mosquito is at.
#'  * This method is bound to \code{Mosquito_Female$chooseHost}.
#'
mbites_chooseHost <- function(){
  private$hostID = private$feed_res$RiskQ$sampleQ()
}

# set methods
Mosquito_Female$set(which = "public",name = "chooseHost",
    value = mbites_chooseHost, overwrite = TRUE
)


###############################################################################
# Human Host Encounter
###############################################################################

#' MBITES: Human Host Encounter
#'
#' After calling \code{\link{mbites_chooseHost}}, the mosquito encounters the human host and attempts to feed.
#' The encounter process consists of several stages:
#'  1. Check initial survival to see if the mosquito survives to probe
#'  2. Check if the mosquito is deterred from probing, if undeterred, call pathogen-specific routines \code{probeHost}
#'  3. Check if the mosquito survived probing to attempt blood feeding
#'  4. Check if the mosquito is able to successfully begin blood feeding; take a blood meal via \code{\link{mbites_BloodMeal}} and then call pathogen-specific routines \code{feedHost}
#'
#' For details on what pathogen modules must implement, see \code{\link{PathogenGeneric}}
#'
#'  * This method is bound to \code{Mosquito_Female$humanEncounter}.
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
          self$BloodMeal() # MBITES-BloodMeal.R
          self$feedHost() # PATHOGEN-XX.R
        }
      }
    }
  }
}

# set methods
Mosquito_Female$set(which = "public",name = "humanEncounter",
    value = mbites_humanEncounter, overwrite = TRUE
)


###############################################################################
# Zoo Host Encounter
###############################################################################

#' MBITES: Non-Human Host Encounter
#'
#' After calling \code{\link{mbites_chooseHost}}, the mosquito encounters the non-human host and attempts to feed.
#' The encounter process consists of several stages:
#'  1. Check initial survival to see if the mosquito survives to feed
#'  2. Check if the mosquito is able to successfully begin blood feeding; take a blood meal via \code{\link{mbites_BloodMeal}}
#'
#'  * This method is bound to \code{Mosquito_Female$zooEncounter}.
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
    }
  }
}

# set methods
Mosquito_Female$set(which = "public",name = "zooEncounter",
    value = mbites_zooEncounter, overwrite = TRUE
)
