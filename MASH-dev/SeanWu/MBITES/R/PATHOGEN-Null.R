###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Null (Biting only) Model
#     MBITES Team
#     March 2018
#
###############################################################################

#' PATHOGEN: Null Model
#'
#' The null (biting only) pathogen model is a simple model to log bites only. It can only be used with the \code{\link{Human_NULL}} class.
#' For other details on what full pathogen models must implement, see \code{\link{PathogenGeneric}}.
#'
#' @name PathogenNull
NULL
#> NULL

#' Null Host Probing
#'
#' This method fills in for pathogen model specific host probing (mosquito to human transmission) methods
#' if using the null human & pathogen model. It calls \code{\link{pushProbe_Human_NULL}} to log probing events.
#'
probeHost_NULL <- function(){
  self$trackProbe()
  MBITES:::Globals$get_tile(private$tileID)$get_human(private$hostID)$pushProbe(m_id=private$id,t=private$tNow)
}

#' Null Host Blood feeding
#'
#' This method fills in for pathogen model specific host blood feeding (human to mosquito transmission) methods
#' if using the null human & pathogen model. It calls \code{\link{pushFeed_Human_NULL}} to log probing events.
#'
feedHost_NULL <- function(){
  self$trackFeed()
  MBITES:::Globals$get_tile(private$tileID)$get_human(private$hostID)$pushFeed()
}


#' Null Mosquito Pathogen Dynamics
#'
#' This null function fills in for the per-bout pathogen update function if using the null pathogen model (biting only).
#'  * This method is bound to \code{Mosquito_Female$pathogenDynamics}
#'
pathogenDynamics_NULL <- function(){
  # dont do anything
}


###############################################################################
# Mosquito methods for probeHost and feedHost
# normally, the functions below would go in the PATHOGEN-XX-XX.R file
# #' @include MBITES-Mosquito.R
###############################################################################

PathogenNull_SETUP <- function(){
  Mosquito_Female$set(which = "public",name = "probeHost",
      value = probeHost_NULL, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "feedHost",
      value = feedHost_NULL, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "pathogenDynamics",
      value = pathogenDynamics_NULL, overwrite = TRUE
  )
}
