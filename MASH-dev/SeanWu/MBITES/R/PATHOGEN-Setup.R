###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Setup
#     MBITES Team
#     March 2018
#
###############################################################################

#' Pathogen
#'
#' @section Pathogen Models:
#'
#' A Pathogen model has to be set up before initializing any objects.
#'
#' @name Pathogen
NULL
#> NULL


###############################################################################
# PATHOGEN-XX
###############################################################################

#' PATHOGEN: Setup Pathogen Model
#'
#' Add methods to the \code{\link{Mosquito_Female}} class to initialize a pathogen model.
#'
#'
#'
#'
#' @details
#' Arguments for the setup function are listed below:
#'
#' *pathogen_model*: character in \code{null}
#'  * null: use the null pathogen model (see \code{\link{PathogenNull}} for more details)
#'
#' @export
PATHOGEN_Setup <- function(pathogen_model){

  switch(pathogen_model,

    # NULL pathogen model
    null = {
      Mosquito_Female$set(which = "public",name = "probeHost",
          value = probeHost_NULL, overwrite = TRUE
      )

      Mosquito_Female$set(which = "public",name = "feedHost",
          value = feedHost_NULL, overwrite = TRUE
      )

      Mosquito_Female$set(which = "public",name = "pathogenDynamics",
          value = pathogenDynamics_NULL, overwrite = TRUE
      )
    },

    {stop("invalid entry for 'pathogen_model'\n")}
  )

  MBITES:::Globals$set_SETUP("pathogen")
}
