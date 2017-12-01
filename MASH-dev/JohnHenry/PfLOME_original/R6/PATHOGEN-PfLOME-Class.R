###############################################################################
#
#       ____  ______    ____  __  _________
#      / __ \/ __/ /   / __ \/  |/  / ____/
#     / /_/ / /_/ /   / / / / /|_/ / __/
#    / ____/ __/ /___/ /_/ / /  / / /___
#   /_/   /_/ /_____/\____/_/  /_/_____/
#
#   MASH-PATHOGEN
#   PfLOME: Class Definition
#   MASH Team
#   August 24, 2017
#
###############################################################################

#' PfLOME Class Definition
#'
#' Generate a PfLOME object for PfLOME module of plasmodium falciparum infection.
#' Each instance of a \code{PfLOME} lives in a \code{\link{Human}} object's \code{private$Pathogens} field.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:#'
#'
#' @section **Methods**:
#'
#' @section **Fields**:
#'
#'
#'
#' @md
#' @export
PfLOME <- R6::R6Class(classname = "PfLOME",
                    portable = TRUE,
                    cloneable = TRUE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    # public members
                    public = list(

                      #################################################
                      # Constructor
                      #################################################

                      initialize = function(){

                      }

                    ),

                    # private members
                    private = list(

                    )
) # end class definition
