###############################################################################
#
#       ____  ________           ___
#      / __ \/ __/ __ \___  ____/ (_)___ _________  ___
#     / /_/ / /_/ /_/ / _ \/ __  / / __ `/ ___/ _ \/ _ \
#    / ____/ __/ ____/  __/ /_/ / / /_/ / /  /  __/  __/
#   /_/   /_/ /_/    \___/\__,_/_/\__, /_/   \___/\___/
#                                /____/
#
#   MASH-PATHOGEN
#   PfPedigree: Class Definition
#   MASH Team
#   August 24, 2017
#
###############################################################################


# IMPLEMENT AS MASH-CPP HashMap OBJECT

#' PfPedigree Class
#'
#' Generate a PfPedigree object to track plasmodium falciparum pedigrees. This should generally be initialized at a \code{\link[MASHmacro]{MacroPatch}} or \code{\link[MicroTile]} level.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * reserve: memory reserve for \code{private$pedigree}, stored internally as a \code{\link[MASHcpp]{HashMap}} object
#'
#' @section **Methods**:
#'
#' @section **Fields**:
#'
#'
#'
#'
#' @md
#' @export
PfPedigree <- R6::R6Class(classname = "PfPedigree",
                    portable = TRUE,
                    cloneable = TRUE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    # public members
                    public = list(

                      #################################################
                      # Constructor
                      #################################################

                      initialize = function(reserve = 100L){

                        private$pedigree = MASHcpp::HashMap$new(N = reserve)
                        private$N = 0

                      }

                    ),

                    # private members
                    private = list(

                      N = NULL,
                      pedigree = NULL

                    )
) # end class definition
