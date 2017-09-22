###############################################################################
#       ____        __  __
#      / __ \____ _/ /_/ /_  ____  ____ ____  ____
#     / /_/ / __ `/ __/ __ \/ __ \/ __ `/ _ \/ __ \
#    / ____/ /_/ / /_/ / / / /_/ / /_/ /  __/ / / /
#   /_/    \__,_/\__/_/ /_/\____/\__, /\___/_/ /_/
#                               /____/
#
#   MASH-MACRO
#   MACRO: PfLOME Human-stage Class Definition
#   MASH-MACRO Team
#   September 14, 2017
#
###############################################################################


###############################################################################
# Class Definition
###############################################################################

#' PfLOME Human-stage Pathogen Class Definition
#'
#' write me.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: write me
#'
#' @section **Methods**:
#'  * get_something: write me
#'  * set_something: write me
#'
#' @section **Fields**:
#'  * field: write me
#'
#'
#'
#'
#'
#'
#'
#'
#' @export
PfLOME_Human <- R6::R6Class(classname = "PfLOME_Human",
                    portable = TRUE,
                    cloneable = FALSE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    # public members
                    public = list(

                      #################################################
                      # Constructor
                      #################################################

                      initialize = function(){

                        private$clones = MASHcpp::HashMap$new(N=10)

                      }

                    ),

                    # private members
                    private = list(

                      # infection
                      MOI = integer(1),
                      clones = NULL,
                      activeP = integer(10),
                      activeG = integer(10),
                      Pt = integer(10),
                      Gt = integer(10),
                      Ptt = NULL,
                      Ptot = NULL,
                      Gtot = NULL,

                      # immune counters
                      HBR = NULL,
                      EIR = NULL,
                      FOI = NULL,
                      BSImm = NULL,
                      GSImm = NULL,
                      ptypes = NULL,
                      TypeImmunity = NULL,
                      FeverThreshold = NULL,

                      # pharmacodynamics
                      Rx = NULL,

                      # history
                      history = NULL

                    )
)
