# ###############################################################################
# #         __  __
# #        / / / /_  ______ ___  ____ _____
# #       / /_/ / / / / __ `__ \/ __ `/ __ \
# #      / __  / /_/ / / / / / / /_/ / / / /
# #     /_/ /_/\__,_/_/ /_/ /_/\__,_/_/ /_/
# #
# #     Human Population
# #     MBITES Team
# #     March 2018
# #
# ###############################################################################
#
#
#
#
#
# ###############################################################################
# # Human_Population
# ###############################################################################
#
# #' Human Population Class
# #'
# #' A \code{Human_Population} inherits from the \code{\link[MBITES]{HashMap}} class, including the interface.
# #'
# #'
# #'
# #' @docType class
# #' @format An \code{\link{R6Class}} generator object
# #' @keywords R6 class
# #'
# #' @section **Constructor**:
# #'  * argument: im an agument!
# #'
# #' @section **Methods**:
# #'  * method: i'm a method!
# #'
# #' @section **Fields**:
# #'  * field: i'm a field!
# #'
# #' @export
# Mosquito_Population <- R6::R6Class(classname = "Mosquito_Population",
#                  portable = TRUE,
#                  cloneable = FALSE,
#                  lock_class = FALSE,
#                  lock_objects = FALSE,
#                  inherit = MBITES:::HashMap,
#
#                  # public members
#                  public = list(
#
#                    # begin constructor
#                    initialize = function(N){
#                      super$initialize(N=N)
#                    }, # end constructor
#
#                    # begin destructor
#                    finalize = function(){
#                      super$finalize()
#                    } # end destructor
#
#
#                  ),
#
#                  # private members
#                  private = list()
# ) # end MosquitoPop class definition
