###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Globals
#     MBITES Team
#     February 2018
#
###############################################################################

#' MBITES Globals Singleton
#'
#' This class is a singleton object in the \code{MBITES} package namespace that stores global values needed for the MBITES simulation.
#' It can be accessed by \code{MBITES:::MBITES_Global}.
#'
#'
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * get_mosquito_id: get an integer ID for a mosquito
#'
#' @section **Fields**:
#'  * id: integer identifier of site
#'  * field: im a field!
#'
MBITES_Globals <- R6::R6Class(classname = "MBITES_Globals",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 public = list(

                   # reset all values
                   reset = function(){
                     cat("reset all MBITES global parameters to initial values\n")
                     private$t_now = 0L
                     private$mosquito_id = 0L
                   },

                   get_mosquito_id = function(){
                     private$mosquito_id = private$mosquito_id + 1L
                     return(private$mosquito_id)
                   },

                   get_t_now = function(){
                     return(private$t_now)
                   },

                   increment_t_now = function(){
                     private$t_now = private$t_now + 1L
                   }

                 ),

                 private = list(

                   mosquito_id        = 0L # global counter of IDs

                   t_now              = 0L # current simulation time

                 ),
)

# assign MBITES globals instance in the package namespace (a bit hacky)
Globals <- MBITES_Globals$new()
