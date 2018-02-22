###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Parameters
#     MBITES Team
#     February 2018
#
###############################################################################

#' MBITES Parameters Singleton
#'
#' This class is a singleton object in the \code{MBITES} package namespace that stores parameters needed for the MBITES simulation.
#' 
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
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * id: integer identifier of site
#'  * field: im a field!
#'
MBITES_Parameters <- R6::R6Class(classname = "MBITES_Parameters",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 public = list(
                   set_x = function(xx){private$x = xx},
                   get_x = function(){private$x}
                 ),

                 private = list(
                   x = numeric(1)
                 ),
)

# assign MBITES parameters instance in the package namespace (a bit hacky)
MBITES_Parameters_Instance <- MBITES_Parameters$new()
