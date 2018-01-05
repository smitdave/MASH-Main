###############################################################################
#      _____ _ __
#     / ___/(_) /____
#     \__ \/ / __/ _ \
#    ___/ / / /_/  __/
#   /____/_/\__/\___/
#
#   MICRO
#   Site Class
#   MASH Team
#   January 2018
#
###############################################################################


###############################################################################
# Site Abstract Base Class
###############################################################################

#' Site Abstract Base Class
#'
#' Abstract base class for sites.
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
#'  * field: im a field!
#'
#' @export
Site_ABC <- R6::R6Class(classname = "Site_ABC",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(){

                   }

                 ),

                 # private members
                 private = list(

                 )
)
