###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Landscape-Point
#     MBITES Team
#     February 2018
#
###############################################################################


#' Landscape Site Class
#'
#' A landscape consists of a set of \code{Site} objects, each of which may have one or more resources present.
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
#' @export
Site <- R6::R6Class(classname = "Site",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   ##############################################################
                   # constructor
                   ##############################################################

                   initialize = function(){

                   } # end constructor

                 ),

                 # private members
                 private = list(

                   id             = integer(1), # integer id
                   xy             = numeric(2), # xy-coordinates
                   res_feed       = list(), # list of references to 'feeding'-type resources
                   res_aqua       = list(), # list of references to 'aqua'-type resources
                   res_sugar      = list(), # list of references to 'sugar'-type resources
                   res_mate       = list() # list of references to 'mate'-type resources

                 )


)
