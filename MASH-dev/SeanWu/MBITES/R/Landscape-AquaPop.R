###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Landscape-Resource-Aquatic Habitat-Aquatic Population pimpl
#     MBITES Team
#     February 2018
#
###############################################################################


#' Landscape Aquatic Population Model Class
#'
#' A \code{Aqua_Pop} object is an abstract base class for models of aquatic population dynamics following the pimpl idiom within a
#' \code{\link[MBITES]{Aqua_Resource}}.
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
Aqua_Pop <- R6::R6Class(classname = "Aqua_Pop",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(){

                   }, # end constructor

                   # add egg batches to aquatic population
                   add_egg = function(EggQ){
                     stop("add_egg should never be called from abstract base class 'Aqua_Pop'!")
                   },

                   # one day of aquatic population
                   one_day = function(){
                     stop("one_day should never be called from abstract base class 'Aqua_Pop'!")
                   },

                   # get emerging imagos
                   get_imago = function(){
                     stop("get_imago should never be called from abstract base class 'Aqua_Pop'!")
                   }

                 ),

                 # private members
                 private = list()
)
