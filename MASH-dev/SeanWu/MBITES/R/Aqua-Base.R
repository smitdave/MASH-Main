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
#' A \code{Aqua_Base} object is an abstract base class for models of aquatic population dynamics following the pimpl idiom within a
#' \code{\link[MBITES]{Aqua_Resource}}. This class defines an interface called from \code{\link[MBITES]{Aqua_Resource}}, such that
#' any aquatic population model must provide definitions of the interface functions.
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * none
#'
#' @section **Methods**:
#'  * add_egg: function that must take an EggQ (a \code{list} object) and add egg batches to the population; it should return a modified EggQ with those elements corresponding to 'processed' egg batches zeroed out
#'  * one_day: function with void return that runs one day of the specific aquatic population simulation implementation
#'  * get_imago: function that returns an ImagoQ (a \code{list} object) for imagos (adult mosquitoes) ready to emerge on that day
#'
#' @section **Fields**:
#'  * none
#'
#' @export
Aqua_Base <- R6::R6Class(classname = "Aqua_Base",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # add egg batches to aquatic population
                   add_egg = function(EggQ){
                     stop("add_egg should never be called from abstract base class 'Aqua_Base'!")
                   },

                   # one day of aquatic population
                   one_day = function(){
                     stop("one_day should never be called from abstract base class 'Aqua_Base'!")
                   },

                   # get emerging imagos
                   get_imago = function(){
                     stop("get_imago should never be called from abstract base class 'Aqua_Base'!")
                   }

                 ),

                 # private members
                 private = list(
                   ImagoQ = list(),
                   tileP  = NULL
                 )
)
