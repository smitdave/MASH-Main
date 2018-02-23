###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Landscape-Resource-Aquatic Habitat
#     MBITES Team
#     February 2018
#
###############################################################################


#' Landscape Aquatic Habitat Resource Base Class
#'
#' A \code{Aqua_Resource_Base} is a type of resource at a \code{\link[MBITES]{Site}} where mosquitoes travel for oviposition of eggs
#' and from which new imagos (adult mosquitoes) emerge from. This abstract base class defines an interface which all models of aquatic ecology
#' must inherit from to generate concrete implementations of the interface methods.
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
#'  * add_egg: function that must take an egg batch and add it to the \code{EggQ}
#'  * one_day: function that updates daily aquatic population dynamics
#'  * push_imago: function that takes emerging imagos from the \code{ImagoQ} and pushes them to the adult mosquito population
#'
#' @section **Fields**:
#'  * EggQ: a list of egg batches
#'  * ImagoQ: a list of emerging imagos (adult mosquitoes)
#'  * SiteP: a reference to a class that inherits the interface of \code{\link[MBITES]{Aqua_Base}}
#'
#' @export
Aqua_Resource_Base <- R6::R6Class(classname = "Aqua_Resource_Base",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(){
                     stop("initialize should never be called from abstract base class 'Aqua_Resource_Base'!")
                   }, # end constructor

                   # add egg batches to aquatic population
                   add_egg = function(){
                     stop("add_egg should never be called from abstract base class 'Aqua_Resource_Base'!")
                   },

                   # one day of aquatic population
                   one_day = function(){
                     stop("one_day should never be called from abstract base class 'Aqua_Resource_Base'!")
                   },

                   # send emerging imagos to adult population
                   push_imago = function(){
                     stop("push_imago should never be called from abstract base class 'Aqua_Resource_Base'!")
                   }

                 ),

                 # private members
                 private = list(

                   EggQ                = list(), # list of egg batches
                   ImagoQ              = list(), # list of newly emerging imagos
                   SiteP               = NULL # pointer to my enclosing Site (has-a relationship; Sites manage Resource lifespans)

                 )
) # end Aqua_Resource class definition
