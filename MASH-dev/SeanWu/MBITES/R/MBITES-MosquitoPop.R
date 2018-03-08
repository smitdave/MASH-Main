###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Mosquito Population
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Mosquito_Population
###############################################################################

#' Mosquito Population Class
#'
#' A \code{Mosquito_Population} inherits from the \code{\link[MBITES]{HashMap}} class, including the interface.
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
#'  * EggQ: a closure of egg batches (see \code{\link[MBITES]{make_EggQ}})
#'  * ImagoQ: a closure of imago cohorts (see \code{\link[MBITES]{make_ImagoQ}})
#'
#' @export
Mosquito_Population <- R6::R6Class(classname = "Mosquito_Population",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::HashMap,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(N){
                     super$initialize(N=N)
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     super$finalize()
                   } # end destructor


                 ),

                 # private members
                 private = list(
                   tileID = integer(1) # id of the tile this mosquito population inhabits
                 )
) # end MosquitoPop class definition
