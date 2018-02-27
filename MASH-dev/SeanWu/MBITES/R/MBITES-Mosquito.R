###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Mosquito
#     MBITES Team
#     February 2018
#
###############################################################################


#' Mosquito Abstract Base Class
#'
#' All mosquitoes inherit from the \code{Mosquito} abstract base class object.
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
#'  * id: integer id (obtained from \code{\link[MBITES]{MBITES_Globals}})
#'  * field: im a field!
#'
#' @export
Mosquito <- R6::R6Class(classname = "Mosquito",
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

                   id             = integer(1), # character id

                   t_next         = numeric(1), # time of my next launch
                   t_now          = numeric(1), # time of my current launch

                   search         = logical(1), # next launch is for search or attempt bout?
                   rspot          = character(1), # my current resting spot


                   hostID         = integer(1), # id of my blood host
                   habitatID      = integer(1), # id of my aquatic habitat
                   sugarID        = integer(1), # id of my sugar source
                   mateID         = integer(1), # id of my mate



                 )


)
