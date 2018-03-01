###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Resource-Sugar
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Sugar Resource Class
###############################################################################

#' Landscape Sugar Resource Class
#'
#' A \code{Sugar_Resource} is a type of resource at a \code{\link[MBITES]{Site}} where mosquitoes can expect to find
#' human or other vertebrate hosts when seeking a blood meal.
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
Sugar_Resource <- R6::R6Class(classname = "Sugar_Resource",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(w){

                     private$w = w


                   } # end constructor

                   # begin destructor
                   # finalize = function(){}, # end destructor

                 ), # end public members

                 # private members
                 private = list(

                   w                  = numeric(1) # weight for this resource

                 ) # end private members


) # end Feeding_Resource class definition
