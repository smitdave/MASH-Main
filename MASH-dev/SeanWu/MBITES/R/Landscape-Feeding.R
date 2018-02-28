###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Landscape-Resource-Feeding
#     MBITES Team
#     February 2018
#
###############################################################################


#' Landscape Feeding Resource Class
#'
#' A \code{Feeding_Resource} is a type of resource at a \code{\link[MBITES]{Site}} where mosquitoes can expect to find
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
Feeding_Resource <- R6::R6Class(classname = "Feeding_Resource",
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

                 ),

                 # private members
                 private = list(

                   w                  = numeric(1) # weight for this resource

                   # SiteP             = NULL, # pointer to my enclosing Site (has-a relationship; Sites manage Resource lifespans)


                 )


)


#' Feeding_Resource: Get Resource Weight
#'
#' Get the weight associated to this resource.
#'  * binding: \code{Feeding_Resource$get_w}
#'
get_w_Feeding_Resource <- function(){
  return(private$w)
}

Feeding_Resource$set(which = "public",name = "get_w",
    value = get_w_Feeding_Resource, overwrite = TRUE
)
