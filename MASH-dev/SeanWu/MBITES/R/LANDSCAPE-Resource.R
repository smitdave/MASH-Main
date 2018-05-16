###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Resource
#     MBITES Team
#     February 2018
#
###############################################################################


#' Landscape Resource Class
#'
#' All resources inherit from the \code{Resource} abstract base class object.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * reset: all resources classes inheriting from this base object must implement a method to reset between simulation runs
#'
#' @section **Fields**:
#'  * id: integer id (obtained from \code{\link{MBITES_Globals}})
#'  * field: im a field!
#'
#' @export
Resource <- R6::R6Class(classname = "Resource",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(w,site){
                     futile.logger::flog.trace("Resource being born at: self %s , private %s",pryr::address(self),pryr::address(private))

                     private$w = w
                     private$SiteP = site

                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("Resource being killed at: self %s , private %s",pryr::address(self),pryr::address(private))
                   },

                   # reset
                   reset = function(){}

                 ),

                 # private members
                 private = list(

                   w             = numeric(1), # relative weight
                   SiteP         = NULL # reference to enclosing Site

                 )
) # end Resource class definition


###############################################################################
# Accessors
###############################################################################

#' Resource: Get Weight
#'
#' Get the numeric weight associated with this resource.
#'  * binding: \code{Resource$get_w}
#'
get_w_Resource <- function(){
  return(private$w)
}

Resource$set(which = "public",name = "get_w",
          value = get_w_Resource, overwrite = TRUE
)

#' Resource: Get Site
#'
#' Get a reference to the enclosing \code{\link{Site}} object.
#'  * binding: \code{Resource$get_site}
#'
get_site_Resource <- function(){
  return(private$SiteP)
}

Resource$set(which = "public",name = "get_site",
          value = get_site_Resource, overwrite = TRUE
)
