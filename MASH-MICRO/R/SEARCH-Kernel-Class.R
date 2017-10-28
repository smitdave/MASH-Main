###############################################################################
#      _____                      __
#     / ___/___  ____ ___________/ /_
#     \__ \/ _ \/ __ `/ ___/ ___/ __ \
#    ___/ /  __/ /_/ / /  / /__/ / / /
#   /____/\___/\__,_/_/   \___/_/ /_/
#
#   MASH-MICRO
#   SEARCH: Kernel Class
#   MASH-MICRO Team
#   October, 2017
#
###############################################################################

# NEED TO INSTANTIATE WITHIN EACH CLASS. THEN SET THE POINTERS TO MOSY POPS AFTER

###############################################################################
# Class Definition
###############################################################################

#' MovementObject Kernel Class Definition
#'
#' This class inherits from \code{\link{MovementObject}}
#'
#' @docType class
#' @format An \code{\link[R6]{R6Class}} generator object
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
MovementObject_Kernel <- R6::R6Class(classname = "MovementObject_Kernel",
                          inherit = MASHmicro::MovementObject,
                          portable = TRUE,
                          cloneable = FALSE,
                          lock_class = FALSE,
                          lock_objects = FALSE,

                          # public members
                          public = list(

                            ##############################################################
                            # Constructor
                            ##############################################################

                            initialize = function(Site){

                              private$SitePointer = Site

                            }

                          ), # end public

                          # private members
                          private = list(

                            # vector of indices to other sites (sorted by distance so its not necessarily in increasing order)
                            F_id = NULL,
                            L_id = NULL,
                            S_id = NULL,
                            M_id = NULL,

                            # vector of distances to other sites
                            D2F = NULL,
                            D2L = NULL,
                            D2S = NULL,
                            D2M = NULL,

                            # vector of probabilities to other sites
                            P2F = NULL,
                            P2L = NULL,
                            P2S = NULL,
                            P2M = NULL

                          ) # end private
) # end class definition


###############################################################################
# Class Methods
###############################################################################





#' Get SitePointer
#'
#' Return pointer to my enclosing site
#'  * This method is bound to \code{MovementObject$get_SitePointer}
#'
get_SitePointer_MovementObject <- function(){return(private$SitePointer)}

MovementObject$set(which = "public",name = "get_SitePointer",
  value = get_SitePointer_MovementObject, overwrite = TRUE
)
