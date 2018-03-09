###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Generic SIS Model
#     MBITES Team
#     March 2018
#
###############################################################################


# #' Generic SIS Pathogen Class
# #'
# #' This is a generic SIS pathogen class without superinfection. It interacts with ...
# #'
# #'
# #' @docType class
# #' @format An \code{\link{R6Class}} generator object
# #' @keywords R6 class
# #'
# #' @section **Constructor**:
# #'  * argument: im an agument!
# #'
# #' @section **Methods**:
# #'  * method: i'm a method!
# #'
# #' @section **Fields**:
# #'  * field: i'm a field!
# #'
# #' @export
# SIS_Pathogen <- R6::R6Class(classname = "SIS_Pathogen",
#                  portable = TRUE,
#                  cloneable = FALSE,
#                  lock_class = FALSE,
#                  lock_objects = FALSE,
#
#                  # public members
#                  public = list(
#
#                    # begin constructor
#                    initialize = function(){
#                      futile.logger::flog.trace("SIS_Pathogen being born at self: %s , private: %s",pryr::address(self),pryr::address(private))
#                    }, # end constructor
#
#                    # begin destructor
#                    finalize = function(){
#                      futile.logger::flog.trace("SIS_Pathogen being killed at self: %s , private: %s",pryr::address(self),pryr::address(private))
#                    } # end destructor
#
#                  ),
#
#                  # private members
#                  private = list(
#
#                    id = integer(1), # pathogen id
#                    state = logical(1) # state: SI?
#
#                  )
# ) # end SIS_Pathogen class definition
#
#
# # host probing is when mosy -> human infection events can occur
# probeHost_SIS <- function(){
#
# }
#
# # blood feeding is when human -> mosy infection events can occur
# bloodFeed_SIS <- function(){
#
# }
