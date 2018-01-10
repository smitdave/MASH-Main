###############################################################################
#      _____ _ __
#     / ___/(_) /____
#     \__ \/ / __/ _ \
#    ___/ / / /_/  __/
#   /____/_/\__/\___/
#
#   MICRO
#   Site Class
#   MASH Team
#   January 2018
#
###############################################################################


###############################################################################
# Site Class
###############################################################################

#' Site Class
#'
#' A site is a location where resources can be found.
#'
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
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * field: im a field!
#'
#' @export
Site <- R6::R6Class(classname = "Site",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(){

                   }

                 ),

                 # private members
                 private = list(

                   # basic information about a site
                   siteXY                       = numeric(2),
                   siteID                       = integer(1),
                   siteType                     = integer(1),

                   # movement object
                   MoveObject                   = NULL, # std::unique_ptr<MoveObject>

                   # references to resources found at this site
                   FeedingSites                 = list(), # std::vector<std::unique_ptr<FeedingSite>>
                   AquaticHabitats              = list(),
                   MatingSites                  = list(),
                   SugarSites                   = list()

                 )
)
