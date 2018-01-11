###############################################################################
#      _____ _ __
#     / ___/(_) /____
#     \__ \/ / __/ _ \
#    ___/ / / /_/  __/
#   /____/_/\__/\___/
#
#   MICRO
#   Site Class Declaration
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

                   # Constructor
                   initialize = function(){

                   }

                 ),

                 # private members
                 private = list(

                   # basic information about a site
                   siteXY                       = numeric(2),
                   siteID                       = integer(1),
                   siteType                     = integer(1),

                   # references to resources found at this site
                   site_f                       = list(), # std::vector<std::unique_ptr<FeedingSite>>
                   site_l                       = list(),
                   site_m                       = list(),
                   site_s                       = list(),

                   # weights of resouces found at this site
                   site_f_w                     = numeric(1), # std::vector<double>
                   site_l_w                     = numeric(1),
                   site_m_w                     = numeric(1),
                   site_s_w                     = numeric(1),

                   # tile pointer
                   tileP                        = NULL # tile* tileP;

                 )
)
