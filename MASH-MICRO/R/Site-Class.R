###############################################################################
#
#       __  ____                _____ _ __
#      /  |/  (_)_____________ / ___/(_) /____
#     / /|_/ / / ___/ ___/ __ \\__ \/ / __/ _ \
#    / /  / / / /__/ /  / /_/ /__/ / / /_/  __/
#   /_/  /_/_/\___/_/   \____/____/_/\__/\___/
#
#   MASH-MICRO
#   MICRO: Site Class Definitions
#   MASH-MICRO Team
#   September 6, 2017
#
###############################################################################

###############################################################################
# Blood Feeding Site
###############################################################################

#' Feeding Site Class Definition
#'
#' im a class!
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
FeedingSite <- R6::R6Class(classname = "FeedingSite",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(ix, siteXY, searchWt, enterP, hazV = 0, hazW = 0, hazI = 0, sugar = NULL, siteType = 1L){

                     private$ix = ix
                     private$siteXY = siteXY
                     private$searchWt = searchWt
                     private$hazV = hazV
                     private$hazW = hazW
                     private$hazI = hazI
                     private$sugar = sugar
                     private$enterP = enterP
                     private$siteType = siteType
                     private$RiskQ = MASHcpp::RiskQ()

                   }

                 ),

                 # private members
                 private = list(

                   # generic fields
                   ix = integer(1),
                   siteXY = numeric(2),
                   searchWt = integer(1),
                   siteType = integer(1),   # an aquatic or sugar site could be inside of a house, for example.

                   # FeedingSite fields
                   hazV = numeric(1),      # vegetation hazards
                   hazW = numeric(1),      # outside wall hazards
                   hazI = numeric(1),      # inside wall hazards
                   enterP = numeric(1),    # house entry probability
                   RiskQ = NULL,           # host risk queue

                   # Pointers
                   LandscapePointer = NULL

                 )
)


###############################################################################
# Aquatic Habitat Site
###############################################################################

#' Aquatic Habitat Class Definition
#'
#' im a class!
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
AquaticSite <- R6::R6Class(classname = "AquaticSite",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(ix, siteXY, searchWt, module, lambda = NULL, numGenotypes = 1, haz = 0, siteType = 0L){

                     # generic fields
                     private$ix = ix
                     private$siteXY = siteXY
                     private$searchWt = searchWt
                     private$haz = haz
                     private$siteType = siteType

                     # shared Aquatic Ecology fields
                     private$ImagoQ = MASHcpp::ImagoQ()

                     # Aquatic Ecology Emerge module fields
                     switch(module,
                       emerge = {
                         private$lambda = lambda
                        },
                        EL4P = {
                          private$numGenotypes = numGenotypes
                          private$EL4P = EL4P$new(N_genotypes = numGenotypes)
                          private$EggQ = MASHcpp::EggQ()
                          # private$EL4P = MASHcpp::EL4P(numGenotypes=numGenotypes,psi_new=0,alpha_new=0,p_new=0)
                        },
                        {stop("unrecognized Aquatic Ecology module")}
                      )

                   }

                 ),

                 # private members
                 private = list(

                   # generic fields
                   ix = integer(1),
                   siteXY = numeric(2),
                   searchWt = numeric(1),
                   siteType = integer(1),   # an aquatic or sugar site could be inside of a house, for example.

                   # AquaticSite fields
                   haz = numeric(1),
                   ImagoQ = NULL,
                   EggQ = NULL,
                   EL4P = NULL,
                   lambda = NULL,
                   numGenotypes = integer(1),

                   # Pointers
                   LandscapePointer = NULL

                 )
)


###############################################################################
# Mating Site
###############################################################################

#' Mating Site Class Definition
#'
#' im a class!
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
MatingSite <- R6::R6Class(classname = "MatingSite",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(ix, siteXY, searchWt, haz = 0, siteType = 0L){

                     private$ix = ix
                     private$siteXY = siteXY
                     private$searchWt = searchWt
                     private$haz = haz
                     private$siteType = siteType
                     private$MatingQ = MASHcpp::MatingQ()

                   }

                 ),

                 # private members
                 private = list(

                   # generic fields
                   ix = integer(1),
                   siteXY = numeric(2),
                   searchWt = integer(1),
                   siteType = integer(1),   # an aquatic or sugar site could be inside of a house, for example.

                   # MatingSite fields
                   haz = numeric(1),
                   MatingQ = NULL,           # host risk queue

                   # Pointers
                   LandscapePointer = NULL,

                   # Vector Control
                   aerialSpray=NULL,
                   swarmSpray=NULL,
                   areaRepellant=NULL
                 )
)


###############################################################################
# Sugar Feeding Site
###############################################################################

#' Sugar Feeding Site Class Definition
#'
#' im a class!
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
SugarSite <- R6::R6Class(classname = "SugarSite",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(ix, siteXY, searchWt, haz = 0, siteType = 0L){

                     private$ix = ix
                     private$siteXY = siteXY
                     private$searchWt = searchWt
                     private$haz = haz
                     private$siteType = siteType

                   }

                 ),

                 # private members
                 private = list(

                   # generic fields
                   ix = integer(1),
                   siteXY = numeric(2),
                   searchWt = integer(1),
                   siteType = integer(1),   # an aquatic or sugar site could be inside of a house, for example.

                   # MatingSite fields
                   haz = numeric(1),

                   # Pointers
                   LandscapePointer = NULL,

                   # Vector Control
                   attractiveSugarBait=NULL,
                   aerialSpray=NULL
                 )
)
