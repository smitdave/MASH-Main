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


###############################################################################
# Abstract Base Mosquito
###############################################################################

#' MBITES: Mosquito Class
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

                   # begin constructor
                   initialize = function(female, b_day, site, tileID){

                     private$id = MBITES:::Globals$get_mosquito_id()

                     private$site = site
                     private$tileID = tileID

                     private$b_day = b_day
                     private$t_now = b_day
                     private$t_next = b_day

                     private$search = FALSE

                   } # end constructor

                 ),

                 # private members
                 private = list(

                   # basic parameters
                   id             = integer(1), # character id

                   # location
                   tileID         = integer(1), # id of the tile i am in
                   site           = NULL, # reference to my current site
                   rspot          = character(1), # my current resting spot

                   # resources
                   sugar_res      = NULL, # reference to my current sugar resource
                   mate_res       = NULL, # reference to my current mating swarm resource

                   # timing
                   b_day          = numeric(1), # the day i emerged
                   t_next         = numeric(1), # time of my next launch
                   t_now          = numeric(1), # time of my current launch

                   # behavioral state parameters
                   search         = logical(1), # next launch is for search or attempt bout?
                   state          = character(1), # my current behavioral state
                   starved        = logical(1), # am i starved for sugar?
                   # gravid         = logical(1), # am i gravid to oviposit?
                   bout_fail      = integer(1), # counter

                   # energetics
                   energy         = numeric(1), # my current energy
                   # energy_preG    = numeric(1), # pre-gonotrophic energy requirement
                   mature         = logical(1), # am i mature?

                   # survival
                   damage_physical = numeric(1), # physical damage
                   damage_chemical = numeric(1), # chemical damage

                   # bloodfeeding and oogenesis
                   # batch          = integer(1), # size of my egg batch
                   # bm_size        = numeric(1), # size of my blood meal

                   # mate ids
                   mateID         = integer(1) # id of my mate

                 )
)

# get_id


###############################################################################
# Female Mosquito
###############################################################################

#' MBITES: Female Mosquito Class
#'
#' Female mosquitoes inherit from the \code{\link[MBITES]{Mosquito}} abstract base class object.
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
Mosquito_Female <- R6::R6Class(classname = "Mosquito_Female",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::Mosquito,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(b_day, site, tileID){

                     super$initialize(TRUE, b_day,site,tileID) # construct the base-class parts

                     private$state = MBITES:::Parameters$get_female_state()

                     private$energy_preG = MBITES:::Parameters$get_energy_preG()

                   } # end constructor

                 ),

                 # private members
                 private = list(

                   # resources
                   aqua_res       = NULL, # reference to my current aquatic habitat resource
                   feed_res       = NULL, # reference to my current blood feeding resource

                   # behavioral state parameters
                   gravid         = logical(1), # am i gravid to oviposit?

                   # energetics
                   energy_preG    = numeric(1), # pre-gonotrophic energy requirement

                   # bloodfeeding and oogenesis
                   batch          = integer(1), # size of my egg batch
                   bm_size        = numeric(1), # size of my blood meal

                   # host ids
                   hostID         = integer(1) # id of my blood host

                 )
)


###############################################################################
# Male Mosquito
###############################################################################

#' MBITES: Male Mosquito Class
#'
#' Male mosquitoes inherit from the \code{\link[MBITES]{Mosquito}} abstract base class object.
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
Mosquito_Male <- R6::R6Class(classname = "Mosquito_Male",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::Mosquito,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(b_day, site, tileID){

                     super$initialize(FALSE,b_day,site,tileID) # construct the base-class parts


                   } # end constructor

                 ),

                 # private members
                 private = list()
)
