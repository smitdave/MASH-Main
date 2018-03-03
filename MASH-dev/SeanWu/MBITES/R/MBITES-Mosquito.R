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

                   ##############################################################
                   # constructor
                   ##############################################################

                   initialize = function(id, b_day){
                     private$id = id
                     private$b_day = b_day
                     private$t_now = b_day
                   } # end constructor

                 ),

                 # private members
                 private = list(

                   # basic parameters
                   id             = integer(1), # character id

                   # location
                   site           = NULL, # reference to my current site
                   rspot          = character(1), # my current resting spot


                   # timing
                   b_day          = numeric(1), # the day i emerged
                   t_next         = numeric(1), # time of my next launch
                   t_now          = numeric(1), # time of my current launch

                   # behavioral state parameters
                   search         = logical(1), # next launch is for search or attempt bout?
                   state          = character(1), # my current behavioral state
                   starved        = logical(1), # am i starved for sugar?
                   gravid         = logical(1), # am i gravid to oviposit?
                   bout_fail      = logical(1), # did i fail my current bout?

                   # energetics
                   energy         = numeric(1), # my current energy
                   energy_preG    = numeric(1), # pre-gonotrophic energy requirement
                   mature         = logical(1), # am i mature?

                   # survival
                   damage_physical = numeric(1), # physical damage
                   damage_chemical = numeric(1), # chemical damage

                   # bloodfeeding and oogenesis
                   batch          = integer(1), # size of my egg batch
                   bm_size        = numeric(1), # size of my blood meal

                   # resource ids
                   hostID         = integer(1), # id of my blood host
                   habitatID      = integer(1), # id of my aquatic habitat
                   sugarID        = integer(1), # id of my sugar source
                   mateID         = integer(1) # id of my mate

                 )


)
