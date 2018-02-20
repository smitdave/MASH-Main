###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Landscape-Resource-Aquatic Habitat
#     MBITES Team
#     February 2018
#
###############################################################################


#' Landscape Aquatic Habitat Resource Class
#'
#' A \code{Aqua_Resource} is a type of resource at a \code{\link[MBITES]{Site}} where mosquitoes travel for oviposition of eggs
#' and from which new imagos (adult mosquitoes) emerge from. Dynamics of aquatic habitats are simulated by a reference (pimpl idiom)
#' to the specific model chosen.
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
Aqua_Resource <- R6::R6Class(classname = "Aqua_Resource",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(){

                   }, # end constructor

                   # one day population dynamics
                   one_day = function(){
                     # add newly oviposited egg batches to aquatic population
                    # private$EggQ = private$AquaPopP$add_egg(private$EggQ)

                     # run one day simulation of aquatic population
                     # private$AquaPopP$one_day()

                     # get emerging imagos
                     # private$ImagoQ = private$AquaPopP$get_imago()

                     # add emerging adults to mosquito population
                     # private$SiteP$get_TilePointer()$get_MosquitoPop()$add_Imago(private$ImagoQ)
                   }

                 ),

                 # private members
                 private = list(

                   AquaPopP            = NULL, # reference (pimpl) to aquatic dynamic model
                   EggQ                = list(), # list of egg batches
                   ImagoQ              = list(), # list of newly emerging imagos
                   SiteP               = NULL # pointer to my enclosing Site (has-a relationship; Sites manage Resource lifespans)


                 )


)
