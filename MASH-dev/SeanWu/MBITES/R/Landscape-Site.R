###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Point
#     MBITES Team
#     February 2018
#
###############################################################################


#' Landscape Site Class
#'
#' A landscape consists of a set of \code{Site} objects, each of which may have one or more resources present.
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
#'  * id: integer identifier of site
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

                   # begin constructor
                   initialize = function(id, xy, move){

                     private$id = id
                     private$xy = xy
                     private$move = move

                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     rm(res_feed)
                     rm(res_aqua)
                     rm(res_sugar)
                     rm(res_mate)
                     invisible(gc())
                   } # end destructor

                 ), # end public members

                 # private members
                 private = list(

                   # site characteristics
                   id             = integer(1), # integer id
                   xy             = numeric(2), # xy-coordinates
                   move           = numeric(1), # 'row' of the transition matrix for movement from this site to all other sites

                   # resources
                   res_feed       = list(), # list of references to 'feeding'-type resources
                   res_feed_w     = numeric(1), # weights of 'feeding'-type resources
                   res_aqua       = list(), # list of references to 'aqua'-type resources
                   res_aqua_w     = numeric(1), # weights of 'aqua'-type resources
                   res_sugar      = list(), # list of references to 'sugar'-type resources
                   res_sugar_w    = numeric(1), # weights of 'sugar'-type resources
                   res_mate       = list(), # list of references to 'mate'-type resources
                   res_mate_w     = numeric(1) # weights of 'mate'-type resources

                 ) # end private members
) # end Site class definition


###############################################################################
# Methods
###############################################################################

#' Site: Add a Blood Feeding Resource
#'
#' Add a blood feeding resource to this site.
#'  * binding: \code{Site$set_feed}
#'
#' @param Feeding_Resource a reference to a \code{\link[MBITES]{Feeding_Resource}} object
#'
set_feed_Site <- function(Feeding_Resource){
  private$res_feed = append(private$res_feed,Feeding_Resource)
  private$res_feed_w = append(private$res_feed_w,Feeding_Resource$get_w())
}

Site$set(which = "public",name = "set_feed",
    value = set_feed_Site, overwrite = TRUE
)


#' Site: Add a Aquatic Habitat Resource
#'
#' Add a aquatic habitat resource to this site.
#'  * binding: \code{Site$set_aqua}
#'
#' @param Aqua_Resource_Base a reference to an object deriving from \code{\link[MBITES]{Aqua_Resource_Base}}
#'
set_aqua_Site <- function(Aqua_Resource_Base){
  private$res_aqua = append(private$res_aqua,Aqua_Resource_Base)
  private$res_aqua_w = append(private$res_aqua_w,Aqua_Resource_Base$get_w())
}

Site$set(which = "public",name = "set_aqua",
    value = set_aqua_Site, overwrite = TRUE
)


#' Site: Add a Sugar Feeding Resource
#'
#' Add a sugar feeding resource to this site.
#'  * binding: \code{Site$set_sugar}
#'
#' @param Sugar_Resource a reference to a \code{\link[MBITES]{Sugar_Resource}} object
#'
set_sugar_Site <- function(Sugar_Resource){
  private$res_sugar = append(private$res_sugar,Sugar_Resource)
  private$res_sugar_w = append(private$res_sugar_w,Sugar_Resource$get_w())
}

Site$set(which = "public",name = "set_sugar",
    value = set_sugar_Site, overwrite = TRUE
)


#' Site: Add a Mating Resource
#'
#' Add a mating resource to this site.
#'  * binding: \code{Site$set_mate}
#'
#' @param Mating_Resource a reference to a \code{\link[MBITES]{Mating_Resource}} object
#'
set_mate_Site <- function(Mating_Resource){
  private$res_mate = append(private$res_mate,Mating_Resource)
  private$res_mate_w = append(private$res_mate_w,Mating_Resource$get_w())
}

Site$set(which = "public",name = "set_mate",
    value = set_mate_Site, overwrite = TRUE
)
