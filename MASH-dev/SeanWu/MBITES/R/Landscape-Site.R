###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Site
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
#'  * id: integer id of site
#'  * xy: numeric vector of coordinates
#'  * move: numeric vector of outbound transition probabilities
#'  * move_id: integer vector of outbound transition id's to other \code{\link[MBITES]{Site}} objects on the landscape
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
                   initialize = function(id, xy, type, move, move_id){

                     private$id = id
                     private$xy = xy
                     private$type = type
                     private$move = move
                     private$move_id = move_id

                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     private$res_feed = NULL
                     private$res_aqua = NULL
                     private$res_sugar = NULL
                     private$res_mate = NULL
                     invisible(gc())
                   } # end destructor

                 ), # end public members

                 # private members
                 private = list(

                   # site characteristics
                   id             = integer(1), # integer id
                   xy             = numeric(2), # xy-coordinates
                   move           = numeric(1), # 'row' of the transition matrix for movement from this site to all other sites
                   move_id        = integer(1), # id's of sites associated with the 'move' transition probabilities
                   type           = integer(1), # type of site (for matrix of weights controlling where resting occurs)

                   # resources
                   res_feed       = list(), # list of references to 'feeding'-type resources
                   res_feed_w     = NULL, # weights of 'feeding'-type resources
                   res_aqua       = list(), # list of references to 'aqua'-type resources
                   res_aqua_w     = NULL, # weights of 'aqua'-type resources
                   res_sugar      = list(), # list of references to 'sugar'-type resources
                   res_sugar_w    = NULL, # weights of 'sugar'-type resources
                   res_mate       = list(), # list of references to 'mate'-type resources
                   res_mate_w     = NULL # weights of 'mate'-type resources

                 ) # end private members
) # end Site class definition


###############################################################################
# movement; when a mosquito calls 'search' return a new site it moves to
###############################################################################

#' Site: Move Mosquito
#'
#' Sample from the movement kernel centered on this site, returns a reference to the destination \code{\link[MBITES]{Site}} object.
#'  * binding: \code{Site$move_mosquito}
#'
move_mosquito_Site <- function(){
  ix = MBITES::sample(x=private$move_id,size=1L,replace=FALSE,prob=private$move)
  return(MBITES:::Globals$get_tile()$get_Site(ix))
}

Site$set(which = "public",name = "move_mosquito",
    value = move_mosquito_Site, overwrite = TRUE
)


###############################################################################
# Sample Resources
###############################################################################

#' Site: Sample Blood Feeding Resources
#'
#' Sample a blood feeding resource at this site, returns a reference to a \code{\link[MBITES]{Feeding_Resource}} object.
#'  * binding: \code{Site$sample_feed}
#'
sample_feed_Site <- function(){
  MBITES::sample(x=private$res_feed,size=1L,replace=FALSE,prob=private$res_feed_w)
}

#' Site: Sample Aquatic Habitat Resources
#'
#' Sample a aquatic habitat resource at this site, returns a reference to a \code{\link[MBITES]{Aqua_Resource}} object.
#'  * binding: \code{Site$sample_aqua}
#'
sample_aqua_Site <- function(){
  MBITES::sample(x=private$res_aqua,size=1L,replace=FALSE,prob=private$res_aqua_w)
}

#' Site: Sample Sugar Feeding Resources
#'
#' Sample a sugar feeding resource at this site, returns a reference to a \code{\link[MBITES]{Sugar_Resource}} object.
#'  * binding: \code{Site$sample_sugar}
#'
sample_sugar_Site <- function(){
  MBITES::sample(x=private$res_sugar,size=1L,replace=FALSE,prob=private$res_sugar_w)
}

#' Site: Sample Mating Swarm Resources
#'
#' Sample a mating swarm resource at this site, returns a reference to a \code{\link[MBITES]{Mating_Resource}} object.
#'  * binding: \code{Site$sample_mate}
#'
sample_mate_Site <- function(){
  MBITES::sample(x=private$res_mate,size=1L,replace=FALSE,prob=private$res_mate_w)
}

Site$set(which = "public",name = "sample_feed",
    value = sample_feed_Site, overwrite = TRUE
)

Site$set(which = "public",name = "sample_aqua",
    value = sample_aqua_Site, overwrite = TRUE
)

Site$set(which = "public",name = "sample_sugar",
    value = sample_sugar_Site, overwrite = TRUE
)

Site$set(which = "public",name = "sample_mate",
    value = sample_mate_Site, overwrite = TRUE
)


###############################################################################
# Add Resources
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
#' @param Aqua_Resource a reference to an object deriving from \code{\link[MBITES]{Aqua_Resource}}
#'
set_aqua_Site <- function(Aqua_Resource){
  private$res_aqua = append(private$res_aqua,Aqua_Resource)
  private$res_aqua_w = append(private$res_aqua_w,Aqua_Resource$get_w())
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


###############################################################################
# Getters & Setters
###############################################################################

#' Site: Return Tile Reference
#'
#' Return a reference to the enclosing \code{\link[MBITES]{Tile}}
#'  * binding: \code{Site$get_tile}
#'
get_tile_Site <- function(){
  return(private$TileP)
}

Site$set(which = "public",name = "get_tile",
    value = get_tile_Site, overwrite = TRUE
)

# Feeding Resource accessors

#' Site: Return Blood Feeding Resource Reference
#'
#' Return a reference to the i'th \code{\link[MBITES]{Feeding_Resource}}
#'  * binding: \code{Site$get_feed}
#'
#' @param i integer index of resource to return
#'
get_feed_Site <- function(i){
  return(private$res_feed[[i]])
}

#' Site: Return Blood Feeding Resource Weights
#'
#' Return a numeric vector of \code{\link[MBITES]{Feeding_Resource}} weights at this site.
#'  * binding: \code{Site$get_feed_w}
#'
get_feed_w_Site <- function(){
  return(private$res_feed_w)
}

Site$set(which = "public",name = "get_feed",
    value = get_feed_Site, overwrite = TRUE
)

Site$set(which = "public",name = "get_feed_w",
    value = get_feed_w_Site, overwrite = TRUE
)
