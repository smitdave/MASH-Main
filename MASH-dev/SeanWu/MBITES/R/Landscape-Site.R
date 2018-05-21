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
#'  * move_id: integer vector of outbound transition id's to other \code{\link{Site}} objects on the landscape
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
                   initialize = function(id, xy, tileID, type, move, move_id, haz){
                     futile.logger::flog.trace("Site %i being born at: self %s , private %s",id,pryr::address(self),pryr::address(private))

                     private$id = id
                     private$xy = xy
                     private$tileID = tileID
                     private$type = type
                     private$move = move
                     private$move_id = move_id
                     private$haz = haz

                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("Site %i being killed at: self %s , private %s",private$id,pryr::address(self),pryr::address(private))

                     private$resource_feeding = NULL
                     private$resource_aquatic = NULL
                     private$resource_sugar = NULL
                     private$resource_mating = NULL
                     invisible(gc())
                   } # end destructor

                 ), # end public members

                 # private members
                 private = list(

                   # site characteristics
                   id             = integer(1), # integer id
                   xy             = numeric(2), # xy-coordinates
                   tileID         = integer(1), # integer id of the tile this site is in
                   move           = numeric(1), # 'row' of the transition matrix for movement from this site to all other sites
                   move_id        = integer(1), # id's of sites associated with the 'move' transition probabilities
                   type           = integer(1), # type of site (for matrix of weights controlling where resting occurs)
                   haz            = numeric(1), # baseline mosquito resting hazard

                   # resources
                   # has resources?
                   has_feed_b     = FALSE,
                   has_aqua_b     = FALSE,
                   has_sugar_b    = FALSE,
                   has_mate_b     = FALSE,
                   # references and weights
                   resource_feeding       = list(), # list of references to 'feeding'-type resources
                   resource_feeding_w     = NULL, # weights of 'feeding'-type resources
                   resource_aquatic       = list(), # list of references to 'aqua'-type resources
                   resource_aquatic_w     = NULL, # weights of 'aqua'-type resources
                   resource_sugar      = list(), # list of references to 'sugar'-type resources
                   resource_sugar_w    = NULL, # weights of 'sugar'-type resources
                   resource_mating       = list(), # list of references to 'mate'-type resources
                   resource_mating_w     = NULL # weights of 'mate'-type resources

                 ) # end private members
) # end Site class definition


###############################################################################
# Activity Space: needed in Tile-Simulation
###############################################################################

#' clear activity space between days
clear_ActivitySpace_Site <- function(){
  # if this site has blood feeding sites
  n = length(private$resource_feeding)
  if(n > 0){
    for(i in 1:n){
      private$resource_feeding[[i]]$RiskQ$clearQ()
    }
  }
}

# set methods
Site$set(which = "public",name = "clear_ActivitySpace",
    value = clear_ActivitySpace_Site, overwrite = TRUE
)


###############################################################################
# Aquatic Ecology: needed in Tile-Simulation
###############################################################################

#' Site: One Day of Aquatic Ecology Dynamics
#'
#' If this site has \code{\link{Aqua_Resource}} (aquatic habitats) present, run the daily \code{one_day} and
#' \code{push_imago} functions that all aquatic habitat resource objects must implement.
#'  * binding: \code{Site$oneDay_AquaticEcology}
#'
oneDay_AquaticEcology_Site <- function(){
  # if this site has aquatic habitats
  n = length(private$resource_aquatic)
  if(n > 0){
    for(i in 1:n){
      private$resource_aquatic[[i]]$one_day()
      private$resource_aquatic[[i]]$push_imago()
    }
  }
}

#' reset between runs
reset_Site <- function(){
  n_aqua <- length(private$resource_aquatic)
  n_feed <- length(private$resource_feeding)
  n_mate <- length(private$resource_mating)

  # reset aquatic habitats
  if(n_aqua>0){
    for(i in 1:n_aqua){
      private$resource_aquatic[[i]]$reset()
    }
  }

  # reset blood feeding sites
  if(n_feed>0){
    for(i in 1:n_feed){
      private$resource_feeding[[i]]$reset()
    }
  }

  # reset mating sites
  if(n_mate>0){
    for(i in 1:n_mate){
      private$resource_mating[[i]]$reset()
    }
  }
}

# set methods
Site$set(which = "public",name = "oneDay_AquaticEcology",
    value = oneDay_AquaticEcology_Site, overwrite = TRUE
)

Site$set(which = "public",name = "reset",
    value = reset_Site, overwrite = TRUE
)


###############################################################################
# movement; when a mosquito calls 'search' return a new site it moves to
###############################################################################

#' Site: Move Mosquito
#'
#' Sample from the movement kernel centered on this site, returns a reference to the destination \code{\link{Site}} object.
#'  * binding: \code{Site$move_mosquito}
#'
move_mosquito_Site <- function(){
  ix = MBITES::sample(x=private$move_id,size=1L,replace=FALSE,prob=private$move)
  return(MBITES:::Globals$get_tile(private$tileID)$get_sites()$get(ix))
}

Site$set(which = "public",name = "move_mosquito",
    value = move_mosquito_Site, overwrite = TRUE
)


###############################################################################
# Sample Resources
###############################################################################

#' Site: Sample Blood Feeding Resources
#'
#' Sample a blood feeding resource at this site, returns a reference to a \code{\link{Feeding_Resource}} object.
#'  * binding: \code{Site$sample_feed}
#'
sample_feed_Site <- function(){
  MBITES::sample(x=private$resource_feeding,size=1L,replace=FALSE,prob=private$resource_feeding_w)[[1]]
}

#' Site: Sample Aquatic Habitat Resources
#'
#' Sample a aquatic habitat resource at this site, returns a reference to a \code{\link{Aqua_Resource}} object.
#'  * binding: \code{Site$sample_aqua}
#'
sample_aqua_Site <- function(){
  MBITES::sample(x=private$resource_aquatic,size=1L,replace=FALSE,prob=private$resource_aquatic_w)[[1]]
}

#' Site: Sample Sugar Feeding Resources
#'
#' Sample a sugar feeding resource at this site, returns a reference to a \code{\link{Sugar_Resource}} object.
#'  * binding: \code{Site$sample_sugar}
#'
sample_sugar_Site <- function(){
  MBITES::sample(x=private$resource_sugar,size=1L,replace=FALSE,prob=private$resource_sugar_w)[[1]]
}

#' Site: Sample Mating Swarm Resources
#'
#' Sample a mating swarm resource at this site, returns a reference to a \code{\link{Mating_Resource}} object.
#'  * binding: \code{Site$sample_mate}
#'
sample_mate_Site <- function(){
  MBITES::sample(x=private$resource_mating,size=1L,replace=FALSE,prob=private$resource_mating_w)[[1]]
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
# Check Resources
###############################################################################

#' Site: Check Blood Feeding Resources
#'
#' Check if any \code{\link{Feeding_Resource}} are present at this site. This is queried by mosquitoes during \code{\link{mbites_checkForResources}}.
#'  * binding: \code{Site$has_feed}
#'
has_feed_Site <- function(){
  return(private$has_feed_b)
}

#' Site: Check Aquatic Habitat Resources
#'
#' Check if any \code{\link{Aqua_Resource}} are present at this site. This is queried by mosquitoes during \code{\link{mbites_checkForResources}}.
#'  * binding: \code{Site$has_aqua}
#'
has_aqua_Site <- function(){
  return(private$has_aqua_b)
}

#' Site: Check Sugar Feeding Resources
#'
#' Check if any \code{\link{Sugar_Resource}} are present at this site. This is queried by mosquitoes during \code{\link{mbites_checkForResources}}.
#'  * binding: \code{Site$has_sugar}
#'
has_sugar_Site <- function(){
  return(private$has_sugar_b)
}

#' Site: Check Mating Swarm Resources
#'
#' Check if any \code{\link{Mating_Resource}} are present at this site. This is queried by mosquitoes during \code{\link{mbites_checkForResources}}.
#'  * binding: \code{Site$has_mate}
#'
has_mate_Site <- function(){
  return(private$has_mate_b)
}

Site$set(which = "public",name = "has_feed",
    value = has_feed_Site, overwrite = TRUE
)

Site$set(which = "public",name = "has_aqua",
    value = has_aqua_Site, overwrite = TRUE
)

Site$set(which = "public",name = "has_sugar",
    value = has_sugar_Site, overwrite = TRUE
)

Site$set(which = "public",name = "has_mate",
    value = has_mate_Site, overwrite = TRUE
)


###############################################################################
# Add Resources
###############################################################################

#' Site: Add a Blood Feeding Resource
#'
#' Add a blood feeding resource to this site.
#'  * binding: \code{Site$add_feed}
#'
#' @param Feeding_Resource a reference to a \code{\link{Feeding_Resource}} object
#'
add_feed_Site <- function(Feeding_Resource){
  private$has_feed_b = TRUE
  private$resource_feeding = append(private$resource_feeding,Feeding_Resource)
  private$resource_feeding_w = append(private$resource_feeding_w,Feeding_Resource$get_w())
}

#' Site: Add a Aquatic Habitat Resource
#'
#' Add a aquatic habitat resource to this site.
#'  * binding: \code{Site$add_aqua}
#'
#' @param Aqua_Resource a reference to an object deriving from \code{\link{Aqua_Resource}}
#'
add_aqua_Site <- function(Aqua_Resource){
  private$has_aqua_b = TRUE
  private$resource_aquatic = append(private$resource_aquatic,Aqua_Resource)
  private$resource_aquatic_w = append(private$resource_aquatic_w,Aqua_Resource$get_w())
}

#' Site: Add a Sugar Feeding Resource
#'
#' Add a sugar feeding resource to this site.
#'  * binding: \code{Site$add_sugar}
#'
#' @param Sugar_Resource a reference to a \code{\link{Sugar_Resource}} object
#'
add_sugar_Site <- function(Sugar_Resource){
  private$has_sugar_b = TRUE
  private$resource_sugar = append(private$resource_sugar,Sugar_Resource)
  private$resource_sugar_w = append(private$resource_sugar_w,Sugar_Resource$get_w())
}

#' Site: Add a Mating Resource
#'
#' Add a mating resource to this site.
#'  * binding: \code{Site$add_mate}
#'
#' @param Mating_Resource a reference to a \code{\link{Mating_Resource}} object
#'
add_mate_Site <- function(Mating_Resource){
  private$has_mate_b = TRUE
  private$resource_mating = append(private$resource_mating,Mating_Resource)
  private$resource_mating_w = append(private$resource_mating_w,Mating_Resource$get_w())
}

Site$set(which = "public",name = "add_feed",
    value = add_feed_Site, overwrite = TRUE
)

Site$set(which = "public",name = "add_aqua",
    value = add_aqua_Site, overwrite = TRUE
)

Site$set(which = "public",name = "add_sugar",
    value = add_sugar_Site, overwrite = TRUE
)

Site$set(which = "public",name = "add_mate",
    value = add_mate_Site, overwrite = TRUE
)


###############################################################################
# Getters & Setters
###############################################################################

# Basic attributes of a site

#' Site: Site ID
#'
#' Return the id of this site.
#'  * binding: \code{Site$get_id}
#'
get_id_Site <- function(){
  return(private$id)
}

#' Site: Return ID of Tile this Site is in
#'
#' Return the id to the enclosing \code{\link{Tile}}
#'  * binding: \code{Site$get_tileID}
#'
get_tileID_Site <- function(){
  return(private$tileID)
}

#' Site: Return Type of this Site
#'
#' Return the integer type of this site.
#'  * binding: \code{Site$get_type}
#'
get_type_Site <- function(){
  return(private$type)
}

#' Site: Get Resting Hazard
#'
#' Return the resting hazard of this site.
#'  * binding: \code{Site$get_haz}
#'
get_haz_Site <- function(){
  return(private$haz)
}

Site$set(which = "public",name = "get_id",
    value = get_id_Site, overwrite = TRUE
)

Site$set(which = "public",name = "get_tileID",
    value = get_tileID_Site, overwrite = TRUE
)

Site$set(which = "public",name = "get_type",
    value = get_type_Site, overwrite = TRUE
)

Site$set(which = "public",name = "get_haz",
    value = get_haz_Site, overwrite = TRUE
)

# Feeding Resource accessors

#' Site: Return Blood Feeding Resource Reference
#'
#' Return a reference to the i'th \code{\link{Feeding_Resource}}
#'  * binding: \code{Site$get_feed}
#'
#' @param i integer index of resource to return
#'
get_feed_Site <- function(i){
  return(private$resource_feeding[[i]])
}

#' Site: Return Blood Feeding Resource Weights
#'
#' Return a numeric vector of \code{\link{Feeding_Resource}} weights at this site.
#'  * binding: \code{Site$get_feed_w}
#'
get_feed_w_Site <- function(){
  return(private$resource_feeding_w)
}

Site$set(which = "public",name = "get_feed",
    value = get_feed_Site, overwrite = TRUE
)

Site$set(which = "public",name = "get_feed_w",
    value = get_feed_w_Site, overwrite = TRUE
)
