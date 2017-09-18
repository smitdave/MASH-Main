###############################################################################
#       __                    __
#      / /   ____ _____  ____/ /_____________ _____  ___
#     / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#    / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#   /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                          /_/
#
#   MASH-MICRO
#   MICRO: Landscape Methods Definition
#   MASH-MICRO Team
#   September 9, 2017
#
###############################################################################


###############################################################################
# Getters & Setters
###############################################################################

#' Get Blood Feeding Sites
#'
#' Return single \code{\link{FeedingSite}} or all sites.
#'  * This method is bound to \code{Landscape$get_FeedingSites}
#'
#' @param ixS integer index; if \code{NULL} return entire list of sites
#'
get_FeedingSites_Landscape <- function(ixS = NULL){
  if(is.null(ixS)){
    return(private$FeedingSites)
  } else {
    return(private$FeedingSites[[ixS]])
  }
}

Landscape$set(which = "public",name = "get_FeedingSites",
  value = get_FeedingSites_Landscape, overwrite = TRUE
)

#' Set Blood Feeding Sites
#'
#' Set a single \code{\link{FeedingSite}} or all sites.
#'  * This method is bound to \code{Landscape$set_FeedingSites}
#'
#' @param FeedingSites input may be a single \code{\link{FeedingSite}} or list of sites
#' @param ixS integer index; if \code{NULL} set entire list of sites
#'
set_FeedingSites_Landscape <- function(FeedingSites, ixS = NULL){
  if(is.null(ixS)){
    private$FeedingSites = FeedingSites
  } else {
    private$FeedingSites[[ixS]] = FeedingSites
  }
}

Landscape$set(which = "public",name = "set_FeedingSites",
  value = set_FeedingSites_Landscape, overwrite = TRUE
)

#' Get Aquatic Habitat Sites
#'
#' Return single \code{\link{AquaticSite}} or all sites.
#'  * This method is bound to \code{Landscape$get_AquaSites}
#'
#' @param ixS integer index; if \code{NULL} return entire list of sites
#'
get_AquaSites_Landscape <- function(ixS = NULL){
  if(is.null(ixS)){
    return(private$AquaSites)
  } else {
    return(private$AquaSites[[ixS]])
  }
}

Landscape$set(which = "public",name = "get_AquaSites",
  value = get_AquaSites_Landscape, overwrite = TRUE
)

#' Set Aquatic Habitat Sites
#'
#' Set a single \code{\link{AquaticSite}} or all sites.
#'  * This method is bound to \code{Landscape$set_AquaSites}
#'
#' @param AquaSites input may be a single \code{\link{AquaticSite}} or list of sites
#' @param ixS integer index; if \code{NULL} set entire list of sites
#'
set_AquaSites_Landscape <- function(AquaSites, ixS = NULL){
  if(is.null(ixS)){
    private$AquaSites = AquaSites
  } else {
    private$AquaSites[[ixS]] = AquaSites
  }
}

Landscape$set(which = "public",name = "set_AquaSites",
  value = set_AquaSites_Landscape, overwrite = TRUE
)


#' Get Sugar Feeding Sites
#'
#' Return single \code{\link{SugarSite}} or all sites.
#'  * This method is bound to \code{Landscape$get_SugarSites}
#'
#' @param ixS integer index; if \code{NULL} return entire list of sites
#'
get_SugarSites_Landscape <- function(ixS = NULL){
  if(is.null(ixS)){
    return(private$SugarSites)
  } else {
    return(private$SugarSites[[ixS]])
  }
}

Landscape$set(which = "public",name = "get_SugarSites",
  value = get_SugarSites_Landscape, overwrite = TRUE
)

#' Set Sugar Feeding Sites
#'
#' Set a single \code{\link{SugarSite}} or all sites.
#'  * This method is bound to \code{Landscape$set_SugarSites}
#'
#' @param SugarSites input may be a single \code{\link{SugarSite}} or list of sites
#' @param ixS integer index; if \code{NULL} set entire list of sites
#'
set_SugarSites_Landscape <- function(SugarSites, ixS = NULL){
  if(is.null(ixS)){
    private$SugarSites = SugarSites
  } else {
    private$SugarSites[[ixS]] = SugarSites
  }
}

Landscape$set(which = "public",name = "set_SugarSites",
  value = set_SugarSites_Landscape, overwrite = TRUE
)


#' Get Mating Sites
#'
#' Return single \code{\link{MatingSite}} or all sites.
#'  * This method is bound to \code{Landscape$get_MatingSites}
#'
#' @param ixS integer index; if \code{NULL} return entire list of sites
#'
get_MatingSites_Landscape <- function(ixS = NULL){
  if(is.null(ixS)){
    return(private$MatingSites)
  } else {
    return(private$MatingSites[[ixS]])
  }
}

Landscape$set(which = "public",name = "get_MatingSites",
  value = get_MatingSites_Landscape, overwrite = TRUE
)

#' Set Mating Sites
#'
#' Set a single \code{\link{MatingSite}} or all sites.
#'  * This method is bound to \code{Landscape$set_MatingSites}
#'
#' @param MatingSites input may be a single \code{\link{MatingSite}} or list of sites
#' @param ixS integer index; if \code{NULL} set entire list of sites
#'
set_MatingSites_Landscape <- function(MatingSites, ixS = NULL){
  if(is.null(ixS)){
    private$MatingSites = MatingSites
  } else {
    private$MatingSites[[ixS]] = MatingSites
  }
}

Landscape$set(which = "public",name = "set_MatingSites",
  value = set_MatingSites_Landscape, overwrite = TRUE
)

#' Get Number of Blood Feeding Sites
#'
#'  * This method is bound to \code{Landscape$get_FeedingSitesN}
#'
get_FeedingSitesN_Landscape <- function(){return(private$FeedingSitesN)}

Landscape$set(which = "public",name = "get_FeedingSitesN",
  value = get_FeedingSitesN_Landscape, overwrite = TRUE
)

#' Get Number of Aquatic Habitat Sites
#'
#'  * This method is bound to \code{Landscape$get_AquaSitesN}
#'
get_AquaSitesN_Landscape <- function(){return(private$AquaSitesN)}

Landscape$set(which = "public",name = "get_AquaSitesN",
  value = get_AquaSitesN_Landscape, overwrite = TRUE
)

#' Get Number of Sugar Feeding Sites
#'
#'  * This method is bound to \code{Landscape$get_SugarSitesN}
#'
get_SugarSitesN_Landscape <- function(){return(private$SugarSitesN)}

Landscape$set(which = "public",name = "get_SugarSitesN",
  value = get_SugarSitesN_Landscape, overwrite = TRUE
)

#' Get Number of Mating Sites
#'
#'  * This method is bound to \code{Landscape$get_MatingSitesN}
#'
get_MatingSitesN_Landscape <- function(){return(private$MatingSitesN)}

Landscape$set(which = "public",name = "get_MatingSitesN",
  value = get_MatingSitesN_Landscape, overwrite = TRUE
)


###############################################################################
# Pointers
###############################################################################

#' Get \code{\link{Tile}} Pointer
#'
#' Return pointer to enclosing \code{Tile$self}.
#'  * This method is bound to \code{Landscape$get_TilePointer}
#'
get_TilePointer_Landscape <- function(){
  return(private$TilePointer)
}

Landscape$set(which = "public",name = "get_TilePointer",
  value = get_TilePointer_Landscape, overwrite = TRUE
)

#' Set \code{\link{Tile}} Pointer
#'
#' Set pointer to enclosing \code{Tile$self}.
#'  * This method is bound to \code{Landscape$set_TilePointer}
#'
#' @param TilePointer an environment
#'
set_TilePointer_Landscape <- function(TilePointer){
  private$TilePointer = TilePointer
}

Landscape$set(which = "public",name = "set_TilePointer",
  value = set_TilePointer_Landscape, overwrite = TRUE
)

#' Get \code{\link{MosquitoPopFemale}} Pointer
#'
#' Return pointer to \code{MosquitoPopFemale$self}.
#'  * This method is bound to \code{Landscape$get_FemalePopPointer}
#'
get_FemalePopPointer_Landscape <- function(){
  return(private$FemalePopPointer)
}

Landscape$set(which = "public",name = "get_FemalePopPointer",
  value = get_FemalePopPointer_Landscape, overwrite = TRUE
)

#' Set \code{\link{MosquitoPopFemale}} Pointer
#'
#' Set pointer to \code{MosquitoPopFemale$self}.
#'  * This method is bound to \code{Landscape$set_FemalePopPointer}
#'
#' @param FemalePopPointer an environment
#'
set_FemalePopPointer_Landscape <- function(FemalePopPointer){
  private$FemalePopPointer = FemalePopPointer
}

Landscape$set(which = "public",name = "set_FemalePopPointer",
  value = set_FemalePopPointer_Landscape, overwrite = TRUE
)

#' Get \code{\link{MosquitoPopMale}} Pointer
#'
#' Return pointer to \code{MosquitoPopMale$self}.
#'  * This method is bound to \code{Landscape$get_MalePopPointer}
#'
get_MalePopPointer_Landscape <- function(){
  return(private$MalePopPointer)
}

Landscape$set(which = "public",name = "get_MalePopPointer",
  value = get_MalePopPointer_Landscape, overwrite = TRUE
)

#' Set \code{\link{MosquitoPopMale}} Pointer
#'
#' Set pointer to \code{MosquitoPopFemale$self}.
#'  * This method is bound to \code{Landscape$set_MalePopPointer}
#'
#' @param MalePopPointer an environment
#'
set_MalePopPointer_Landscape <- function(MalePopPointer){
  private$MalePopPointer = MalePopPointer
}

Landscape$set(which = "public",name = "set_MalePopPointer",
  value = set_MalePopPointer_Landscape, overwrite = TRUE
)

#' Get \code{\link[MASHmacro]{HumanPop}} Pointer
#'
#' Return pointer to \code{HumanPop$self}.
#'  * This method is bound to \code{Landscape$get_HumansPointer}
#'
get_HumansPointer_Landscape <- function(){
  return(private$HumansPointer)
}

Landscape$set(which = "public",name = "get_HumansPointer",
  value = get_HumansPointer_Landscape, overwrite = TRUE
)

#' Set \code{\link[MASHmacro]{HumanPop}} Pointer
#'
#' Set pointer to \code{HumanPop$self}.
#'  * This method is bound to \code{Landscape$set_HumansPointer}
#'
#' @param HumansPointer an environment
#'
set_HumansPointer_Landscape <- function(HumansPointer){
  private$HumansPointer = HumansPointer
}

Landscape$set(which = "public",name = "set_HumansPointer",
  value = set_HumansPointer_Landscape, overwrite = TRUE
)

###############################################################################
# Site-specific Functions
###############################################################################

#' Clear Host Risk Queue for Landscape
#'
#' Clear the \code{\link[MASHcpp]{RiskQ}} for all blood feeding sites by calling \code{\link{clear_RiskQ_FeedingSite}}
#'  * This method is bound to \code{Landscape$clear_RiskQ}
#'
clear_RiskQ_Landscape <- function(){
  for(ixF in 1:private$FeedingSitesN){
    private$FeedingSites[[ixF]]$clear_RiskQ()
  }
}

Landscape$set(which = "public",name = "clear_RiskQ",
  value = clear_RiskQ_Landscape, overwrite = TRUE
)

#' Clear Mating Queue for Landscape
#'
#' Clear the \code{\link[MASHcpp]{MatingQ}} for all mating sites by calling \code{\link{clear_MatingQ_MatingSite}}
#' This function is called in \code{\link{mbitesMale_Pop_MBITES}}.
#'  * This method is bound to \code{Landscape$clear_MatingQ}
#'
clear_MatingQ_Landscape <- function(){
  for(ix in 1:private$MatingSitesN){
    private$MatingSites[[ix]]$clear_MatingQ()
  }
}

Landscape$set(which = "public",name = "clear_MatingQ",
  value = clear_MatingQ_Landscape, overwrite = TRUE
)
