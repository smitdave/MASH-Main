###############################################################################
#
#       __  ____  ____  ______    _   __
#      / / / / / / /  |/  /   |  / | / /
#     / /_/ / / / / /|_/ / /| | /  |/ /
#    / __  / /_/ / /  / / ___ |/ /|  /
#   /_/ /_/\____/_/  /_/_/  |_/_/ |_/
#
#   MASH-MICRO
#   HUMAN: HumanPop Additional Methods for MICRO
#   MASH-MICRO Team
#   September 10, 2017
#
###############################################################################



#' Initialize Human Methods for MICRO
#'
#' Initialize additional methods in \code{\link[MASHmacro]{Human}} and \code{\link[MASHmacro]{HumanPop}}
#'
#' @export
Humans.MICRO.Setup <- function(overwrite = TRUE){

  cat("initializing Humans MICRO methods\n",sep="")

  # Activity Space

  MASHmacro::Human$set(which = "public",name = "get_ActivitySpace",
    value = get_ActivitySpace_Human, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "set_ActivitySpace",
    value = set_ActivitySpace_Human, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "init_ActivitySpace",
    value = init_ActivitySpace_HumanPop, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "init_ActivitySpace",
    value = init_ActivitySpace_Human, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "sim_ActivitySpace",
    value = sim_ActivitySpace_HumanPop, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "sim_ActivitySpace",
    value = sim_ActivitySpace_Human, overwrite = overwrite
  )

  MASHmicro::Tile$set(which = "public",name = "get_ActivitySpace",
    value = get_ActivitySpace_Tile, overwrite = overwrite
  )

  MASHmicro::Tile$set(which = "public",name = "write_ActivitySpace",
    value = write_ActivitySpace_Tile, overwrite = overwrite
  )

  # HumanPop

  MASHmacro::HumanPop$set(which = "private",name = "FemalePopPointer",
    value = NULL, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "private",name = "MalePopPointer",
    value = NULL, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "private",name = "LandscapePointer",
    value = NULL, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "get_LandscapePointer",
    value = get_LandscapePointer_HumanPop, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "set_LandscapePointer",
    value = set_LandscapePointer_HumanPop, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "get_FemalePopPointer",
    value = get_FemalePopPointer_HumanPop, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "set_FemalePopPointer",
    value = set_FemalePopPointer_HumanPop, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "get_MalePopPointer",
    value = get_MalePopPointer_HumanPop, overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "set_MalePopPointer",
    value = set_MalePopPointer_HumanPop, overwrite = overwrite
  )

  # Human

  MASHmacro::Human$set(which = "private",name = "FemalePopPointer",
    value = NULL, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "private",name = "MalePopPointer",
    value = NULL, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "private",name = "LandscapePointer",
    value = NULL, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "get_LandscapePointer",
    value = get_LandscapePointer_Human, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "set_LandscapePointer",
    value = set_LandscapePointer_Human, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "get_FemalePopPointer",
    value = get_FemalePopPointer_Human, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "set_FemalePopPointer",
    value = set_FemalePopPointer_Human, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "get_MalePopPointer",
    value = get_MalePopPointer_Human, overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "set_MalePopPointer",
    value = set_MalePopPointer_Human, overwrite = overwrite
  )

}

# HumanPop

#' Get \code{\link{Landscape}} Pointer
#'
#' Return pointer to \code{Landscape$self} this population lives in.
#'  * This method is bound to \code{HumanPop$get_LandscapePointer}
#'
get_LandscapePointer_HumanPop <- function(){return(private$LandscapePointer)}

#' Set \code{\link{Landscape}} Pointer
#'
#' Set pointer to \code{Landscape$self} this population lives in.
#'  * This method is bound to \code{HumanPop$set_LandscapePointer}
#'
#' @param LandscapePointer an environment
#'
set_LandscapePointer_HumanPop <- function(LandscapePointer){
  private$LandscapePointer = LandscapePointer
}

#' Get \code{\link{MosquitoPopFemale}} Pointer
#'
#' Return pointer to the female mosquito population on this landscape.
#'  * This method is bound to \code{HumanPop$get_FemalePopPointer}
#'
get_FemalePopPointer_HumanPop <- function(){return(private$FemalePopPointer)}

#' Set \code{\link{MosquitoPopFemale}} Pointer
#'
#' Set pointer to the female mosquito population on this landscape.
#'  * This method is bound to \code{HumanPop$set_FemalePopPointer}
#'
#' @param FemalePopPointer an environment
#'
set_FemalePopPointer_HumanPop <- function(FemalePopPointer){
  private$FemalePopPointer = FemalePopPointer
}

#' Get \code{\link{MosquitoPopMale}} Pointer
#'
#' Return pointer to the female mosquito population on this landscape.
#'  * This method is bound to \code{HumanPop$get_MalePopPointer}
#'
get_MalePopPointer_HumanPop <- function(){return(private$MalePopPointer)}

#' Set \code{\link{MosquitoPopMale}} Pointer
#'
#' Set pointer to the female mosquito population on this landscape.
#'  * This method is bound to \code{HumanPop$set_MalePopPointer}
#'
#' @param MosquitoPopMale an environment
#'
set_MalePopPointer_HumanPop <- function(MalePopPointer){
  private$MalePopPointer = MalePopPointer
}

# Human

#' Get \code{\link{Landscape}} Pointer
#'
#' Return pointer to \code{Landscape$self} this population lives in.
#'  * This method is bound to \code{Human$get_LandscapePointer}
#'
get_LandscapePointer_Human <- function(){return(private$LandscapePointer)}

#' Set \code{\link{Landscape}} Pointer
#'
#' Set pointer to \code{Landscape$self} this population lives in.
#'  * This method is bound to \code{Human$set_LandscapePointer}
#'
#' @param LandscapePointer an environment
#'
set_LandscapePointer_Human <- function(LandscapePointer){
  private$LandscapePointer = LandscapePointer
}

#' Get \code{\link{MosquitoPopFemale}} Pointer
#'
#' Return pointer to the female mosquito population on this landscape.
#'  * This method is bound to \code{Human$get_FemalePopPointer}
#'
get_FemalePopPointer_Human <- function(){return(private$FemalePopPointer)}

#' Set \code{\link{MosquitoPopFemale}} Pointer
#'
#' Set pointer to the female mosquito population on this landscape.
#'  * This method is bound to \code{Human$set_FemalePopPointer}
#'
#' @param FemalePopPointer an environment
#'
set_FemalePopPointer_Human <- function(FemalePopPointer){
  private$FemalePopPointer = FemalePopPointer
}

#' Get \code{\link{MosquitoPopMale}} Pointer
#'
#' Return pointer to the female mosquito population on this landscape.
#'  * This method is bound to \code{Human$get_MalePopPointer}
#'
get_MalePopPointer_Human <- function(){return(private$MalePopPointer)}

#' Set \code{\link{MosquitoPopMale}} Pointer
#'
#' Set pointer to the female mosquito population on this landscape.
#'  * This method is bound to \code{Human$set_MalePopPointer}
#'
#' @param MosquitoPopMale an environment
#'
set_MalePopPointer_Human <- function(MalePopPointer){
  private$MalePopPointer = MalePopPointer
}
