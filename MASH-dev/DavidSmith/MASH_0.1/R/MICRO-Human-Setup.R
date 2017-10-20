#################################################################
#
#   MASH
#   R6-ified
#   Additional Human Methods & Fields for MICRO
#   David Smith, Hector Sanchez, Sean Wu
#   June 1, 2017
#
#################################################################

#######################################################
# Initalize Methods & Fields in 'Human' & 'HumanPop'
#################################################################

#' MICRO: Initialize Additional Methods & Fields in \code{\link{Human}} and \code{\link{HumanPop}}
#'
#' Write me! \code{\link{Human}}
#'
#' @param overwrite overwrite existing methods
#' @examples
#' MACRO.Humans.Setup()
#' @export
MICRO.Humans.Setup <- function(overwrite = TRUE){

  message("initializing MICRO component methods & fields for Human & HumanPop Class")

  #############################################
  # HumanPop methods
  #############################################

  #############################################
  # TilePointer: point to the MicroTile in this microsimulation TILE (MICRO)
  #############################################

  HumanPop$set(which = "private",name = "TilePointer",
            value = NULL,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "get_TilePointer",
            value = get_MicroHumanPop_TilePointer,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "set_TilePointer",
            value = set_MicroHumanPop_TilePointer,
            overwrite = overwrite
  )

  #############################################
  # LandscapePointer: point to the Landscape (collection of sites) in this microsimulation TILE (MICRO)
  #############################################

  HumanPop$set(which = "private",name = "LandscapePointer",
            value = NULL,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "get_LandscapePointer",
            value = get_MicroHumanPop_LandscapePointer,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "set_LandscapePointer",
            value = set_MicroHumanPop_LandscapePointer,
            overwrite = overwrite
  )

  #############################################
  # FemalePopPointer: point to the Female Mosquito population in this microsimulation TILE (MICRO)
  #############################################

  HumanPop$set(which = "private",name = "FemalePopPointer",
            value = NULL,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "get_FemalePopPointer",
            value = get_MicroHumanPop_FemalePopPointer,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "set_FemalePopPointer",
            value = set_MicroHumanPop_FemalePopPointer,
            overwrite = overwrite
  )


  #############################################
  # Human methods
  #############################################

  #############################################
  # TilePointer: point to the MicroTile in this microsimulation TILE (MICRO)
  #############################################

  Human$set(which = "private",name = "TilePointer",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_TilePointer",
            value = get_MicroHuman_TilePointer,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_TilePointer",
            value = set_MicroHuman_TilePointer,
            overwrite = overwrite
  )

  #############################################
  # LandscapePointer: point to the Landscape (collection of sites) in this microsimulation TILE (MICRO)
  #############################################

  Human$set(which = "private",name = "LandscapePointer",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_LandscapePointer",
            value = get_MicroHuman_LandscapePointer,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_LandscapePointer",
            value = set_MicroHuman_LandscapePointer,
            overwrite = overwrite
  )

  #############################################
  # FemalePopPointer: point to the Female Mosquito population in this microsimulation TILE (MICRO)
  #############################################

  Human$set(which = "private",name = "FemalePopPointer",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_FemalePopPointer",
            value = get_MicroHuman_FemalePopPointer,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_FemalePopPointer",
            value = set_MicroHuman_FemalePopPointer,
            overwrite = overwrite
  )

  #############################################
  # MICRO-Human-ActivitySpace.R
  #############################################

  Human$set(which = "private",name = "ActivitySpace",
            value = list(
              nDaily = 0L,
              Nplaces = 0L,
              p = 1,
              loc = 0L
            ),
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_ActivitySpace",
            value = get_MicroHuman_ActivitySpace,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_ActivitySpace",
            value = set_MicroHuman_ActivitySpace,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "init_ActivitySpace",
            value = init_MicroHumanPop_ActivitySpace,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "init_ActivitySpace",
            value = init_MicroHuman_ActivitySpace,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "sim_ActivitySpace",
            value = sim_MicroHumanPop_ActivitySpace,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "sim_ActivitySpace",
            value = sim_MicroHuman_ActivitySpace,
            overwrite = overwrite
  )


}


#################################################################
# 'HumanPop' Method Definitions
#################################################################

# TilePointer

#' MICRO: Get \code{\link{HumanPop}} \code{\link{MicroTile}} Pointer
#'
#' This function is bound to \code{HumanPop$get_TilePointer()}
#'
get_MicroHumanPop_TilePointer <- function(){
  return(private$TilePointer)
}

#' MICRO: Set \code{\link{HumanPop}} \code{\link{MicroTile}} Pointer
#'
#' This function is bound to \code{HumanPop$set_TilePointer()}
#'
#' @param TilePointer the R6 \code{\link{MicroTile}} object to point to
#'
set_MicroHumanPop_TilePointer <- function(TilePointer){
  private$TilePointer = TilePointer
}

# LandscapePointer

#' MICRO: Get \code{\link{HumanPop}} \code{\link{Landscape}} Pointer
#'
#' This function is bound to \code{HumanPop$get_LandscapePointer()}
#'
get_MicroHumanPop_LandscapePointer <- function(){
  return(private$LandscapePointer)
}

#' MICRO: Set \code{\link{HumanPop}} \code{\link{Landscape}} Pointer
#'
#' This function is bound to \code{HumanPop$set_LandscapePointer()}
#'
#' @param LandscapePointer the R6 \code{\link{Landscape}} object to point to
#'
set_MicroHumanPop_LandscapePointer <- function(LandscapePointer){
  private$LandscapePointer = LandscapePointer
}

# FemalePopPointer

#' MICRO: Get \code{\link{HumanPop}} \code{\link{MicroMosquitoPopFemale}} Pointer
#'
#' This function is bound to \code{HumanPop$get_FemalePopPointer()}
#'
get_MicroHumanPop_FemalePopPointer <- function(){
  return(private$FemalePopPointer)
}

#' MICRO: Set \code{\link{HumanPop}} \code{\link{MicroMosquitoPopFemale}} Pointer
#'
#' This function is bound to \code{HumanPop$set_FemalePopPointer()}
#'
#' @param LandscapePointer the R6 \code{\link{MicroMosquitoPopFemale}} object to point to
#'
set_MicroHumanPop_FemalePopPointer <- function(FemalePopPointer){
  private$FemalePopPointer = FemalePopPointer
}


#################################################################
# 'Human' Method Definitions
#################################################################

# TilePointer

#' MICRO: Get \code{\link{Human}} \code{\link{MicroTile}} Pointer
#'
#' This function is bound to \code{Human$get_TilePointer()}
#'
get_MicroHuman_TilePointer <- function(){
  return(private$TilePointer)
}

#' MICRO: Set \code{\link{Human}} \code{\link{MicroTile}} Pointer
#'
#' This function is bound to \code{Human$set_TilePointer()}
#'
#' @param TilePointer the R6 \code{\link{MicroTile}} object to point to
#'
set_MicroHuman_TilePointer <- function(TilePointer){
  private$TilePointer = TilePointer
}

# LandscapePointer

#' MICRO: Get \code{\link{Human}} \code{\link{Landscape}} Pointer
#'
#' This function is bound to \code{Human$get_LandscapePointer()}
#'
get_MicroHuman_LandscapePointer <- function(){
  return(private$LandscapePointer)
}

#' MICRO: Set \code{\link{Human}} \code{\link{Landscape}} Pointer
#'
#' This function is bound to \code{Human$set_LandscapePointer()}
#'
#' @param LandscapePointer the R6 \code{\link{Landscape}} object to point to
#'
set_MicroHuman_LandscapePointer <- function(LandscapePointer){
  private$LandscapePointer = LandscapePointer
}

# FemalePopPointer

#' MICRO: Get \code{\link{Human}} \code{\link{MicroMosquitoPopFemale}} Pointer
#'
#' This function is bound to \code{Human$get_FemalePopPointer()}
#'
get_MicroHuman_FemalePopPointer <- function(){
  return(private$FemalePopPointer)
}

#' MICRO: Set \code{\link{Human}} \code{\link{MicroMosquitoPopFemale}} Pointer
#'
#' This function is bound to \code{Human$set_FemalePopPointer()}
#'
#' @param LandscapePointer the R6 \code{\link{MicroMosquitoPopFemale}} object to point to
#'
set_MicroHuman_FemalePopPointer <- function(FemalePopPointer){
  private$FemalePopPointer = FemalePopPointer
}
