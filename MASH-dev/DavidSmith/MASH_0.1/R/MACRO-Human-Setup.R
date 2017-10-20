#################################################################
#
#   MASH
#   R6-ified
#   Additional Human Methods & Fields for MACRO
#   David Smith, Hector Sanchez, Sean Wu
#   May 21, 2017
#
#################################################################


#################################################################
# Initalize Methods & Fields in 'Human'
#################################################################

#' MACRO: Initialize Additional Methods & Fields in \code{\link{Human}} and \code{\link{HumanPop}}
#'
#' Initialize methods and fields for biting and inter-patch travel for a macrosimulation tile \code{\link{MacroTile}}.
#'
#' @param pathogenModule what PATHOGEN module to use (must be character in "PfSI","PfMOI")
#' @param overwrite overwrite existing fields
#' @examples
#' MACRO.Humans.Setup()
#' @export
MACRO.Humans.Setup <- function(pathogenModule = "PfSI" ,overwrite = TRUE){

  message("initializing MACRO component methods & fields for Human & HumanPop Class")

  #############################################
  # HumanPop methods
  #############################################

  #############################################
  # patchID: vector of patchID for all humans
  #############################################

  HumanPop$set(which = "private",name = "patchID",
            value = NULL,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "get_patchID",
            value = MacroHumanPop_get_patchID,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "set_patchID",
            value = MacroHumanPop_set_patchID,
            overwrite = overwrite
  )

  #############################################
  # TilePointer: point to the Patches (a network of patches) in this metapopulation TILE (MACRO)
  #############################################

  HumanPop$set(which = "private",name = "TilePointer",
            value = NULL,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "get_TilePointer",
            value = MacroHuman_get_TilePointer,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "set_TilePointer",
            value = MacroHuman_set_TilePointer,
            overwrite = overwrite
  )

  #############################################
  # PatchesPointer: point to the Patches (a network of patches) in this metapopulation TILE (MACRO)
  #############################################

  HumanPop$set(which = "private",name = "PatchesPointer",
            value = NULL,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "get_PatchesPointer",
            value = MacroHuman_get_PatchesPointer,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "set_PatchesPointer",
            value = MacroHuman_set_PatchesPointer,
            overwrite = overwrite
  )

  #############################################
  # MosquitoPointer: point to the Mosquito population in this metapopulation TILE (MACRO)
  #############################################

  HumanPop$set(which = "private",name = "MosquitoPointer",
            value = NULL,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "get_MosquitoPointer",
            value = MacroHuman_get_MosquitoPointer,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "set_MosquitoPointer",
            value = MacroHuman_set_MosquitoPointer,
            overwrite = overwrite
  )


  #############################################
  # Human methods
  #############################################

  #############################################
  # location: where the human is now
  #############################################

  Human$set(which = "private",name = "location",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_location",
            value = MacroHuman_get_location,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_location",
            value = MacroHuman_set_location,
            overwrite = overwrite
  )

  #############################################
  # patchID: the human's permanant home
  #############################################

  Human$set(which = "private",name = "patchID",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_patchID",
            value = MacroHuman_get_patchID,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_patchID",
            value = MacroHuman_set_patchID,
            overwrite = overwrite
  )

  #############################################
  # TilePointer: point to the Patches (a network of patches) in this metapopulation TILE (MACRO)
  #############################################

  Human$set(which = "private",name = "TilePointer",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_TilePointer",
            value = MacroHuman_get_TilePointer,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_TilePointer",
            value = MacroHuman_set_TilePointer,
            overwrite = overwrite
  )

  #############################################
  # PatchesPointer: point to the Patches (a network of patches) in this metapopulation TILE (MACRO)
  #############################################

  Human$set(which = "private",name = "PatchesPointer",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_PatchesPointer",
            value = MacroHuman_get_PatchesPointer,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_PatchesPointer",
            value = MacroHuman_set_PatchesPointer,
            overwrite = overwrite
  )

  #############################################
  # MosquitoPointer: point to the Mosquito population in this metapopulation TILE (MACRO)
  #############################################

  Human$set(which = "private",name = "MosquitoPointer",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_MosquitoPointer",
            value = MacroHuman_get_MosquitoPointer,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_MosquitoPointer",
            value = MacroHuman_set_MosquitoPointer,
            overwrite = overwrite
  )

  #############################################
  # MACRO-Human-Travel.R
  #############################################

  # interface with MacroPatch
  Human$set(which = "public",name = "go_Patch",
            value = MacroHuman_go_Patch,
            overwrite = overwrite
  )

  # travel history:

  Human$set(which = "private",name = "travelHistory",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_travelHistory",
            value = MacroHuman_get_travelHistoryHuman,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "get_travelHistory",
            value = MacroHuman_get_travelHistoryHumanPop,
            overwrite = overwrite
  )

  # travel: the human's travel habits

  Human$set(which = "private",name = "travel",
            value = NULL,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_travel",
            value = MacroHuman_get_travel,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_travel",
            value = MacroHuman_set_travel,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "init_travel",
            value = MacroHuman_init_travel,
            overwrite = overwrite
  )

  # Events

  Human$set(which = "public",name = "add2Q_takeTrip",
            value = MacroHuman_add2Q_takeTrip,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_takeTrip",
            value = MacroHuman_event_takeTrip,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "takeTrip",
            value = MacroHuman_takeTrip,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "add2Q_returnHome",
            value = MacroHuman_add2Q_returnHome,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_returnHome",
            value = MacroHuman_event_returnHome,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "returnHome",
            value = MacroHuman_returnHome,
            overwrite = overwrite
  )

  # movement related utilities

  HumanPop$set(which = "public",name = "json_travelHistory",
            value = MacroHuman_json_travelHistory,
            overwrite = overwrite
  )


  #############################################
  # MACRO-Human-Biting.R
  #############################################

  # expectedBites

  Human$set(which = "private",name = "myEIR",
            value = 0,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "get_myEIR",
            value = MacroHuman_get_myEIR,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "set_myEIR",
            value = MacroHuman_set_myEIR,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "expectedBites",
            value = MacroHuman_expectedBites,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "add2Q_Bites",
            value = MacroHuman_add2Q_Bites,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "queueInfectiousBites",
            value = MacroHuman_queueInfectiousBites,
            overwrite = overwrite
  )

  switch(pathogenModule,
          PfSI = {
            # set sumKappa
            Human$set(which = "public",name = "sumKappa",
                      value = MacroHuman_sumKappa_PfSI,
                      overwrite = overwrite
            )
          },
          PfMOI = {
            # set sumKappa
            Human$set(which = "public",name = "sumKappa",
                      value = MacroHuman_sumKappa_PfMOI,
                      overwrite = overwrite
            )
          },
          {stop("other PATHOGEN modules not yet implemented for MACRO")}
  )

  # set sumKappa
  HumanPop$set(which = "public",name = "updateKappa",
            value = MacroHuman_updateKappa,
            overwrite = overwrite
  )

}


#################################################################
# Define Methods in 'Human'
#################################################################

# location

#' MACRO: Get \code{Human} location
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_get_location <- function(){
  return(private$location)
}

#' MACRO: Set \code{Human} location
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_set_location <- function(location){
  private$location = location
}

# patchID

#' Get \code{Human} patchID
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_get_patchID <- function(){
  return(private$patchID)
}

#' Set \code{Human} patchID
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_set_patchID <- function(patchID){
  private$patchID = patchID
}

# TilePointer

#' Get \code{Human} \code{MacroTile} Pointer
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_get_TilePointer <- function(){
  return(private$TilePointer)
}

#' Set \code{Human} \code{MacroTile} Pointer
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_set_TilePointer <- function(TilePointer){
  private$TilePointer = TilePointer
}

# PatchesPointer

#' Get \code{Human} \code{MacroPatch} Pointer
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_get_PatchesPointer <- function(){
  return(private$PatchesPointer)
}

#' Set \code{Human} \code{MacroPatch} Pointer
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_set_PatchesPointer <- function(PatchesPointer){
  private$PatchesPointer = PatchesPointer
}

# MosquitoPointer

#' Get \code{Human} \code{MacroMosquitoPop} Pointer
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_get_MosquitoPointer <- function(){
  return(private$MosquitoPointer)
}

#' Set \code{Human} \code{MacroMosquitoPop} Pointer
#'
#' Write me!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_set_MosquitoPointer <- function(MosquitoPointer){
  private$MosquitoPointer = MosquitoPointer
}

#' MACRO \code{HumanPop} Method: Get all patchID
#'
#' Return all patchID
#'
MacroHumanPop_get_patchID <- function(){
  return(private$patchID)
}

#' MACRO \code{HumanPop} Method: Set all patchID
#'
#' Return all patchID
#'
MacroHumanPop_set_patchID <- function(patchID){
  private$patchID = patchID
}
