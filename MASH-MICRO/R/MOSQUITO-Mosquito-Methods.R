###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MICRO
#   MICRO: Mosquito Class Methods
#   MASH-MICRO Team
#   September 6, 2017
#
###############################################################################


###############################################################################
# Generic Methods
###############################################################################

#' Remove Self
#'
#' Remove this mosquito from the population containing it.
#'  * This method is bound to \code{MosquitoFemale$rmSelf}
#'
rmSelf_MosquitoFemale <- function(){
  private$FemalePopPointer$get_pop()$rm(key = private$id)
}

MosquitoFemale$set(which = "public",name = "rmSelf",
  value = rmSelf_MosquitoFemale, overwrite = TRUE
)


#' Remove Self
#'
#' Remove this mosquito from the population containing it.
#'  * This method is bound to \code{MosquitoMale$rmSelf}
#'
rmSelf_MosquitoMale <- function(){
  private$FemalePopPointer$get_pop()$rm(key = private$id)
}

MosquitoMale$set(which = "public",name = "rmSelf",
  value = rmSelf_MosquitoMale, overwrite = TRUE
)


###############################################################################
# Female-specific Methods
###############################################################################

#' Get bmSize
#'
#' Return mosquito blood meal size (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_bmSize}
#'
get_bmSize_Mosquito <- function(){
  return(private$bmSize)
}

MosquitoFemale$set(which = "public",name = "get_bmSize",
  value = get_bmSize_Mosquito, overwrite = TRUE
)

#' Get batch
#'
#' Return mosquito egg batch size (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_bmSize}
#'
get_batch_Mosquito <- function(){
  return(private$batch)
}

MosquitoFemale$set(which = "public",name = "get_batch",
  value = get_batch_Mosquito, overwrite = TRUE
)

#' Get eggT
#'
#' Return mosquito minimum time for eggs to mature (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_eggT}
#'
get_eggT_Mosquito <- function(){
  return(private$eggT)
}

MosquitoFemale$set(which = "public",name = "get_eggT",
  value = get_eggT_Mosquito, overwrite = TRUE
)

#' Get eggP
#'
#' Return mosquito minimum blood provision for eggs to mature (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_eggP}
#'
get_eggP_Mosquito <- function(){
  return(private$eggP)
}

MosquitoFemale$set(which = "public",name = "get_eggP",
  value = get_eggP_Mosquito, overwrite = TRUE
)

#' Get sire
#'
#' Return ID of the \code{\link{MosquitoMale}} this mosquito mated with (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_sire}
#'
get_sire_Mosquito <- function(){
  return(private$sire)
}

MosquitoFemale$set(which = "public",name = "get_sire",
  value = get_sire_Mosquito, overwrite = TRUE
)

#' Get energyPreG
#'
#' Return mosquito minimum blood provision for maturation (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_energyPreG}
#'
get_energyPreG_Mosquito <- function(){
  return(private$energyPreG)
}

MosquitoFemale$set(which = "public",name = "get_energyPreG",
  value = get_energyPreG_Mosquito, overwrite = TRUE
)

#' Get hostID
#'
#' Return ID of the host this mosquito last fed upon (\code{integer}).
#'  * This method is bound to \code{MosquitoFemale$get_hostID}
#'
get_hostID_Mosquito <- function(){
  return(private$hostID)
}

MosquitoFemale$set(which = "public",name = "get_hostID",
  value = get_hostID_Mosquito, overwrite = TRUE
)

#' Get Pathogens
#'
#' Return \code{Pathogens} object in this mosquito.
#'  * This method is bound to \code{MosquitoFemale$get_Pathogens}
#
get_Pathogens_Mosquito <- function(){
  return(private$Pathogens)
}

MosquitoFemale$set(which = "public",name = "get_Pathogens",
  value = get_Pathogens_Mosquito, overwrite = TRUE
)

#' Initialize Pathogens
#'
#' Initialize \code{Pathogens} object in this mosquito. This function is a placeholder that should be overwritten by an appropriate PATHOGEN module setup function.
#'  * This method is bound to \code{MosquitoFemale$init_Pathogens}
#
init_Pathogens_Mosquito <- function(){
  cat("if you are seeing this error the correct PATHOGEN module setup has not been run!\n",sep="")
}

MosquitoFemale$set(which = "public",name = "init_Pathogens",
  value = init_Pathogens_Mosquito, overwrite = TRUE
)

#' Get History
#'
#' Return history from \code{\link[MASHcpp]{MosquitoFemaleHistory}}
#'  * This method is bound to \code{MosquitoFemale$get_history}
#
get_history_Mosquito <- function(){
  return(
    private$history$exportHistory()
  )
}

MosquitoFemale$set(which = "public",name = "get_history",
  value = get_history_Mosquito, overwrite = TRUE
)

#' Get Bionomics
#'
#' Return bionomics calculated from \code{\link[MASHcpp]{MosquitoFemaleHistory}}
#'  * This method is bound to \code{MosquitoFemale$get_bionomics}
#
get_bionomics_Mosquito <- function(){
  return(
    private$history$exportBionomics()
  )
}

MosquitoFemale$set(which = "public",name = "get_bionomics",
  value = get_bionomics_Mosquito, overwrite = TRUE
)


###############################################################################
# Getters & Setters
###############################################################################

#' Get ID
#'
#' Return mosquito ID (\code{character}).
#'  * This method is bound to \code{MosquitoFemale$get_id} and \code{MosquitoMale$get_id}
#'
get_id_Mosquito <- function(){
  return(private$id)
}

MosquitoFemale$set(which = "public",name = "get_id",
  value = get_id_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_id",
  value = get_id_Mosquito, overwrite = TRUE
)

#' Get bDay
#'
#' Return mosquito birthday (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_bDay} and \code{MosquitoMale$get_bDay}
#'
get_bDay_Mosquito <- function(){
  return(private$bDay)
}

MosquitoFemale$set(which = "public",name = "get_bDay",
  value = get_bDay_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_bDay",
  value = get_bDay_Mosquito, overwrite = TRUE
)

#' Get tNow
#'
#' Return mosquito current time (time at which mosquito entered current state) (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_tNow} and \code{MosquitoMale$get_tNow}
#'
get_tNow_Mosquito <- function(){
  return(private$tNow)
}

MosquitoFemale$set(which = "public",name = "get_tNow",
  value = get_tNow_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_tNow",
  value = get_tNow_Mosquito, overwrite = TRUE
)

#' Get tNext
#'
#' Return mosquito next jump time (time at which mosquito will leave current state) (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_tNext} and \code{MosquitoMale$get_tNext}
#'
get_tNext_Mosquito <- function(){
  return(private$tNext)
}

MosquitoFemale$set(which = "public",name = "get_tNext",
  value = get_tNext_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_tNext",
  value = get_tNext_Mosquito, overwrite = TRUE
)

#' Get genotype
#'
#' Return mosquito genotype (\code{integer}).
#'  * This method is bound to \code{MosquitoFemale$get_genotype} and \code{MosquitoMale$get_genotype}
#'
get_genotype_Mosquito <- function(){
  return(private$genotype)
}

MosquitoFemale$set(which = "public",name = "get_genotype",
  value = get_genotype_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_genotype",
  value = get_genotype_Mosquito, overwrite = TRUE
)

#' Get state
#'
#' Return mosquito current behavioral state (\code{character}).
#'  * This method is bound to \code{MosquitoFemale$get_state} and \code{MosquitoMale$get_state}
#'
get_state_Mosquito <- function(){
  return(private$state)
}

MosquitoFemale$set(which = "public",name = "get_state",
  value = get_state_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_state",
  value = get_state_Mosquito, overwrite = TRUE
)

#' Get stateNew
#'
#' Return mosquito next behavioral state (\code{character}).
#'  * This method is bound to \code{MosquitoFemale$get_stateNew} and \code{MosquitoMale$get_stateNew}
#'
get_stateNew_Mosquito <- function(){
  return(private$stateNew)
}

MosquitoFemale$set(which = "public",name = "get_stateNew",
  value = get_stateNew_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_stateNew",
  value = get_stateNew_Mosquito, overwrite = TRUE
)

#' Get inPointSet
#'
#' Return mosquito point set (\code{character}). With \code{ix}, the mosquito spatial location is resolved.
#'  * This method is bound to \code{MosquitoFemale$get_inPointSet} and \code{MosquitoMale$get_inPointSet}
#'
get_inPointSet_Mosquito <- function(){
  return(private$inPointSet)
}

MosquitoFemale$set(which = "public",name = "get_inPointSet",
  value = get_inPointSet_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_inPointSet",
  value = get_inPointSet_Mosquito, overwrite = TRUE
)

#' Get ix
#'
#' Return mosquito point index (\code{integer}). With \code{inPointSet}, the mosquito spatial location is resolved.
#'  * This method is bound to \code{MosquitoFemale$get_ix} and \code{MosquitoMale$get_ix}
#'
get_ix_Mosquito <- function(){
  return(private$ix)
}

MosquitoFemale$set(which = "public",name = "get_ix",
  value = get_ix_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_ix",
  value = get_ix_Mosquito, overwrite = TRUE
)

#' Get mature
#'
#' Return mosquito maturity (\code{logical}).
#'  * This method is bound to \code{MosquitoFemale$get_mature} and \code{MosquitoMale$get_mature}
#'
get_mature_Mosquito <- function(){
  return(private$mature)
}

MosquitoFemale$set(which = "public",name = "get_mature",
  value = get_mature_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_mature",
  value = get_mature_Mosquito, overwrite = TRUE
)


#' Get lspot
#'
#' Return mosquito landing spot (\code{character}).
#' Codes corresponding to the following spatial relation to the site this mosquito is at.
#'  * i: rest on the inside wall of a structure
#'  * w: rest on the outside wall of a structure
#'  * v: rest on vegetation
#'  * r: reattempt without resting
#'  * l: leave the area
#'
#'  * This method is bound to \code{MosquitoFemale$get_lspot} and \code{MosquitoMale$get_lspot}
#'
get_lspot_Mosquito <- function(){
  return(private$lspot)
}

MosquitoFemale$set(which = "public",name = "get_lspot",
  value = get_lspot_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_lspot",
  value = get_lspot_Mosquito, overwrite = TRUE
)

#' Get damage
#'
#' Return mosquito wing damage (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_damage} and \code{MosquitoMale$get_damage}
#'
get_damage_Mosquito <- function(){
  return(private$damage)
}

MosquitoFemale$set(which = "public",name = "get_lspot",
  value = get_damage_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_lspot",
  value = get_damage_Mosquito, overwrite = TRUE
)

#' Get energy
#'
#' Return mosquito energy reserves (\code{numeric}).
#'  * This method is bound to \code{MosquitoFemale$get_energy} and \code{MosquitoMale$get_energy}
#'
get_energy_Mosquito <- function(){
  return(private$energy)
}

MosquitoFemale$set(which = "public",name = "get_energy",
  value = get_energy_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_energy",
  value = get_energy_Mosquito, overwrite = TRUE
)


###############################################################################
# Pointers
###############################################################################

#' Get \code{\link{MosquitoPopFemale}} Pointer
#'
#' Return \code{MosquitoPopFemale$self} enclosing this mosquito (if method for \code{\link{MosquitoFemale}}), or in the same \code{\link{Tile}} (if method for \code{\link{MosquitoMale}}) by reference.
#'  * This method is bound to \code{MosquitoFemale$get_FemalePopPointer} and \code{MosquitoMale$get_FemalePopPointer}
#'
get_FemalePopPointer_Mosquito <- function(){
  return(private$FemalePopPointer)
}

MosquitoFemale$set(which = "public",name = "get_FemalePopPointer",
  value = get_FemalePopPointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_FemalePopPointer",
  value = get_FemalePopPointer_Mosquito, overwrite = TRUE
)

#' Set \code{\link{MosquitoPopFemale}} Pointer
#'
#' Set the pointer to \code{MosquitoPopFemale$self} enclosing this mosquito (if method for \code{\link{MosquitoFemale}}), or in the same \code{\link{Tile}} (if method for \code{\link{MosquitoMale}}).
#'  * This method is bound to \code{MosquitoFemale$set_FemalePopPointer} and \code{MosquitoMale$set_FemalePopPointer}
#'
#' @param FemalePopPointer an environment
#'
set_FemalePopPointer_Mosquito <- function(FemalePopPointer){
  private$FemalePopPointer = FemalePopPointer
}

MosquitoFemale$set(which = "public",name = "set_FemalePopPointer",
  value = set_FemalePopPointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "set_FemalePopPointer",
  value = set_FemalePopPointer_Mosquito, overwrite = TRUE
)


#' Get \code{\link{MosquitoPopMale}} Pointer
#'
#' Return \code{MosquitoPopMale$self} enclosing this mosquito (if method for \code{\link{MosquitoMale}}), or in the same \code{\link{Tile}} (if method for \code{\link{MosquitoFemale}}) by reference.
#'  * This method is bound to \code{MosquitoFemale$get_MalePopPointer} and \code{MosquitoMale$get_MalePopPointer}
#'
get_MalePopPointer_Mosquito <- function(){
  return(private$MalePopPointer)
}

MosquitoFemale$set(which = "public",name = "get_MalePopPointer",
  value = get_MalePopPointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_MalePopPointer",
  value = get_MalePopPointer_Mosquito, overwrite = TRUE
)

#' Set \code{\link{MosquitoPopMale}} Pointer
#'
#' Set the pointer to \code{MosquitoPopMale$self} enclosing this mosquito (if method for \code{\link{MosquitoMale}}), or in the same \code{\link{Tile}} (if method for \code{\link{MosquitoFemale}}).
#'  * This method is bound to \code{MosquitoFemale$set_MalePopPointer} and \code{MosquitoMale$set_MalePopPointer}
#'
#' @param MalePopPointer an environment
#'
set_MalePopPointer_Mosquito <- function(MalePopPointer){
  private$MalePopPointer = MalePopPointer
}

MosquitoFemale$set(which = "public",name = "set_MalePopPointer",
  value = set_MalePopPointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "set_MalePopPointer",
  value = set_MalePopPointer_Mosquito, overwrite = TRUE
)


#' Get \code{\link{Landscape}} Pointer
#'
#' Return enclosing \code{Landscape$self} by refernce.
#'  * This method is bound to \code{MosquitoFemale$get_LandscapePointer} and \code{MosquitoMale$get_LandscapePointer}
#'
get_LandscapePointer_Mosquito <- function(){
  return(private$LandscapePointer)
}

MosquitoFemale$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_Mosquito, overwrite = TRUE
)

#' Set \code{\link{Landscape}} Pointer
#'
#' Set pointer to the enclosing \code{Landscape$self}.
#'  * This method is bound to \code{MosquitoFemale$set_LandscapePointer} and \code{MosquitoMale$set_LandscapePointer}
#'
#' @param LandscapePointer an environment
#'
set_LandscapePointer_Mosquito <- function(LandscapePointer){
  private$LandscapePointer = LandscapePointer
}

MosquitoFemale$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_Mosquito, overwrite = TRUE
)


#' Get \code{\link[MASHmacro]{HumanPop}} Pointer
#'
#' Return pointer to \code{HumanPop$self} within the same microsimulation tile by reference.
#'  * This method is bound to \code{MosquitoFemale$get_HumansPointer} and \code{MosquitoMale$get_HumansPointer}
#'
get_HumansPointer_Mosquito <- function(){
  return(private$HumansPointer)
}

MosquitoFemale$set(which = "public",name = "get_HumansPointer",
  value = get_HumansPointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_HumansPointer",
  value = get_HumansPointer_Mosquito, overwrite = TRUE
)

#' Set \code{\link[MASHmacro]{HumanPop}} Pointer
#'
#' Set pointer to \code{HumanPop$self} within the same microsimulation tile.
#'  * This method is bound to \code{MosquitoFemale$set_HumansPointer} and \code{MosquitoMale$set_HumansPointer}
#'
#' @param HumansPointer an environment
#'
set_HumansPointer_Mosquito <- function(HumansPointer){
  private$HumansPointer = HumansPointer
}

MosquitoFemale$set(which = "public",name = "set_HumansPointer",
  value = set_HumansPointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "set_HumansPointer",
  value = set_HumansPointer_Mosquito, overwrite = TRUE
)


#' Get \code{\link{Tile}} Pointer
#'
#' Return pointer to enclosing \code{Tile$self}.
#'  * This method is bound to \code{MosquitoFemale$get_TilePointer} and \code{MosquitoMale$get_TilePointer}
#'
get_TilePointer_Mosquito <- function(){
  return(private$TilePointer)
}

MosquitoFemale$set(which = "public",name = "get_TilePointer",
  value = get_TilePointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "get_TilePointer",
  value = get_TilePointer_Mosquito, overwrite = TRUE
)

#' Set \code{\link{Tile}} Pointer
#'
#' Set pointer to enclosing \code{Tile$self}.
#'  * This method is bound to \code{MosquitoFemale$set_TilePointer} and \code{MosquitoMale$set_TilePointer}
#'
#' @param TilePointer an environment
#'
set_TilePointer_Mosquito <- function(TilePointer){
  private$TilePointer = TilePointer
}

MosquitoFemale$set(which = "public",name = "set_TilePointer",
  value = set_TilePointer_Mosquito, overwrite = TRUE
)

MosquitoMale$set(which = "public",name = "set_TilePointer",
  value = set_TilePointer_Mosquito, overwrite = TRUE
)
