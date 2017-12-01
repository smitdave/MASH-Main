###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MICRO
#   MICRO: MosquitoPop Class Methods
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


###############################################################################
# Male-specific Methods
###############################################################################

#' Get mateFitness
#'
#' Get the male mating fitness associated with a given genotype.
#'  * This method is bound to \code{MosquitoPopMale$get_mateFitness}
#'
#' @param genotype integer; genotype of male mosquito
#'
get_mateFitness_MosquitoPopMale <- function(genotype){
  return(private$MBITES_PAR$mateFitness[genotype])
}

MosquitoPopMale$set(which = "public",name = "get_mateFitness",
  value = get_mateFitness_MosquitoPopMale, overwrite = TRUE
)


###############################################################################
# Push Pop
###############################################################################

#' Push pop
#'
#' Push new female mosquitoes into \code{\link{MosquitoPopFemale}}.
#'  * This method is bound to \code{MosquitoPopFemale$push_pop}
#'
#' @param N integer; number of emerging mosquitoes
#' @param tEmerge integer; day of emergence
#' @param genotype integer; genotype of emerging mosquitoes
#' @param locNow integer; site of emergence
#'
push_pop_MosquitoPopFemale <- function(N, tEmerge, genotype, locNow){
  for(i in 1:N){

    # assign the mosquitoes
    myID = paste0(tEmerge,"_",i,"_",genotype)
    private$pop$assign(key = myID, value = MosquitoFemale$new(id=myID,time=tEmerge,locNow=locNow,genotype=genotype,state=self$get_MBITES_PAR("initState"),eggT=self$get_MBITES_PAR("eggT"),eggP=self$get_MBITES_PAR("eggP"),energyPreG=self$get_MBITES_PAR("energyPreG")))

    private$pop$get(myID)$set_FemalePopPointer(self)
    private$pop$get(myID)$set_MalePopPointer(private$MalePopPointer)

    private$pop$get(myID)$set_TilePointer(private$TilePointer)
    private$pop$get(myID)$set_LandscapePointer(private$LandscapePointer)
    private$pop$get(myID)$set_HumansPointer(private$HumansPointer)

  }
}

MosquitoPopFemale$set(which = "public",name = "push_pop",
  value = push_pop_MosquitoPopFemale, overwrite = TRUE
)

#' Push pop
#'
#' Push new male mosquitoes into \code{\link{MosquitoPopMale}}.
#'  * This method is bound to \code{MosquitoPopMale$push_pop}
#'
#' @param N integer; number of emerging mosquitoes
#' @param tEmerge integer; day of emergence
#' @param genotype integer; genotype of emerging mosquitoes
#' @param locNow integer; site of emergence
#'
push_pop_MosquitoPopMale <- function(N, tEmerge, genotype, locNow){
  for(i in 1:N){

    # assign the mosquitoes
    myID = paste0(tEmerge,"_",i,"_",genotype)
    private$pop$assign(key = myID, value = MosquitoMale$new(id=myID,time=tEmerge,locNow=locNow,genotype=genotype,state=self$get_MBITES_PAR("initState"),mateFitness=self$get_mateFitness(genotype)))

    private$pop$get(myID)$set_FemalePopPointer(private$FemalePopPointer)
    private$pop$get(myID)$set_MalePopPointer(self)

    private$pop$get(myID)$set_TilePointer(private$TilePointer)
    private$pop$get(myID)$set_LandscapePointer(private$LandscapePointer)
    private$pop$get(myID)$set_HumansPointer(private$HumansPointer)

  }
}

MosquitoPopMale$set(which = "public",name = "push_pop",
  value = push_pop_MosquitoPopMale, overwrite = TRUE
)


###############################################################################
# Generic Methods
###############################################################################

#' Get pop
#'
#' Simply provides access to the \code{\link[MASHcpp]{HashMap}} object that contains the mosquito objects.
#'  * This method is bound to \code{MosquitoPopFemale$get_pop} and \code{MosquitoPopFemale$get_pop}
#'
get_pop_MosquitoPop <- function(){
  return(private$pop)
}

MosquitoPopFemale$set(which = "public",name = "get_pop",
  value = get_pop_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "get_pop",
  value = get_pop_MosquitoPop, overwrite = TRUE
)

#' Get Number of Alive Mosquitoes
#'
#' Provide the number of living mosquitoes.
#'  * This method is bound to \code{MosquitoPopFemale$get_N} and \code{MosquitoPopFemale$get_N}
#'
get_N_MosquitoPop <- function(){
  alive = private$pop$apply(tag="isAlive",returnVal=TRUE)
  return(sum(alive))
}

MosquitoPopFemale$set(which = "public",name = "get_N",
  value = get_N_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "get_N",
  value = get_N_MosquitoPop, overwrite = TRUE
)


#' Get MBITES_PAR
#'
#' Get an M-BITES parameter.
#'  * This method is bound to \code{MosquitoPopFemale$get_MBITES_PAR} and \code{MosquitoPopFemale$get_MBITES_PAR}
#'
#' @param par if given, return named parameter otherwise return entire list
#'
get_MBITES_PAR_MosquitoPop <- function(par = NULL){
  if(is.null(par)){
    return(private$MBITES_PAR)
  } else {
    return(private$MBITES_PAR[[par]])
  }
}

MosquitoPopFemale$set(which = "public",name = "get_MBITES_PAR",
  value = get_MBITES_PAR_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "get_MBITES_PAR",
  value = get_MBITES_PAR_MosquitoPop, overwrite = TRUE
)


#' Set MBITES_PAR
#'
#' Set M-BITES parameter list.
#'  * This method is bound to \code{MosquitoPopFemale$set_MBITES_PAR} and \code{MosquitoPopFemale$set_MBITES_PAR}
#'
set_MBITES_PAR_MosquitoPop <- function(MBITES_PAR){
  private$MBITES_PAR = MBITES_PAR
}

MosquitoPopFemale$set(which = "public",name = "set_MBITES_PAR",
  value = set_MBITES_PAR_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "set_MBITES_PAR",
  value = set_MBITES_PAR_MosquitoPop, overwrite = TRUE
)


###############################################################################
# Generic Pointers
###############################################################################

#' Get \code{\link{Landscape}} Pointer
#'
#' Return enclosing \code{Landscape$self} by refernce.
#'  * This method is bound to \code{MosquitoPopFemale$get_LandscapePointer} and \code{MosquitoPopMale$get_LandscapePointer}
#'
get_LandscapePointer_MosquitoPop <- function(){
  return(private$LandscapePointer)
}

MosquitoPopFemale$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_MosquitoPop, overwrite = TRUE
)


#' Set \code{\link{Landscape}} Pointer
#'
#' Set pointer to the enclosing \code{Landscape$self}.
#'  * This method is bound to \code{MosquitoPopFemale$set_LandscapePointer} and \code{MosquitoPopMale$set_LandscapePointer}
#'
#' @param LandscapePointer an environment
#'
set_LandscapePointer_MosquitoPop <- function(LandscapePointer){
  private$LandscapePointer = LandscapePointer
}

MosquitoPopFemale$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_MosquitoPop, overwrite = TRUE
)


#' Get \code{\link[MASHmacro]{HumanPop}} Pointer
#'
#' Return pointer to \code{HumanPop$self} within the same microsimulation tile by reference.
#'  * This method is bound to \code{MosquitoPopFemale$get_HumansPointer} and \code{MosquitoPopMale$get_HumansPointer}
#'
get_HumansPointer_MosquitoPop <- function(){
  return(private$HumansPointer)
}

MosquitoPopFemale$set(which = "public",name = "get_HumansPointer",
  value = get_HumansPointer_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "get_HumansPointer",
  value = get_HumansPointer_MosquitoPop, overwrite = TRUE
)

#' Set \code{\link[MASHmacro]{HumanPop}} Pointer
#'
#' Set pointer to \code{HumanPop$self} within the same microsimulation tile.
#'  * This method is bound to \code{MosquitoPopFemale$set_HumansPointer} and \code{MosquitoPopMale$set_HumansPointer}
#'
#' @param HumansPointer an environment
#'
set_HumansPointer_MosquitoPop <- function(HumansPointer){
  private$HumansPointer = HumansPointer
}

MosquitoPopFemale$set(which = "public",name = "set_HumansPointer",
  value = set_HumansPointer_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "set_HumansPointer",
  value = set_HumansPointer_MosquitoPop, overwrite = TRUE
)


#' Get \code{\link{Tile}} Pointer
#'
#' Return pointer to enclosing \code{Tile$self}.
#'  * This method is bound to \code{MosquitoPopFemale$get_TilePointer} and \code{MosquitoPopMale$get_TilePointer}
#'
get_TilePointer_MosquitoPop <- function(){
  return(private$TilePointer)
}

MosquitoPopFemale$set(which = "public",name = "get_TilePointer",
  value = get_TilePointer_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "get_TilePointer",
  value = get_TilePointer_MosquitoPop, overwrite = TRUE
)


#' Set \code{\link{Tile}} Pointer
#'
#' Set pointer to enclosing \code{Tile$self}.
#'  * This method is bound to \code{MosquitoPopFemale$set_TilePointer} and \code{MosquitoPopMale$set_TilePointer}
#'
#' @param TilePointer an environment
#'
set_TilePointer_MosquitoPop <- function(TilePointer){
  private$TilePointer = TilePointer
}

MosquitoPopFemale$set(which = "public",name = "set_TilePointer",
  value = set_TilePointer_MosquitoPop, overwrite = TRUE
)

MosquitoPopMale$set(which = "public",name = "set_TilePointer",
  value = set_TilePointer_MosquitoPop, overwrite = TRUE
)

###############################################################################
# Female-specific Pointers
###############################################################################

#' Get \code{\link{MosquitoPopMale}} Pointer
#'
#' Return \code{MosquitoPopMale$self} in the same \code{\link{Tile}} by reference.
#'  * This method is bound to \code{MosquitoPopFemale$get_MalePopPointer}
#'
get_MalePopPointer_MosquitoPopFemale <- function(){
  return(private$MalePopPointer)
}

MosquitoPopFemale$set(which = "public",name = "get_MalePopPointer",
  value = get_MalePopPointer_MosquitoPopFemale, overwrite = TRUE
)

#' Set \code{\link{MosquitoPopMale}} Pointer
#'
#' Set the pointer to \code{MosquitoPopMale$self} in the same \code{\link{Tile}} by reference.
#'  * This method is bound to \code{MosquitoPopFemale$set_MalePopPointer}
#'
#' @param MalePopPointer an environment
#'
set_MalePopPointer_MosquitoPopFemale <- function(MalePopPointer){
  private$MalePopPointer = MalePopPointer
}

MosquitoPopFemale$set(which = "public",name = "set_MalePopPointer",
  value = set_MalePopPointer_MosquitoPopFemale, overwrite = TRUE
)


###############################################################################
# Male-specific Pointers
###############################################################################

#' Get \code{\link{MosquitoPopFemale}} Pointer
#'
#' Return \code{MosquitoPopFemale$self} in the same \code{\link{Tile}} by reference.
#'  * This method is bound to \code{MosquitoPopMale$get_FemalePopPointer}
#'
get_FemalePopPointer_MosquitoPopMale <- function(){
  return(private$FemalePopPointer)
}

MosquitoPopMale$set(which = "public",name = "get_FemalePopPointer",
  value = get_FemalePopPointer_MosquitoPopMale, overwrite = TRUE
)

#' Set \code{\link{MosquitoPopFemale}} Pointer
#'
#' Set the pointer to \code{MosquitoPopMale$self} in the same \code{\link{Tile}} by reference.
#'  * This method is bound to \code{MosquitoPopMale$set_FemalePopPointer}
#'
#' @param MalePopPointer an environment
#'
set_FemalePopPointer_MosquitoPopMale <- function(FemalePopPointer){
  private$FemalePopPointer = FemalePopPointer
}

MosquitoPopMale$set(which = "public",name = "set_FemalePopPointer",
  value = set_FemalePopPointer_MosquitoPopMale, overwrite = TRUE
)
