#################################################################
#
#   MASH
#   MICRO Mosquito Populations
#   Methods (assigned directly to classes)
#   David Smith, Hector Sanchez, Sean Wu
#   August 9, 2017
#
#################################################################


#################################################################
# MicroMosquitoPopFemale
#################################################################

#' \code{\link{MicroMosquitoPopFemale}} Method: Find Indices of Alive Mosquitoes
#'
#' Find indices of non-null, non-dead mosquitoes in population.
#'  * This method is bound to \code{MicroMosquitoPopFemale$which_alive}
#'
#' @md
MicroMosquitoPopFemale_which_alive <- function(){
  return(
    which(vapply(X = private$pop, FUN = function(x){
         if(is.null(x)){
           return(FALSE)
         } else {
           if(x$isAlive()){
             return(TRUE)
           } else {
             return(FALSE)
           }
         }
      }, FUN.VALUE = logical(1)))
   )
}

MicroMosquitoPopFemale$set(which = "public",name = "which_alive",
          value = MicroMosquitoPopFemale_which_alive,
          overwrite = TRUE
)


#' \code{\link{MicroMosquitoPopFemale}} Method: Update \code{nullPop} Index of \code{NULL} Entries
#'
#' Update indices of \code{NULL} mosquitoes used for memory allocation.
#'  * This method is bound to \code{MicroMosquitoPopFemale$update_nullPop}
#'
#' @md
MicroMosquitoPopFemale_update_nullPop <- function(){
  private$nullPop = which(vapply(X = private$pop,FUN = is.null,FUN.VALUE = logical(1)))
}

MicroMosquitoPopFemale$set(which = "public",name = "update_nullPop",
          value = MicroMosquitoPopFemale_update_nullPop,
          overwrite = TRUE
)


#' \code{\link{MicroMosquitoPopFemale}} Method: Push Emerging Mosquitoes to Adult Population
#'
#' Used to handle emergence; typically from ImagoQ to the population.
#'  * This method is bound to \code{MicroMosquitoPopFemale$push_pop}
#'
#' @md
MicroMosquitoPopFemale_push_pop <- function(N, tEmerge, ix, genotype, damID, sireID){

  # if not enough NULL indices, expand the vector
  if(N >= length(private$nullPop)){

    # extend the population
    self$extend_pop()

    # update nullPop indices
    self$update_nullPop()
  }

  # push the mosquitoes
  for(i in 1:N){
    private$pop[[private$nullPop[i]]] = MicroMosquitoFemale$new(id = paste0(tEmerge,"_",private$nullPop[i]), time = tEmerge, ix = ix, genotype = genotype, state = private$initState)
    private$pop[[private$nullPop[i]]]$set_FemalePopPointer(self)
    private$pop[[private$nullPop[i]]]$set_MalePopPointer(private$MalePopPointer)
    private$pop[[private$nullPop[i]]]$set_LandscapePointer(private$LandscapePointer)
    private$pop[[private$nullPop[i]]]$set_HumansPointer(private$HumansPointer)
    private$pop[[private$nullPop[i]]]$set_TilePointer(private$TilePointer)
  }

  # update nullPop indices
  self$update_nullPop()

}

MicroMosquitoPopFemale$set(which = "public",name = "push_pop",
          value = MicroMosquitoPopFemale_push_pop,
          overwrite = TRUE
)


#' \code{\link{MicroMosquitoPopFemale}} Method: Extend the Population Vector
#'
#' Used to handle emergence; typically from ImagoQ to the population.
#'  * This method is bound to \code{MicroMosquitoPopFemale$extend_pop}
#'
#' @md
MicroMosquitoPopFemale_extend_pop <- function(){

 N = length(private$pop)

 nullList = vector(mode="list",length=N)
 private$pop = c(private$pop,nullList)

}

MicroMosquitoPopFemale$set(which = "public",name = "extend_pop",
          value = MicroMosquitoPopFemale_extend_pop,
          overwrite = TRUE
)


#' \code{\link{MicroMosquitoPopFemale}} Method: Push Emerging Mosquitoes to Adult Population
#'
#' Manage the pop vector (find dead mosquitoes; if 'con' is provided, write their histories out to JSON).
#'  * This method is bound to \code{MicroMosquitoPopFemale$clear_pop}
#'
#' @md
MicroMosquitoPopFemale_clear_pop <- function(historyTrack = FALSE){

  deadIx = which(vapply(X = private$pop, FUN = function(x){
       if(is.null(x)){
         return(FALSE)
       } else {
         if(x$isAlive()){
           return(FALSE)
         } else {
           return(TRUE)
         }
       }
    }, FUN.VALUE = logical(1)))

 if(historyTrack){
   histories = lapply(X = private$pop[deadIx],FUN = function(x){x$get_history()})
   names(histories) = vapply(X = private$pop[deadIx], FUN = function(x){x$get_id()}, FUN.VALUE = character(1))
 }

 for(ix in deadIx){
   private$pop[[ix]] = NULL
 }

 # write the list out to JSON
 if(historyTrack){
   fileName = paste0("historyF",private$TilePointer$get_tNow(),".json")
   con = file(description = paste0(private$TilePointer$get_directory(),"MOSQUITO/",fileName),open = "wt")
   writeLines(text = jsonlite::toJSON(x = histories,pretty = TRUE),con = con)
   close(con)
 }

 # update_nullPop
 self$update_nullPop()
}

MicroMosquitoPopFemale$set(which = "public",name = "clear_pop",
          value = MicroMosquitoPopFemale_clear_pop,
          overwrite = TRUE
)


#' \code{\link{MicroMosquitoPopFemale}} Method: Update Adult Population
#'
#' Mostly used to update the population after fitting EL4P (see \code{\link{EL4P.Mesh.Fit}}) with equilibrium mosquito density and site locations.
#'  * This method is bound to \code{MicroMosquitoPopFemale$update_pop}
#'
#' @md
MicroMosquitoPopFemale_update_pop <- function(N, time_init=0, ix_init, genotype_init, module){

  # update the initial state depending on the M-BITES module used
  switch(module,
     BRO = {private$initState = "B"},
     BROM = {private$initState = "M"},
     BROS = {private$initState = "B"},
     BROMS = {private$initState = "M"},
     FULL = {private$initState = "M"},
     {stop("unrecognized M-BITES lifecycle module selection")}
   )

   # allocate population
   private$pop = vector(mode="list",length=(N*5))
   if(length(ix_init) != length(genotype_init)){
     stop("one or more of the input vectors to MicroMosquitoPopFemale initializer is not the same length")
   }

   # allocate initial cohort
   for(ix in 1:N){
     # make new mosquito
     private$pop[[ix]] = MicroMosquitoFemale$new(id = paste0(time_init,"_",ix), time = time_init, ix = ix_init[ix], genotype = genotype_init[ix], state = private$initState)
     # set pointers
     private$pop[[ix]]$set_FemalePopPointer(self)
     private$pop[[ix]]$set_MalePopPointer(private$MalePopPointer)
     private$pop[[ix]]$set_TilePointer(private$TilePointer)
     private$pop[[ix]]$set_LandscapePointer(private$LandscapePointer)
     private$pop[[ix]]$set_HumansPointer(private$HumansPointer)
   }
   # find NULL indices
   self$update_nullPop()
}

MicroMosquitoPopFemale$set(which = "public",name = "update_pop",
          value = MicroMosquitoPopFemale_update_pop,
          overwrite = TRUE
)


#################################################################
# MicroMosquitoPopMale
#################################################################
