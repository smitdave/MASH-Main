#################################################################
#
#   MASH
#   R6-ified
#   Human Biting-related Methods for MACRO
#   David Smith, Hector Sanchez, Sean Wu
#   May 23, 2017
#
#################################################################


#################################################################
# Kappa: update the human component of kappa
#################################################################

#' MACRO: Update \code{\link{Human}} PfSI kappa For a Patch
#'
#' Add my contribution to kappa to current patch (current value of \code{location}).
#' This method is bound to \code{Human$sumKappa()}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_sumKappa_PfSI <- function(){
  if(private$Pathogens$get_infected()){
    self$get_PatchesPointer()$accumulate_kappa(kappa = (private$bWeight*private$Pathogens$get_c()), ix = private$location)
  } else {
    self$get_PatchesPointer()$accumulate_kappa(kappa = 0, ix = private$location)
  }
}

#' MACRO: Update \code{\link{Human}} PfMOI kappa For a Patch
#'
#' Add my contribution to kappa to current patch (current value of \code{location}).
#' This method is bound to \code{Human$sumKappa()}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_sumKappa_PfMOI <- function(){
  if(private$Pathogens$get_MOI() > 0){
    self$get_PatchesPointer()$accumulate_kappa(kappa = (private$bWeight*private$Pathogens$get_c()), ix = private$location)
  } else {
    self$get_PatchesPointer()$accumulate_kappa(kappa = 0, ix = private$location)
  }
}

#' MACRO: Update \code{HumanPop} kappa For all Patches
#'
#' Update normalized biting propensities (kappa) for all patches.
#' This method is bound to \code{HumanPop$updateKappa()}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_updateKappa <- function(){
  for(ixH in 1:self$nHumans){
    private$pop[[ixH]]$sumKappa()
  }
  newKappa = (self$get_PatchesPointer()$get_kappa() / (self$get_PatchesPointer()$get_bWeightHuman() + self$get_PatchesPointer()$get_bWeightZoo() + self$get_PatchesPointer()$get_bWeightZootox()))
  noPeople = which(is.nan(newKappa)) # PATCH_CODE
  newKappa[noPeople] = 0 # PATCH_CODE
  self$get_PatchesPointer()$set_kappa(kappa = newKappa)
}


#################################################################
# expectedBites: how much do I expect to be bitten at a patch?
#################################################################

#' Get \code{Human} myEIR
#'
#' Write me!
#' This method is bound to \code{Human$get_myEIR()}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_get_myEIR <- function(){
  return(private$myEIR)
}

#' Set \code{Human} myEIR
#'
#' Write me!
#' This method is bound to \code{Human$set_myEIR()}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_set_myEIR <- function(myEIR){
  private$myEIR = myEIR
}

#' MACRO \code{Human} Method: expectedBites
#'
#' Write me! a method for \code{\link{Human}}
#' This method is bound to \code{Human$expectedBites()}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_expectedBites <- function(){
  here = self$get_location() # where am i now?
  newEIR = self$get_bWeight() * (self$get_MosquitoPointer()$get_f()*self$get_MosquitoPointer()$get_Z(here)) / (self$get_PatchesPointer()$get_bWeightHuman(here) + self$get_PatchesPointer()$get_bWeightZoo(here) + self$get_PatchesPointer()$get_bWeightZootox(here))
  self$set_myEIR(newEIR)
}


#' MACRO \code{Human} Method: add2Q_Bites
#'
#' Write me! a method for \code{\link{Human}}
#' This method is bound to \code{Human$add2Q_Bites()}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_add2Q_Bites <- function(tEvent, PAR){
  for(ixB in 1:PAR$nBites){ # multiple bites in one day; in RM-Macro time is only resolved to day for mosquitoes
    self$add2Q_SimBitePfSI(tEvent = tEvent)
  }
}

#' MACRO \code{HumanPop} Method: queueInfectiousBites
#'
#' Write me! a method for \code{\link{HumanPop}}
#' This method is bound to \code{HumanPop$queueInfectiousBites()}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
MacroHuman_queueInfectiousBites <- function(){
  for(ixH in 1:self$nHumans){
    private$pop[[ixH]]$expectedBites() # update expectedBites (EIR)
    mu = private$pop[[ixH]]$get_myEIR() # my expected EIR
    nBites = rnbinom(n = 1,mu = mu, size = 0.1) # number of bites
    if(nBites > 0){
      private$pop[[ixH]]$add2Q_Bites(tEvent = self$get_TilePointer()$get_tNow(), PAR = list(nBites = nBites)) # add bites to queue
    }
  }
}
