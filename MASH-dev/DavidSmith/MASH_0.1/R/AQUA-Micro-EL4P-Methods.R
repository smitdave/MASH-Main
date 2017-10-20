#################################################################
#
#       ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MICRO Aquatic Ecology: EL4P
#   Method definitions
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 7, 2017
#
#################################################################

#################################################################
# Landscape and AquaticSite Methods
#################################################################

#' Update \code{\link{Landscape}} and \code{\link{AquaticSite}} after Fitting EL4P Aquatic Ecology Module
#'
#' This function updates parameters psi, alpha, and p in a \code{\link{Landscape}} and \code{\link{AquaticSite}}
#' It modifies \code{\link{AquaticSite}} and \code{\link{Landscape}} classes.
#'
#'  * This method is bound to \code{Landscape$updateLandscapeEL4P}
#'
#' @param EL4P_fit output of \code{\link{EL4P.Mesh.Fit}}
#' @param EL4P_PAR output of \code{\link{EL4P.Parameters}}
#'
#' @md
updateLandscape_MicroEL4P <- function(EL4P_fit, EL4P_PAR){

  for(ix in 1:self$AquaSitesN){

    # set parameters of the EL4P pool
    private$AquaSites[[ix]]$get_EL4P()$set_psi(EL4P_fit$psi[ix])
    private$AquaSites[[ix]]$get_EL4P()$set_alpha(EL4P_fit$alpha[ix])
    private$AquaSites[[ix]]$get_EL4P()$set_p(EL4P_PAR$p)

    # set the equilibrium populations of the EL4P pool
    private$AquaSites[[ix]]$get_EL4P()$set_pop(EL4P_fit$equilibriumPops[[ix]])

  }

}


#################################################################
# One Step 'EL4P'
#################################################################

#' MICRO \code{\link{AquaticSite}} Method: EL4P One Day Dynamics
#'
#' Collect eggs from the \code{\link{EggQ}} in this site and move \code{\link{EL4P}}, and run the one step difference equation dynamics for aquatic development,
#' finally, move emerging adults from the pool into the \code{\link{ImagoQ}} for all genotypes.
#'  * This method is bound to \code{AquaticSite$oneStep_EL4PSite()}.
#'
#' @param tNow current global time of \code{\link{MicroTile}}
#' @md
oneStep_MicroEL4PSite <- function(tNow){

  # move eggs from EggQ to EL4P
  eggs = private$EggQ$get_EggQTime(tNow = tNow,clear = TRUE)
  if(length(eggs) > 0){
    for(i in 1:length(eggs)){
      private$EL4P$addEggs(eggs_N = eggs[[i]]$N, genotype = eggs[[i]]$genotype)
    }
  }

  # run daily EL4P difference equations
  private$EL4P$oneStep()

  # move emerging adults from EL4P to ImagoQ
  for(i in 1:private$numGenotypes){
    genotypeLambda = private$EL4P$get_specificLambda(ix=i-1)
    genotypeEmerge = rpois(n=1,lambda=genotypeLambda)
    if(genotypeLambda>0){
      private$ImagoQ$add_ImagoQ(genotypeEmerge,tNow,(i-1),"-1","-1")
    }
  }

}

#' MICRO \code{\link{Landscape}} Method: EL4P One Day Dynamics
#'
#' Run daily EL4P module aquatic dynamics for all sites. See \code{\link{oneStep_MicroEL4PSite}} for details.
#' This function fills a generic for \code{\link{simMICRO_oneStep}}.
#'  * This method is bound to \code{Landscape$oneStep_AquaticEcology()}.
#'
#' @md
oneStep_MicroEL4P <- function(){
  tNow = private$TilePointer$get_tNow()
  for(ixA in 1:self$AquaSitesN){
    private$AquaSites[[ixA]]$oneStep_EL4PSite(tNow)
  }
}

#' MICRO \code{\link{Landscape}} Method: Get Emerging Adults from ImagoQ and Zero out ImagoQ
#'
#' Grab emerging adult batches where tEmerge <= tNow and add to the \code{\link{MicroMosquitoPopFemale}}.
#' This is a helper function for \code{\link{addCohort_MicroEmerge}}.
#'  * This method is bound to \code{AquaticSite$addCohort_MicroEL4PSite()}.
#'
#' @param tNow current global time of \code{\link{MicroTile}}
#' @md
addCohort_MicroEL4PSite <- function(tNow){

  EmergingAdults = private$ImagoQ$get_ImagoQTime(tNow = tNow,clear = TRUE)

  if(length(EmergingAdults) > 0){
    for(i in 1:length(EmergingAdults)){
      private$LandscapePointer$get_FemalePopPointer()$push_pop(N = EmergingAdults[[i]]$N, tEmerge = EmergingAdults[[i]]$tEmerge, ix = private$ix, genotype = EmergingAdults[[i]]$genotype, damID = EmergingAdults[[i]]$damID, sireID = EmergingAdults[[i]]$sireID)
    }
  }

  rm(EmergingAdults)
}

#' MICRO \code{\link{Landscape}} Method: Get Emerging Adults from ImagoQ and Zero out ImagoQ
#'
#' Grab emerging adult batches where tEmerge <= tNow and add to the \code{\link{MicroMosquitoPopFemale}}.
#' This function fills a generic for \code{\link{simMICRO_oneStep}}.
#'  * This method is bound to \code{Landscape$addCohort()}
#'
#'
#' @md
addCohort_MicroEL4P <- function(){
  tNow = private$TilePointer$get_tNow()
  for(ixA in 1:self$AquaSitesN){
    private$AquaSites[[ixA]]$addCohort_MicroEL4PSite(tNow)
  }
}
