###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MACRO
#   MosquitoRM Simulation
#   MASH Team
#   November 2017
#
###############################################################################

#' Run Ross-MacDonald Mosquito Daily Population Dynamics
#'
#' Run mosquito populations through single step of Ross-MacDonald style population dynamics and diffusion between patches.
#'
#'  * This method is bound to \code{Mosquito_RM$oneDay_popDynamics}
#'
oneDay_popDynamics_Mosquito_RM <- function(){

  EIP = self$get_EIP(tNow = private$PatchPointer$get_tNow())

  # daily dynamics
  private$M = private$p * private$M
  private$Y = private$p * private$Y
  private$Z = private$p * private$Z

  # number of newly infected mosquitoes
  private$Y0 = private$f * private$Q * private$PatchPointer$get_kappa() * (private$M - private$Y)
  if(any(private$Y0<0)){
    private$Y0[which(private$Y0<0)] = 0
  }

  private$Y = private$Y + private$Y0

  # Migration & Sporozoite Maturation
  if(!is.null(private$psi)){
    private$M = private$psi %*% private$M
    private$Y = private$psi %*% private$Y
    private$Z = private$psi %*% private$Z + private$ZZ[1,]
  } else {
    private$Z = private$Z + private$ZZ[1,]
  }

  private$ZZ[-private$maxEIP,] = private$ZZ[-1,]
  private$ZZ[private$maxEIP,] = 0

  if(!is.null(private$psi)){
    private$ZZ[EIP,] = private$ZZ[EIP,] + private$P[EIP] * private$Psi %*% private$Y0
  } else {
    private$ZZ[EIP,] = private$ZZ[EIP,] + private$P[EIP] * private$Y0
  }

}

Mosquito_RM$set(which = "public",name = "oneDay_popDynamics",
          value = oneDay_popDynamics_Mosquito_RM, overwrite = TRUE
)


#' Placeholder Function for Oviposition
#'
#' Run mosquito populations through single step of Ross-MacDonald style population dynamics and diffusion between patches.
#'
#'  * This method is bound to \code{Mosquito_RM$oneDay_popDynamics}
#'
oneDay_oviposition_Mosquito_RM <- function(){
  return(NULL)
}

Mosquito_RM$set(which = "public",name = "oneDay_oviposition",
          value = oneDay_oviposition_Mosquito_RM, overwrite = TRUE
)

#' Recieve Emerging Adults
#'
#' Recieves a number of emerging adults in a certain patch. This method directly interfaces with \code{\link{oneDay_addCohort_AquaPop_Base}}
#'
#'  * This method is bound to \code{Mosquito_RM$get_emergingAdults}
#'
get_emergingAdults_Mosquito_RM <- function(M,ix){
  private$M[ix] = private$M[ix] + M
}

Mosquito_RM$set(which = "public",name = "get_emergingAdults",
          value = get_emergingAdults_Mosquito_RM, overwrite = TRUE
)
