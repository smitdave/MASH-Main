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

###############################################################################
# MosquitoRM: Ross-Macdonald Difference Equations
###############################################################################

#' MosquitoRM: Run Daily Population Dynamics
#'
#' do something.
#'
#' @param some parameter
#'
oneDay_popDynamics_MosquitoRM <- function(){

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
    privateY = private$psi %*% private$Y
    privateZ = private$psi %*% private$Z + private$ZZ[1,]
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

MosquitoRM$set(which = "public",name = "oneDay_popDynamics",
          value = oneDay_popDynamics_MosquitoRM, overwrite = TRUE
)


###############################################################################
# MosquitoRM: Oviposition
###############################################################################

layEggs_Emerge_MosquitoRM <- function(){

}
