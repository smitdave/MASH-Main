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

  EIP = private$EIP[private$TilePointer$get_tNow()%%365]

  # daily dynamics
  private$M = private$p * private$M
  private$Y = private$p * private$Y
  private$Z = private$p * private$Z

  # number of newly infected mosquitoes
  kappa = private$TilePointer$get_Patches()$apply(tag="get_kappa",returnVal=TRUE)
  private$Y0 = private$f * private$Q * unlist(kappa,use.names = FALSE) * (private$M - private$Y)
  if(any(private$Y0<0)){
    private$Y0[which(private$Y0<0)] = 0
  }

  private$Y = private$Y + private$Y0

  # Migration & Sporozoite Maturation
  if(!is.null(private$psi)){
    private$M = as.vector(private$psi %*% private$M)
    private$Y = as.vector(private$psi %*% private$Y)
    private$Z = as.vector(private$psi %*% private$Z + private$ZZ[1,])
  } else {
    private$Z = private$Z + private$ZZ[1,]
  }

  private$ZZ[-private$maxEIP,] = private$ZZ[-1,]
  private$ZZ[private$maxEIP,] = 0

  if(!is.null(private$psi)){
    private$ZZ[EIP,] = private$ZZ[EIP,] + private$P[EIP] * as.vector(private$psi %*% private$Y0)
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
#'  * This method is bound to \code{Mosquito_RM$oneDay_oviposition}
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


###############################################################################
# Output
###############################################################################

#' Initialize Population Output
#'
#' Initialize mosquito population output
#'
#'  * This method is bound to \code{Mosquito_RM$initOutput}
#'
initOutput_Mosquito_RM <- function(con){
  writeLines(text = paste0(c("time",paste0("patch",1:private$TilePointer$get_nPatch())),collapse = ","),con = con, sep = "\n")
}

Mosquito_RM$set(which = "public",name = "initOutput",
          value = initOutput_Mosquito_RM, overwrite = TRUE
)

#' Write Population Output
#'
#' Write mosquito population output
#'
#'  * This method is bound to \code{Mosquito_RM$output}
#'
output_Mosquito_RM <- function(con){
  tNow = private$TilePointer$get_tNow()
  writeLines(text = paste0(c(tNow,private$M),collapse = ","), con = con, sep = "\n")
  writeLines(text = paste0(c(tNow,private$Y),collapse = ","), con = con, sep = "\n")
  writeLines(text = paste0(c(tNow,private$Z),collapse = ","), con = con, sep = "\n")
}

Mosquito_RM$set(which = "public",name = "output",
          value = output_Mosquito_RM, overwrite = TRUE
)

###############################################################################
# Reset
###############################################################################

#' Reset the Mosquito Population
#'
#' Reset mosquito population between simulation runs
#'
#'  * This method is bound to \code{Mosquito_RM$reset}
#'
reset_Mosquito_RM <- function(MosquitoPar){
  private$M             = MosquitoPar$M
  private$Y             = private$Y*0
  private$Z             = private$Z*0
  private$ZZ            = private$ZZ*0
}

Mosquito_RM$set(which = "public",name = "reset",
          value = reset_Mosquito_RM, overwrite = TRUE
)
