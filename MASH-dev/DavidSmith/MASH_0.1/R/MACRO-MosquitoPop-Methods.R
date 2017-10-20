#################################################################
#
#   MASH
#   R6-ified
#   MACRO MosquitoPop Class Methods
#   David Smith, Hector Sanchez, Sean Wu
#   May 22, 2016
#
#################################################################


#################################################################
# EIP
#################################################################

# for now just add methods straight to class object; can make a seperate MACRO.MosquitoPop.Setup() later if necessary.

#' Set \code{MacroMosquitoPop} getEIP
#'
#' get time dependent EIP
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
getEIP_MacroMosquitoPop <- function(tNow){
  # update later
  return(private$EIP)
}

MacroMosquitoPop$set(which = "public",name = "getEIP",
          value = getEIP_MacroMosquitoPop,
          overwrite = TRUE
)


#################################################################
# Oviposition
#################################################################

#' \code{MacroMosquitoPop} layEggs
#'
#' oviposition
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
layEggs_MacroMosquitoPop <- function(){
  for(ixP in 1:self$get_PatchesPointer()$get_N()){
    eggs = private$M[ixP] * self$get_PatchesPointer()$get_aquaP(ix = ixP) * private$v * private$f
    self$get_PatchesPointer()$set_PatchesEggQ(PatchesEggQ = newEgg(N = eggs, tOviposit = self$get_TilePointer()$get_tNow(), damID = 0L, sireID = 0L, genotype = NULL), ix = ixP)
  }
}

MacroMosquitoPop$set(which = "public",name = "layEggs",
          value = layEggs_MacroMosquitoPop,
          overwrite = TRUE
)

#################################################################
# oneDay_RM (daily Ross-Macdonald difference equations)
#################################################################

#' MACRO \code{MacroMosquitoPop} Method: oneDay_RM
#'
#' Daily Ross-Macdonald difference/diffusion equations for mosquito dynamics.
#' This method is bound to \code{MacroMosquitoPop$oneDay_RM}
#' These equations are likely inefficient; see optimizing BLAS/LAPACK and copying less matricies for a faster down the line implementation.
#'
oneDay_RM <- function(){

  # browser() # DEBUG

  # if(any(is.nan(private$Z))){browser()} # DEBUG

  #Mosquito Survival
  M = private$p * private$M
  Y = private$p * private$Y
  Z = private$p * private$Z
  ZZ = private$ZZ

  # if(any(is.nan(Z) || any(is.nan(ZZ)))){browser()} # DEBUG

  # Infected Mosquitoes
  Y0 = private$f * private$Q * private$PatchesPointer$get_kappa() * (M - Y)
  if(any(Y0<0)){
    ix = which(Y0<0)
    Y0[ix] = 0
  }
  # PATCH_CODE : maybe floor M-Y at 0
  # Y0 = private$f * private$Q * private$PatchesPointer$get_kappa() * M
  Y = Y + Y0

  # Migration & Sporozoite Maturation
  if(!is.null(private$psi)){
    M = private$psi %*% M
    Y = private$psi %*% Y
    Z = private$psi %*% Z + ZZ[1,]
  } else {
    Z = Z + ZZ[1,]
  }

  # if(any(is.nan(Z) || any(is.nan(ZZ)))){browser()} # DEBUG

  ZZ[-private$maxEIP,] = ZZ[-1,]
  ZZ[private$maxEIP,] = 0

  # if(any(is.nan(Z) || any(is.nan(ZZ)))){browser()} # DEBUG

  EIP = self$getEIP(t)
  if(!is.null(private$psi)){
    ZZ[EIP,] = ZZ[EIP,] + private$P[EIP] * private$Psi %*% Y0
  } else {
    ZZ[EIP,] = ZZ[EIP,] + private$P[EIP] * Y0
  }

  # if(any(is.nan(Z) || any(is.nan(ZZ)))){browser()} # DEBUG

  private$M = M
  private$Y = Y
  private$Z = Z
  private$ZZ = ZZ

}

MacroMosquitoPop$set(which = "public",name = "oneDay_RM",
          value = oneDay_RM,
          overwrite = TRUE
)
