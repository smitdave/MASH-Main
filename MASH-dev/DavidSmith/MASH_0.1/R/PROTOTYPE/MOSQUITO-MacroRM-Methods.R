#######################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MOSQUITO: MacroRM Mosquito Methods
#   David Smith, Hector Sanchez, Sean Wu
#   August 16, 2017
#
#######################################################################


#' MACRO: Daily Ross-MacDonald Difference Equations for \code{link{MacroRMMosquito}}
#'
#' Run daily Ross-MacDonald difference equation dynamics for basic mosquito model.
#'
#'  * This method is bound to \code{MacroRMMosquito$oneDay_RM()}.
#' @md
MacroRM_lifeCycle_RM <- function(){

  # get daily EIP
  EIP = self$get_EIP(private$TilePointer$get_tNow())

  # newly infected mosquitoes
  private$Y0 = self$get_parameter("f") * self$get_parameter("Q") * private$PatchPointer$get_kappa() * (private$M - private$Y)

  # life cycle model
  private$M = (self$get_parameter("p") * private$M) + private$PatchPointer$emergingAdults()
  private$Y = self$get_parameter("p") * (private$Y + private$Y0)
  private$Z = (self$get_parameter("p") * private$Z) + private$ZZ[1]

  # EIP
  private$ZZ[1] = 0
  private$ZZ[-self$get_parameter("maxEIP")] = private$ZZ[-1]
  private$ZZ[EIP] = (self$get_parameter("p")^EIP) * private$Y0

}

#' MACRO: Migration Out for \code{link{MacroRMMosquito}}
#'
#' Migrate mosquitoes out of the focal well-mixed patch. Set migration vectors prior to letting \code{\link{MacroTile}} sync the migration.
#'
#'  * This method is bound to \code{MacroRMMosquito$migrationOut_RM()}.
#' @md
MacroRM_migrationOut_RM <- function(){

  # migrate mosquitoes
  movement = private$get_movement(private$patchID)

  private$migrateM = private$M %*% movement
  private$migrateY = private$Y %*% movement
  private$migrateZ = private$Z %*% movement
  private$migrateZZ = matrix(data=private$ZZ,ncol=1,nrow=private$TilePointer$get_nPatch()) %*% movement

}

#' MACRO: Migration In for \code{link{MacroRMMosquito}}
#'
#' After collecting migration in \code{\link{MacroTile}}, migrate mosquitoes back to focal \code{\link{MacroPatch}}
#'
#'  * This method is bound to \code{MacroRMMosquito$migrationIn_RM()}.
#' @md
MacroRM_migrationIn_RM <- function(migrateM_in, migrateY_in, migrateZ_in, migrateZZ_in){

  private$M = migrateM_in
  private$Y = migrateY_in
  private$Z = migrateZ_in
  private$ZZ = migrateZZ_in

}
