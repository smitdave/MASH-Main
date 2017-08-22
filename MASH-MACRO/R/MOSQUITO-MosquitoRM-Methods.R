###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MACRO
#   MACRO: MosquitoRM Methods
#   David Smith, Hector Sanchez, Sean Wu
#   August 20, 2017
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
run_popDynamics_MosquitoRM <- function(){

  EIP = self$get_EIP(tNow = private$PatchPointer$get_tNow())

  # number of newly infected mosquitoes
  private$Y0 = private$par$f * private$par$Q * private$PatchPointer$get_kappa() * (private$M - private$Y)

  # daily dynamics
  private$M = (private$par$p * private$M) + private$PatchPointer$addCohort()
  private$Y = private$par$p * (private$Y + private$Y0)
  private$Z = (private$par$p * private$Z) + private$ZZ[1]

  # progression through EIP
  private$ZZ[1] = 0
  private$ZZ[-private$par$maxEIP] = ZZ[-1] # shift up by one
  private$ZZ[EIP] = private$ZZ[EIP] + (private$p^EIP * private$Y0)

}

#' MosquitoRM: Run Daily Inter-patch Outbound Migration
#'
#' do something.
#'
#' @param some parameter
#'
run_MigrationOut_MosquitoRM <- function(){

  migration = self$get_migration()

  private$M_out = private$M %*% migration
  private$Y_out = private$Y %*% migration
  private$Z_out = private$Z %*% migration
  private$Y0_out = private$Y0 %*% migration

}

#' MosquitoRM: Run Daily Inter-patch Inbound Migration
#'
#' do something.
#'
#' @param some parameter
#'
run_MigrationOut_MosquitoRM <- function(M_in, Y_in, Z_in, Y0_in){

  private$M = M_in
  private$Y = Y_in
  private$Z = Z_in
  private$Y0 = Y0_in

}




###############################################################################
# MacroTile: Inter-patch Migration
###############################################################################


run_Migration_MicroTile <- function(){

}
