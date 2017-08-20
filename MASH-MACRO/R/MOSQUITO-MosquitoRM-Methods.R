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
# MosquitoRM: Getters & Setters
###############################################################################

#' MosquitoRM: Return a Named Parameter
#'
#' Return a value from \code{private$parameters} given a character key.
#'
#' @param key a character key; return that value from the named parameter list.
#'
get_parameter_MosquitoRM <- function(key){
  return(private$parameters[[key]])
}

MosquitoRM$set(which = "public",name = "get_parameter",
  value = get_parameter_MosquitoRM,
  overwrite = TRUE)

get_migration <- function(){
  private$PatchPointer$get_TilePointer()$get_migrationRow(ix = private$patchID)
}


###############################################################################
# MosquitoRM: Data Output
###############################################################################

write_CSV_MosquitoRM <- function(){

  popOut = paste0(c(private$PatchPointer$get_tNow(),private$patchID,private$M,private$Y,private$Z),collapse = ",")
  writeLines(text = popOut, con = private$PatchPointer$conMosquito,sep = "\n")

}

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
  private$Y0 = private$par$f * private$par$Q private$PatchPointer$kappa * (private$M - private$Y)

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
