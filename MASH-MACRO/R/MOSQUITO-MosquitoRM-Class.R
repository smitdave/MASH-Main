###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MACRO
#   MACRO: MosquitoRM Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   August 20, 2017
#
###############################################################################

#' MosquitoRM Class Definition
#'
#' Generate a single well-mixed mosquito population which lives in a \code{\link{MacroPatch}} which lives according to Ross-Macdonald style difference equations.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @section **Methods**:
#'  * get_pop: see \code{\link{get_pop_HumanPop}}
#'  * get_human: see \code{\link{get_human_HumanPop}}
#'  * get_history: see \code{\link{get_history_HumanPop}}
#'  * simHumans: see \code{\link{simHumans_HumanPop}}
#'
#' @section **Fields**:
#'  * N: number of human
#'  * tStart: time to start simulation
#'  * pop: a object of class \code{\link[MASHcpp]{HashMap}} that stores instantiations of \code{\link{Human}}, see help for more details on the internal structure of this type.
#'
#'
#'
#'
#'
#'
#'
#' @md
#' @export
MosquitoRM <- R6::R6Class(classname="MosquitoRM",
                     portable = TRUE,
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(patchID, M, Y = 0, Z = 0, par){

                         # patchID
                         private$patchID = patchID

                         # set population state variables
                         private$M = M
                         private$Y = Y
                         private$Z = Z
                         private$ZZ = matrix(data = rep(0,times=par$maxEIP),ncol=1)

                         private$M_out = 0
                         private$Y_out = 0
                         private$Y0_out = 0
                         private$Z_out = 0

                         # set par
                         private$par = par

                       }

                     ),

                     #private members
                     private = list(

                       # fields
                       patchID = NULL,

                       M = NULL,
                       Y = NULL,
                       Y0 = NULL,
                       Z = NULL,
                       ZZ = NULL,

                       M_out = NULL,
                       Y_out = NULL,
                       Y0_out = NULL,
                       Z_out = NULL,

                       par = NULL,

                       # pointers
                       PatchPointer = NULL,
                       HumanPopPointer = NULL

                     )

) #end class definition


###############################################################################
# MosquitoRM: Generic & Shared Methods
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
  return(private$par[[key]])
}

MosquitoRM$set(which = "public",name = "get_parameter",
  value = get_parameter_MosquitoRM,
  overwrite = TRUE)

get_migration_MosquitoRM <- function(){
  private$PatchPointer$get_TilePointer()$get_migrationRow(ix = private$patchID)
}

MosquitoRM$set(which = "public",name = "get_migration",
  value = get_migration_MosquitoRM,
  overwrite = TRUE)

###############################################################################
# MosquitoRM: Data Output
###############################################################################

write_CSV_MosquitoRM <- function(){

  popOut = paste0(c(private$PatchPointer$get_tNow(),private$patchID,private$M,private$Y,private$Z),collapse = ",")
  writeLines(text = popOut, con = private$PatchPointer$conMosquito,sep = "\n")

}

MosquitoRM$set(which = "public",name = "write_CSV",
  value = write_CSV_MosquitoRM,
  overwrite = TRUE)
