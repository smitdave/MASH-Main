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
#'  * i'm a field: write me
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

                       initialize = function(MosquitoRM_PAR){

                         N = nrow(MosquitoRM_PAR$psi)

                         private$p             = MosquitoRM_PAR$p
                         private$f             = MosquitoRM_PAR$f
                         private$Q             = MosquitoRM_PAR$Q
                         private$v             = MosquitoRM_PAR$v
                         private$EIP           = MosquitoRM_PAR$EIP
                         private$maxEIP        = MosquitoRM_PAR$maxEIP

                         private$M             = MosquitoRM_PAR$M_density
                         private$Y             = rep(0L, N) # infected (incubating)
                         private$Z             = rep(0L, N) # infectious
                         private$ZZ            = matrix(data=0L,nrow=MosquitoRM_PAR$maxEIP,ncol=N) # each row is the number that will be added to the infectious state on that day

                         private$psi           = MosquitoRM_PAR$psi
                         private$P             = MosquitoRM_PAR$p^c(1:MosquitoRM_PAR$maxEIP) # survival over EIP

                       }

                     ),

                     #private members
                     private = list(

                       # RM parameters
                       p              = NULL,
                       f              = NULL,
                       Q              = NULL,
                       v              = NULL,
                       EIP            = NULL,
                       maxEIP         = NULL,

                       # Life stages
                       M              = NULL, # mosquito density
                       Y              = NULL, # infected (incubating)
                       Z              = NULL, # infectious
                       ZZ             = NULL, # each row is the number that will be added to the infectious state on that day

                       # Survival & Dispersion
                       psi            = NULL, # rough diffusion matrix
                       P              = NULL,

                       # Pointers
                       TilePointer = NULL, # point to the enclosing metapopulation TILE (MACRO)
                       PatchesPointer = NULL, # point to the enclosing Patches (a network of patches) in this metapopulation TILE (MACRO)
                       HumansPointer = NULL # point to the HumanPop class that also lives in this metapopulation TILE


                     )

) #end class definition
