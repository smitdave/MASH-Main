#################################################################
#
#   MASH
#   R6-ified
#   MACRO MosquitoPop Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   May 22, 2016
#
#################################################################

#' MACRO Mosquito Population Class Definition
#'
#' This is a generic MACRO mosquito population blah blah ...
#'  below i describe the basic structure of the patch. methods and fields for specific COMPONENTS can be found in:
#' * somewhere 1
#' * somewhere 2
#' @md
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' talk about me in detail!
#' @section Methods:
#'  * Constructor:
#'  * another method
#' @section Fields:
#'  * p:
#'  * f:
#'  * Q:
#'  * v:
#'  * EIP:
#'  * maxEIP:
#'  * M:
#'  * Y:
#'  * Z:
#'  * ZZ:
#'  * psi:
#'  * P:
#'  * TilePointer:
#'  * PatchesPointer:
#'  * HumansPointer:
#' @md
#' @export
MacroMosquitoPop <- R6::R6Class(classname = "MacroMosquitoPop",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   # N: number of patches
                   # M_density: mosquito density at each patch
                   # MacroMosquitoPop_PAR: a list of RM parameters
                   initialize = function(N, MacroMosquitoPop_PAR){

                     private$p = MacroMosquitoPop_PAR$p
                     private$f = MacroMosquitoPop_PAR$f
                     private$Q = MacroMosquitoPop_PAR$Q
                     private$v = MacroMosquitoPop_PAR$v
                     private$EIP = MacroMosquitoPop_PAR$EIP
                     private$maxEIP = MacroMosquitoPop_PAR$maxEIP

                     private$M   = MacroMosquitoPop_PAR$M_density
                     private$Y   = rep(0L, N) # infected (incubating)
                     private$Z   = rep(0L, N) # infectious
                     private$ZZ  = matrix(data=0L,nrow=MacroMosquitoPop_PAR$maxEIP,ncol=N) # each row is the number that will be added to the infectious state on that day

                     private$psi = MacroMosquitoPop_PAR$psi
                     private$P   = MacroMosquitoPop_PAR$p^c(1:MacroMosquitoPop_PAR$maxEIP) # survival over EIP

                   },

                   #################################################
                   # Getters and Setters
                   #################################################

                   # p: daily survival; lifetime is geometric(p)
                   get_p = function(){
                     return(private$p)
                   },
                   set_p = function(p){
                     private$p = p
                   },

                   # f: feeding rate
                   get_f = function(){
                     return(private$f)
                   },
                   set_f = function(f){
                     private$f = f
                   },

                   # Q: human blood index
                   get_Q = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$Q[ix])
                     } else {
                      return(private$Q)
                     }
                   },
                   set_Q = function(Q, ix = NULL){
                     if(is.null(ix)){
                       private$Q = Q
                     } else {
                       private$Q[ix] = Q
                     }
                   },

                   # v: daily egg laying rate
                   get_v = function(){
                     return(private$v)
                   },
                   set_v = function(v){
                     private$v = v
                   },

                   # maxEIP: maximum length of EIP
                   get_maxEIP = function(){
                     return(private$maxEIP)
                   },
                   set_maxEIP = function(maxEIP){
                     private$maxEIP = maxEIP
                   },

                   # M: mosquito density
                   get_M = function(ix = NULL){
                     if(!is.null(ix)){
                       return(private$M[ix])
                     } else {
                      return(private$M)
                     }
                   },
                   set_M = function(M, ix = NULL){
                     if(!is.null(ix)){
                       private$M[ix] = M
                     } else {
                      private$M = M
                     }
                   },

                   # Y: incubating mosquitoes
                   get_Y = function(ix = NULL){
                     if(!is.null(ix)){
                       return(private$Y[ix])
                     } else {
                      return(private$Y)
                     }
                   },
                   set_Y = function(Y, ix = NULL){
                     if(!is.null(ix)){
                       private$Y[ix] = Y
                     } else {
                      private$Y = Y
                     }
                   },

                   # Z: infectious mosquitoes
                   get_Z = function(ix = NULL){
                     if(!is.null(ix)){
                       return(private$Z[ix])
                     } else {
                      return(private$Z)
                     }
                   },
                   set_Z = function(Z, ix = NULL){
                     if(!is.null(ix)){
                       private$Z[ix] = Z
                     } else {
                      private$Z = Z
                     }
                   },

                   # ZZ: mosquito progression through EIP
                   get_ZZ = function(){
                     return(private$ZZ)
                   },
                   set_ZZ = function(ZZ){
                     private$ZZ = ZZ
                   },

                   # P: fraction of cohort to survive over EIP
                   get_P = function(){
                     return(private$P)
                   },
                   set_P = function(P){
                     private$P = P
                   },

                   # generic accessors
                   get_private = function(){
                     return(as.list(private))
                   },

                   #################################################
                   # Pointers
                   #################################################

                   # TilePointer
                   get_TilePointer = function(){
                     return(private$TilePointer)
                   },
                   set_TilePointer = function(TilePointer){
                     private$TilePointer = TilePointer
                   },

                   # PatchesPointer
                   get_PatchesPointer = function(){
                     return(private$PatchesPointer)
                   },
                   set_PatchesPointer = function(PatchesPointer){
                     private$PatchesPointer = PatchesPointer
                   },

                   # HumansPointer
                   get_HumansPointer = function(){
                     return(private$HumansPointer)
                   },
                   set_HumansPointer = function(HumansPointer){
                     private$HumansPointer = HumansPointer
                   }

                  ),

                  # private methods & fields
                  private = list(

                    # RM parameters
                    p = NULL,
                    f = NULL,
                    Q = NULL,
                    v = NULL,
                    EIP = NULL,
                    maxEIP = NULL,

                    # Life stages
                    M   = NULL, # mosquito density
                    Y   = NULL, # infected (incubating)
                    Z   = NULL, # infectious
                    ZZ  = NULL, # each row is the number that will be added to the infectious state on that day

                    # Survival & Dispersion
                    psi = NULL, # rough diffusion matrix
                    P   = NULL,

                    # Pointers
                    TilePointer = NULL, # point to the enclosing metapopulation TILE (MACRO)
                    PatchesPointer = NULL, # point to the enclosing Patches (a network of patches) in this metapopulation TILE (MACRO)
                    HumansPointer = NULL # point to the HumanPop class that also lives in this metapopulation TILE

                  )

)
