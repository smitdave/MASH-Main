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


# p: daily survival; lifetime is geometric(p)
get_p_MosquitoRM <- function(){
  return(private$p)
}

set_p_MosquitoRM <- function(p){
  private$p = p
}

# f: feeding rate
get_f_MosquitoRM <- function(){
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
