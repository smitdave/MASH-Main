###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MACRO
#   MosquitoRM Class Implementation
#   MASH Team
#   November 2017
#
###############################################################################

#' Get P
#'
#' Return P, the fraction of a cohort to survive over that many days of the EIP
#'
get_P_MosquitoRM <- function(){
  return(private$P)
}

MosquitoRM$set(which = "public",name = "get_P",
          value = get_P_MosquitoRM, overwrite = TRUE
)

#' Set P
#'
#' Set P, the fraction of a cohort to survive over that many days of the EIP
#'
#' @param P a numeric vector of length equal to EIP
#'
set_P_MosquitoRM <- function(P){
  private$P = P
}

MosquitoRM$set(which = "public",name = "set_P",
          value = set_P_MosquitoRM, overwrite = TRUE
)

# p: daily survival; lifetime is geometric(p)
get_p_MosquitoRM <- function(){
  return(private$p)
}

MosquitoRM$set(which = "public",name = "get_p",
          value = get_p_MosquitoRM, overwrite = TRUE
)

set_p_MosquitoRM <- function(p){
  private$p = p
}

MosquitoRM$set(which = "public",name = "set_p",
          value = set_p_MosquitoRM, overwrite = TRUE
)

# f: feeding rate
get_f_MosquitoRM <- function(){
  return(private$f)
}

MosquitoRM$set(which = "public",name = "get_f",
          value = get_f_MosquitoRM, overwrite = TRUE
)

set_f_MosquitoRM <- function(f){
  private$f = f
}

MosquitoRM$set(which = "public",name = "set_f",
          value = set_f_MosquitoRM, overwrite = TRUE
)

# Q: human blood index
get_Q_MosquitoRM <- function(ix = NULL){
  if(is.null(ix)){
    return(private$Q[ix])
  } else {
   return(private$Q)
  }
}

MosquitoRM$set(which = "public",name = "get_Q",
          value = get_Q_MosquitoRM, overwrite = TRUE
)

set_Q_MosquitoRM <- function(Q, ix = NULL){
  if(is.null(ix)){
    private$Q = Q
  } else {
    private$Q[ix] = Q
  }
}

MosquitoRM$set(which = "public",name = "set_Q",
          value = set_Q_MosquitoRM, overwrite = TRUE
)

# v: daily egg laying rate
get_v_MosquitoRM <- function(){
  return(private$v)
}

MosquitoRM$set(which = "public",name = "get_v",
          value = get_v_MosquitoRM, overwrite = TRUE
)

set_v_MosquitoRM <- function(v){
  private$v = v
}

MosquitoRM$set(which = "public",name = "set_v",
          value = set_v_MosquitoRM, overwrite = TRUE
)

# maxEIP: maximum length of EIP
get_maxEIP_MosquitoRM <- function(){
  return(private$maxEIP)
}

MosquitoRM$set(which = "public",name = "get_maxEIP",
          value = get_maxEIP_MosquitoRM, overwrite = TRUE
)

set_maxEIP_MosquitoRM <- function(maxEIP){
  private$maxEIP = maxEIP
}

MosquitoRM$set(which = "public",name = "set_maxEIP",
          value = set_maxEIP_MosquitoRM, overwrite = TRUE
)

# M: mosquito density
get_M_MosquitoRM <- function(ix = NULL){
  if(!is.null(ix)){
    return(private$M[ix])
  } else {
   return(private$M)
  }
}

MosquitoRM$set(which = "public",name = "get_M",
          value = get_M_MosquitoRM, overwrite = TRUE
)

set_M_MosquitoRM <- function(M, ix = NULL){
  if(!is.null(ix)){
    private$M[ix] = M
  } else {
   private$M = M
  }
}

MosquitoRM$set(which = "public",name = "set_M",
          value = set_M_MosquitoRM, overwrite = TRUE
)

accumulate_M_MosquitoRM <- function(M, ix){
  private$M[ix] = private$M[ix] + M
}

MosquitoRM$set(which = "public",name = "accumulate_M",
          value = accumulate_M_MosquitoRM, overwrite = TRUE
)

# Y: incubating mosquitoes
get_Y_MosquitoRM <- function(ix = NULL){
  if(!is.null(ix)){
    return(private$Y[ix])
  } else {
   return(private$Y)
  }
}

MosquitoRM$set(which = "public",name = "get_Y",
          value = get_Y_MosquitoRM, overwrite = TRUE
)

set_Y_MosquitoRM <- function(Y, ix = NULL){
  if(!is.null(ix)){
    private$Y[ix] = Y
  } else {
   private$Y = Y
  }
}

MosquitoRM$set(which = "public",name = "set_Y",
          value = set_Y_MosquitoRM, overwrite = TRUE
)

# Z: infectious mosquitoes
get_Z_MosquitoRM <- function(ix = NULL){
  if(!is.null(ix)){
    return(private$Z[ix])
  } else {
   return(private$Z)
  }
}

MosquitoRM$set(which = "public",name = "get_Z",
          value = get_Z_MosquitoRM, overwrite = TRUE
)

set_Z_MosquitoRM <- function(Z, ix = NULL){
  if(!is.null(ix)){
    private$Z[ix] = Z
  } else {
   private$Z = Z
  }
}

MosquitoRM$set(which = "public",name = "set_Z",
          value = set_Z_MosquitoRM, overwrite = TRUE
)

# ZZ: mosquito progression through EIP
get_ZZ_MosquitoRM <- function(){
  return(private$ZZ)
}

MosquitoRM$set(which = "public",name = "get_ZZ",
          value = get_ZZ_MosquitoRM, overwrite = TRUE
)


###############################################################################
# Pointers
###############################################################################

# TilePointer
get_TilePointer_MosquitoRM <- function(){
  return(private$TilePointer)
}

MosquitoRM$set(which = "public",name = "get_TilePointer",
          value = get_TilePointer_MosquitoRM, overwrite = TRUE
)

set_TilePointer_MosquitoRM <- function(TilePointer){
  private$TilePointer = TilePointer
}

MosquitoRM$set(which = "public",name = "set_TilePointer",
          value = set_TilePointer_MosquitoRM, overwrite = TRUE
)

# PatchesPointer
get_PatchesPointer_MosquitoRM <- function(){
  return(private$PatchesPointer)
}

MosquitoRM$set(which = "public",name = "get_PatchesPointer",
          value = get_PatchesPointer_MosquitoRM, overwrite = TRUE
)

set_PatchesPointer_MosquitoRM <- function(PatchesPointer){
  private$PatchesPointer = PatchesPointer
}

MosquitoRM$set(which = "public",name = "set_PatchesPointer",
          value = set_PatchesPointer_MosquitoRM, overwrite = TRUE
)

# HumansPointer
get_HumansPointer_MosquitoRM <- function(){
  return(private$HumansPointer)
}

MosquitoRM$set(which = "public",name = "get_HumansPointer",
          value = get_HumansPointer_MosquitoRM, overwrite = TRUE
)

set_HumansPointer_MosquitoRM <- function(HumansPointer){
  private$HumansPointer = HumansPointer
}

MosquitoRM$set(which = "public",name = "set_HumansPointer",
          value = set_HumansPointer_MosquitoRM, overwrite = TRUE
)
