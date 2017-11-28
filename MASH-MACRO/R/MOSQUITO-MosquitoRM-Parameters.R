###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MACRO
#   MACRO: MosquitoRM Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   August 21, 2017
#
###############################################################################

#' MosquitoRM: Generate Parameters
#'
#' Generate a named list of parameters for \code{\link{MosquitoRM}}.
#'
#' @param EIP numeric vector describing daily EIP (must be length 365)
#' @param M mosquito density in each patch
#' @param psi movement matrix
#' @param f mosquito blood feeding rate (1/f is average time between bloodmeals)
#' @param Q human blood index, the proportion of bloodmeals taken on humans
#' @param p daily survival probability (1/p lifespan)
#' @param v number of eggs laid per mosquito per egg batch
#' @param maxEIP the maximum length of the EIP if accounting for seasonal EIP
#'
#' @export
MosquitoRM.Parameters <- function(EIP, M, psi, f = 0.3, Q = 0.9, p = 0.9, v = 20, maxEIP = 30){

  #  MosquitoRM parameter level fields
  out = list()
  out$psi = psi
  out$f = f
  out$Q = Q
  out$p = p
  out$v = v

  if(length(EIP)!=365){stop("length of input EIP must be 365")}
  if(max(EIP)>maxEIP){maxEIP = max(EIP)+5L}
  out$EIP = EIP
  out$maxEIP = maxEIP

  out$M = M

  return(out)
}
