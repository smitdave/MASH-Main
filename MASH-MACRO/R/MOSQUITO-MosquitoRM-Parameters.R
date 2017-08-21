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
#' @param M absolute mosquito density in this patch
#' @param f absolute mosquito blood feeding rate (1/f is average time between bloodmeals)
#' @param Q human blood index, the proportion of bloodmeals taken on humans
#' @param maxEIP the maximum length of the EIP if accounting for time/temperature-dependent EIP
#'
#' @export
MosquitoRM.Parameters <- function(M, f = 0.3, Q = 0.9, p = 0.9, maxEIP = 30){

  # MosquitoRM constructor level fields
  out = list()
  out$M = M

  #  MosquitoRM parameter level fields
  out$par = list()
  out$par$f = f
  out$par$Q = Q
  out$par$p = p

  return(out)
}
