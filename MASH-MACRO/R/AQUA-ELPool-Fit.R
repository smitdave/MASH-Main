###############################################################################
#
#       ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MACRO
#   AQUATIC ECOLOGY: ELPool Fitting
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 22, 2017
#
###############################################################################

#' Aquatic Ecology: ELPool Calculate Psi from K
#'
#' Calculate strength of density dependence parameter psi (increase in per-capita mortality in response to crowding) from
#' adult and pool characteristics. Functional form of equation given by \deqn{K=\sqrt[\sigma]{\frac{\frac{fv \alpha }{g}-(\alpha + \gamma)}{\psi }}}
#' For more details and references see \code{\link[MASHcpp]{ELPool}}.
#'
#'
#' @param f adult female mosquito blood feeding rate (waiting time between bloodmeals is 1/f)
#' @param v size of egg batch per bloodmeal
#' @param alpha daily mautration rate of aquatic stages (1/alpha is expected duration of aquatic life stages)
#' @param g daily adult female mortality rate (average lifespan is 1/g)
#' @param gamma daily density independent mortality rate
#' @param K carrying capacity
#'
#' @return numeric
#' @export
K2psi_ELPool <- function(f,v,alpha,g,gamma,K){
  return(
    (((f*v*alpha)/g) - (alpha+gamma)) / (K^2)
  )
}
