###############################################################################
#
#      ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MICRO
#   AQUATIC ECOLOGY: EL4P Fitting
#   MASH-MICRO Team
#   October 2017
#
###############################################################################

#' EL4P Fit K
#'
#' Given equilibrium parameters and emergence lambda, fit carrying capacity K.
#'
#' @param beta daily rate of oviposition of eggs (units in 1/day * 1/female mosquito)
#' @param mu_E egg stage daily mortality rate
#' @param mu_L larvae stage daily mortality rate
#' @param mu_P pupae stage daily mortality rate
#' @param mu_M adult female daily mortality rate
#' @param d_E mean duration of egg stage (days)
#' @param d_L mean duration of larvae stage (days)
#' @param d_P mean duration of pupae stage (days)
#'
#' @export
EL4P_fitK <- function(lambda, beta = 32, mu_E = 0.168, mu_L = 0.168, mu_P = 0.168, mu_M = 0.123, d_E = 1, d_L = 14, d_P = 1){

  out = list()

  out$P_eq <- lambda / (2 * d_P)
  out$M_eq <- out$P_eq / (2 * d_P * mu_M)
  out$E_eq <- beta * out$M_eq / (mu_E + (1/d_E))
  out$L1_eq <- (beta^(3/4) * d_L * out$M_eq * (1 + d_P * mu_P)^(1/4) * mu_M^(1/4)) /
    (2^(7/4) * (1 + d_E * mu_E)^(3/4))
  out$L2_eq <- (beta^(2/4) * d_L * out$M_eq * (1 + d_P * mu_P)^(2/4) * mu_M^(2/4)) /
    (2^(6/4) * (1 + d_E * mu_E)^(2/4))
  out$L3_eq <- (beta^(1/4) * d_L * out$M_eq * (1 + d_P * mu_P)^(3/4) * mu_M^(3/4)) /
    (2^(5/4) * (1 + d_E * mu_E)^(1/4))
  out$L4_eq <- (d_L * out$M_eq * (1 + d_P * mu_P) * mu_M) / 2
  out$L_eq <- out$L1_eq + out$L2_eq + out$L3_eq + out$L4_eq
  out$K <- out$L_eq / (out$E_eq/(mu_L * d_E * out$L1_eq) - 4/(mu_L * d_L) - 1)

  return(out)
}
