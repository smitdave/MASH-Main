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


#' EL4P Fit a Landscape
#'
#'
#' * this method is bound to code{Landscape$EL4P_fit}
#'
#'
#'
#' @param lambda see \code{\link{EL4P_getLambda}}
#' @param W vector of weights on K (if \code{NULL} weights are sampled from gamma distribution)
#' @param Wa if \code{W} not provided
#' @param Wb
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
EL4P_fit_Landscape <- function(lambda, W = NULL, Wa = 1, Wb = 1, beta = 32, mu_E = 0.168, mu_L = 0.168, mu_P = 0.168, mu_M = 0.123, d_E = 1, d_L = 14, d_P = 1){

  if(!is.null(W)){
    if(length(W)!=private$AquaSitesN){
      stop("length of W must be same as number of aquatic habitats")
    } else {
      W = rgamma(n = private$AquaSitesN,shape = Wa,scale = Wb)
    }
  }

  fitK = EL4P_fitK(lambda,beta,mu_E,mu_L,mu_P,mu_M,d_E,d_L,d_P)
  K = (fitK$K*W)/sum(W)

  E = (fitK$E_eq*W)/sum(W)
  L1 = (fitK$L1_eq*W)/sum(W)
  L2 = (fitK$L2_eq*W)/sum(W)
  L3 = (fitK$L3_eq*W)/sum(W)
  L4 = (fitK$L4_eq*W)/sum(W)
  P = (fitK$P_eq*W)/sum(W)
  M = (fitK$M_eq*W)/sum(W)
  lambdaEq = (fitK$lambda_eq*W)/sum(W)

  for(ix in 1:private$AquaSitesN){

    cat("fitting site: ",ix," of: ",private$AquaSitesN,"\n")

    # set parameters
    private$AquaSites[[ix]]$get_EL4P()$set_K(K[ix])
    private$AquaSites[[ix]]$get_EL4P()$set_mu_E(mu_E)
    private$AquaSites[[ix]]$get_EL4P()$set_mu_L(mu_L)
    private$AquaSites[[ix]]$get_EL4P()$set_mu_P(mu_P)
    private$AquaSites[[ix]]$get_EL4P()$set_d_E(d_E)
    private$AquaSites[[ix]]$get_EL4P()$set_d_L(d_L)
    private$AquaSites[[ix]]$get_EL4P()$set_d_P(d_P)

    # set population values
    private$AquaSites[[ix]]$get_EL4P()$set_E(E[ix])
    private$AquaSites[[ix]]$get_EL4P()$set_L1(L1[ix])
    private$AquaSites[[ix]]$get_EL4P()$set_L2(L2[ix])
    private$AquaSites[[ix]]$get_EL4P()$set_L3(L3[ix])
    private$AquaSites[[ix]]$get_EL4P()$set_L4(L4[ix])
    private$AquaSites[[ix]]$get_EL4P()$set_P(P[ix])
    private$AquaSites[[ix]]$get_EL4P()$set_lambda(lambdaEq[ix])

    # run to equilibrium


  }

}


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
  out$lambda_eq <- (1/2) * (out$P_eq / d_P)
  out$K <- out$L_eq / (out$E_eq/(mu_L * d_E * out$L1_eq) - 4/(mu_L * d_L) - 1)

  return(out)
}


#' EL4P Get Lambda
#'
#' Given equilibrium parameters and desired value of \deqn{R_{0}}
#'
#' @param R0 basic reproductive number (expected number of infected humans produced per infected mosquito when prevalence in humans is 0)
#' @param b mosquito to human transmission efficiency
#' @param c human to mosquito transmission efficiency
#' @param r human recovery rate
#' @param a human feeding rate (fQ; feeding rate multiplied by human blood index)
#' @param g daily mortality rate (1/lifespan)
#' @param EIP length of entomological incubation period
#'
#' @export
EL4P_getLambda <- function(R0, b = 0.55, c = 0.15, r = 1/200, a = 0.3, g = 1/12, EIP = 12){
  return(
    R0 / ((b*c*(a^2)*exp(-g*EIP)) / (r*(g^2)))
  )
}
