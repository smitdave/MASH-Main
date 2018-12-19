################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Mosquito-RM initialization parameters for constructor
#
#   Sean Wu
#   December 2018
#
################################################################################

#' Mosquito-RM: Constructor Parameters
#'
#' Generates a single named list with parameters to construct a
#' 'mosquito_rm' object (see inst/include/Mosquito-RM.hpp for constructor parameters).
#' Please note that 'N' (number of patches) may not be the same as the total number of patches
#' constructed in the tile object, due to the existence of reservoir patches. However the
#' first 1,...,N patches should be non-reservoir patches (simulated explicitly).
#'
#' @param N integer number of patches
#' @param lambda numeric matrix (365 x N) describing emergence rate by day of year and patch
#' @param psi diffusion (movement) matrix (N x N); rows must sum to 1
#' @param EIP integer vector (N) describing length of EIP by day of year
#' @param p daily survival probability
#' @param f blood feeding rate
#' @param Q human blood index
#' @param v daily egg production
#' @param M initial conditions of total female mosquito populations (N patches)
#' @param Y initial conditions of incubating female mosquito populations (N patches)
#' @param Z initial conditions of infectious female mosquito populations (N patches)
#'
#' @export
human_pfsi_conpars <- function(
  N, lambda, psi,
  EIP,
  p, f, Q, v,
  M, Y, Z)
{

  # check pars
  if(any(!dim(lambda) == c(365,N))){
    stop(paste0("dimension of emergence matrix 'lambda' must be 365 x N\n"))
  }

  if(any(dim(psi) != N)){
    stop(paste0("dimension of diffusion matrix 'psi' must be same as number of patches (N)\n"))
  }

  if(any(rowSums(psi) != 1)){
    stop("diffusion matrix 'psi' is not a row-normalized stochastic matrix\n")
  }

  if(length(EIP) != 365){
    stop("length of EIP vector must be 365\n")
  }

  if((length(M) != N) | (length(Y) != N) | (length(Z) != N)){
    stop("lengths of 'M', 'Y', 'Z' vectors must be N")
  }

  list(
    model = "RM",
    N = as.integer(N),
    lambda = as.matrix(lambda),
    psi = as.matrix(psi),
    EIP = as.integer(EIP),
    maxEIP = as.integer(max(EIP) + 1),
    p = as.numeric(p),
    f = as.numeric(f),
    Q = as.numeric(Q),
    v = as.numeric(v),
    M = as.numeric(M),
    Y = as.numeric(Y),
    Z = as.numeric(Z)
  )
}
