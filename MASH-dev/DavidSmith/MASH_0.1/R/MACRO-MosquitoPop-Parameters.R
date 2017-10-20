#################################################################
#
#   MASH
#   R6-ified
#   MACRO MosquitoPop Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 26, 2017
#
#################################################################


#' Initialize MosquitoPop Parameters for \code{MacroMosquitoPop}
#'
#' make a list of pars for \code{\link{MacroMosquitoPop}}
#'
#' @param N number of patches
#' @param EIP entomological incubation period
#' @param M_density either a vector of mosquito densities equal to \code{N} or a single value for all patches
#' @param p daily survival probability
#' @param f feeding rate (average number of daily blood meals per capita)
#' @param Q human blood index (propensity to feed on humans)
#' @param v daily egg laying rate
#' @param maxEIP maximum length (in days) to track mosquito progression through EIP
#' @param psi movement matrix for rough diffusion mosquito movement, if \code{NULL} mosquitoes do not move from patch to patch
#' @return return a list \code{MacroMosquitoPop_PAR}
#' @examples
#' MACRO.MosquitoPop.Parameters(N = 5)
#' @export
MACRO.MosquitoPop.Parameters <- function(N, EIP = 10,M_density=10, p=0.9, f=0.3, Q=0.9, v=20, maxEIP=30, psi=NULL){

  PAR = list(
    p=p,
    f=f,
    Q=rep(Q,N),
    v=v,
    EIP = EIP,
    maxEIP=maxEIP,
    psi=psi
  )
  if(length(M_density)==1){
    PAR$M_density = rep(M_density,N)
  } else {
    if(length(M_density)!=N){
      stop("length of vector M_density must be equal to number of patches")
    } else {
      PAR$M_density = M_density
    }
  }

  return(PAR)
}
