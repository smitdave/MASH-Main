#################################################################
# Queue Management
#################################################################

#' Extend \code{MacroPatch} ImagoQ (Emerging Adults Queue)
#'
#' Extend the ImagoQ for patch \code{ixP}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
extend_PatchesImagoQ <- function(ixP, N=10L){
  private$PatchesImagoQ[[ixP]] = c(private$PatchesImagoQ[[ixP]],allocImagoQ(N))
}

#' Manage \code{MacroPatch} ImagoQ (Emerging Adults Queue)
#'
#' If all the ImagoQ slots for patch \code{ixP} are full then call \code{\link{extend_PatchesImagoQ}}
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
manage_PatchesImagoQ <- function(ixP, N=10L){
  if(all(vapply(X = private$PatchesImagoQ[[ixP]],FUN = function(x){x$N != 0},FUN.VALUE = logical(1)))){
    self$extend_PatchesImagoQ(ixP=ixP,N=N)
  }
}
