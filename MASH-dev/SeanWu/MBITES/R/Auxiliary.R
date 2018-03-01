




##############################################################
# Sample
##############################################################

#' Sample Indices
#'
#' Wrapper for \code{\link{sample}} that will not lead to unexpected behavior when \code{x} is of length 1.
#'  * This function is used in \code{\link{mbites_chooseHost}}
#'
#' @export
sample <- function(x, ...){
  return(
    x[sample.int(length(x), ...)]
  )
}
