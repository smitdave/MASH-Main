#' Sample Indices
#'
#' Wrapper for \code{\link{sample}} that will not lead to unexpected behavior when \code{x} is of length 1.
#' @export
sample_aux <- function(x, ...){
  return(
    x[sample.int(length(x), ...)]
  )
}
