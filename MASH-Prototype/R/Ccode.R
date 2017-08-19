

#' Test of eapply with no output
#' @export
eapply_noOut <- function(envir){
  .Call(R_eapply_noOut, envir)
}
