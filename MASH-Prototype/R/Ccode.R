

#' Test of eapply with no output
#' @useDynLib MASHprototype R_eapply_noOut
#' @export
eapply_noOut <- function(envir){
  .Call(R_eapply_noOut, envir)
}
