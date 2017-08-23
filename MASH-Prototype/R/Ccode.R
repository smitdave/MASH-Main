

# #' Test of eapply with no output
# #' @useDynLib MASHprototype R_eapply_noOut
# #' @export
# eapply_noOut <- function(envir){
#   .Call(R_eapply_noOut, envir)
# }

#' ELP ODE model
#'
#' @export
#'
ELPode <- function(time, parms, ...) {
  ## assign parameters and solve differential equations
  #cat("compiled code running\n")
  y0    <- parms[c("L", "M")]
  parms <- parms[c("f", "v", "alpha","gamma","psi","g")]
  #cat(y0, "\n")
  #cat(parms, "\n")
  out <- deSolve::ode(y = y0, times = time, func = "ode_ELP", parms = parms,
             dllname = "MASHprototype",
             initfunc = "init_ELP", nout = 2, outnames=c("L", "M"), ...)

}
