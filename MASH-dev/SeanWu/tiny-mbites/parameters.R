# parameters
make_parameters <- function(
  # timing
  time_BFSB = 1/(6/24),
  time_BFAB = 1/(3/24),
  time_ELSB = 1/(6/24),
  time_ELAB = 1/(3/24),
  time_ppr = 1/(18/24),
){

  out <- list()

  out$time_BFSB <- time_BFSB
  out$time_BFAB <- time_BFAB
  out$time_ELSB <- time_ELSB
  out$time_ELAB <- time_ELAB
  out$time_ppr <- time_ppr


  list2env(out,hash=TRUE)
}
