.onAttach <- function(libname, pkgname) {
  packageStartupMessage("M-BITES: Mosquito flight Bout-based and Individual-based Transmission Ecology Simulator")
}

# assign data here to user global environment
.onLoad <- function(libname, pkgname) {
  message("DEBUGGING ON: see mbites.log for logged messages!")
  futile.logger::flog.threshold(futile.logger::TRACE) # FOR DEBUGGING
  futile.logger::flog.appender(futile.logger::appender.tee('mbites.log'))
  futile.logger::flog.trace("MBITES logging being initiated")
}
