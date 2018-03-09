.onAttach <- function(libname, pkgname) {
  packageStartupMessage("M-BITES: Mosquito flight Bout-based and Individual-based Transmission Ecology Simulator")
}

# assign data here to user global environment
.onLoad <- function(libname, pkgname) {
  message("DEBUGGING ON: see mbites.log for logged messages!")

  # erase old logger file
  dir = system.file(package="MBITES")
  cat("MBITES dir: ",dir,"\n")
  file = file.path(dir,"mbites.log")
  cat("MBITES log file: ",file,"\n")
  if(file.exists(file)){
    cat("removing previous log file: ",file,"\n")
    file.remove(file)
  }

  futile.logger::flog.threshold(futile.logger::TRACE) # FOR DEBUGGING
  futile.logger::flog.appender(futile.logger::appender.tee('mbites.log'))
  futile.logger::flog.trace("MBITES logging being initiated")

  rm(dir)
  rm(file)
}

.onUnload <- function(libpath) {
  futile.logger::flog.trace("MBITES exiting")
}
