# .onLoad <- function(libname, pkgname) {
#   message("trying to source .c file")
#   system("R CMD SHLIB ./src/deSolveModel.c")
#   dyn.load("./src/deSolveModel.so")
# }
