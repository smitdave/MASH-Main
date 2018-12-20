################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Common utility functions (useful regardless of model)
#
#   Sean Wu
#   December 2018
#
################################################################################


#' Utility: Helper function to match argument calls
#'
#' from: \url{https://stackoverflow.com/a/43329945/4651968}
#'
match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )


  match.call(sys.function(sys.parent()), call)
}


#' Utility: Make a Parameter Object for C++ Initialization
#'
#' Take a parameter and transform into a package (named list) so it can
#' easily be passed to the parameters object in C++ (see inst/include/Parameters.hpp)
#'
#' @param val numeric or integer value
#' @param name name of the parameter
#' @param fixtype if \code{NULL} the function tries to figure out if you want this parameter to be an \code{int} or a \code{double} in C++, otherwise use the provided input
#'
#' @export
pars_obj <- function(val,name,fixtype = NULL){

  type <- NULL

  if(is.null(fixtype)){

    if(val %% 1 == 0){
      type <- 0L # integer flag
    } else {
      type <- 1L # double flag
    }

  } else {

    type <- fixtype

  }

  list(
    type = as.integer(type),
    name = as.character(name),
    val = ifelse(type,as.numeric(val),as.integer(val))
  )
}
