###############################################################################
#       __  __           __    __  ___
#      / / / /___ ______/ /_  /  |/  /___ _____
#     / /_/ / __ `/ ___/ __ \/ /|_/ / __ `/ __ \
#    / __  / /_/ (__  ) / / / /  / / /_/ / /_/ /
#   /_/ /_/\__,_#____#_/ /_#_/  /_/\__,_/ .___/
#                                      /_/
#
#   MASH-CPP
#   HashMap C Helper Functions
#   Sean Wu
#   August 18, 2017
#
###############################################################################

#' MASH-CPP: eapply with Invisisble Return
#'
#' write me
#'
#' @param envir an environment to iterate over
#' @useDynLib MASHcpp R_eapplyInvisible
#' @export
eapplyInvisible <- function(envir){
  .Call(R_eapplyInvisible, envir)
}


# eapplyTest <- function(envir ,FUN,...){
#   .External(R_eapplyTest, FUN, envir, ...)
# }
#
# dotsapply <- function( fun, ... )
#    .External( dotsapplyR, fun, environment(), ... )
