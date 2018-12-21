################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Human-PfSI initialization parameters for constructor
#
#   Sean Wu
#   December 2018
#
################################################################################


################################################################################
#   Human constructor
################################################################################

#' Human-PfSI: Individual Constructor Parameters
#'
#' Generates a single named list with parameters to construct a single
#' 'human_pfsi' object (see inst/include/Human-PfSI.hpp for constructor parameters).
#' Be aware that all 'id' related fields should be considered 0-indexed (especially the home patch).
#'
#' @param id integer id of this person
#' @param home_patch_id the id of their home patch
#' @param trip_duration numeric value; average durarion of trips away from home
#' @param trip_frequency numeric value; average rate at which they make trips away from home
#' @param bweight numeric value; biting weight to this person (relative to others in a patch; should have mean 1)
#' @param age numeric age (not currently used)
#' @param inf logical (start simulation infected or not)
#' @param chx logical (start simulation with chemoprophylactic protection or not)
#'
#' @export
human_pfsi_conpars <- function(id,home_patch_id,trip_duration,trip_frequency,bweight,age,inf,chx){

  # check pars
  if(all(inf,chx)){
    stop(paste0("human: ",id," cannot both have active infection and be under chemoprophylactic protection\n"))
  }

  if(((id %% 1) != 0) | ((home_patch_id %% 1) != 0)){
    stop(paste0("human: ",id," must have integer valued 'id' 'home_patch_id'\n"))
  }

  list(
    model = "PfSI",
    id = as.integer(id),
    home_patch_id = as.integer(home_patch_id),
    trip_duration = as.numeric(trip_duration),
    trip_frequency = as.numeric(trip_frequency),
    bweight = as.numeric(bweight),
    age = as.numeric(age),
    inf = as.logical(inf),
    chx = as.logical(chx)
  )
}

#' Human-PfSI: Check Constructor Parameters for a Population
#'
#' Given a list where each element is output from \code{\link{human_pfsi_conpars}},
#' double check to make sure all elements are valid constructor parameters for that human.
#'
#' @param par a list
#'
#' @export
check_human_pfsi_conpars <- function(par){
  
  # check one human
  check_one <- function(x){
    with(x,{
      if(!is.integer(id) | !is.integer(home_patch_id)){
        stop(paste0("human: ",id," must have integer valued 'id' 'home_patch_id'\n"))
      }
      if(((id %% 1) != 0) | ((home_patch_id %% 1) != 0)){
        stop(paste0("human: ",id," must have integer valued 'id' 'home_patch_id'\n"))
      }
      if(!is.logical(inf) | !is.logical(chx)){
        stop(paste0("human: ",id," must have boolean valued 'inf' and 'chx' parameters\n"))
      }
      if(inf & chx){
        stop(paste0("human: ",id," cannot both have active infection and be under chemoprophylactic protection\n"))
      }
      if(any(c(trip_duration,trip_frequency,bweight,age) < 0) | any(!is.numeric(c(trip_duration,trip_frequency,bweight,age)))){
        stop(paste0("human: ",id," 'trip_duration', 'trip_frequency', 'bweight', 'age' must all be positive floating point values\n"))
      }
    })
  }

  invisible(sapply(par,check_one))
}


################################################################################
#   PfSI vaccination events
################################################################################

#' Human-PfSI: Make parameters for a vaccination event
#'
#' Make a parameter list for a PE (sporozoite-blocking) vaccination event or GS (gametocyte-killing) vaccination event.
#' The entire parameters list for the population will be a list of size equal
#' to the number of people to recieve vaccination, each element of which is the output
#' of this function.
#'
#' @param id an integer id of a human to recieve vaccination
#' @param t a numeric vector giving times of vaccination events
#' @param treat a logical vector indicating if treatment is to accompany vaccinations
#'
#' @export
pevaxx_pfsi_conpars <- function(id,t,treat,type){

  if(!is.numeric(t) | any(t < 0)){
    stop("time of pevaxx event must be a vector of positive floats")
  }

  if(!is.logical(treat)){
    stop("treatment must be a logical vector")
  }

  if(!is.integer(id) | (id < 0)){
    stop("id must be a positive integer value")
  }

  if(length(t) != length(treat)){
    stop("length of 't' (time of vaxx event vector) must be same as 'treat' (treatment to accompany vaxx vector)")
  }

  if(!(type %in% c("PE","GS"))){
    stop("vaccine 'type' must be PE (sporozoite-blocking) or GS (gametocyte-killing)")
  }

  list(
    id = as.integer(id),
    n = length(t),
    tEvent = as.numeric(t),
    treat = as.logical(treat),
    type = as.character(type)
  )
}
