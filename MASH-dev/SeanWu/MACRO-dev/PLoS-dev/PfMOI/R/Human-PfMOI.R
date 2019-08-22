################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Human-PfMOI initialization parameters for constructor
#
#   Sean Wu (slwu89@berkeley.edu)
#   August 2019
#
################################################################################


################################################################################
#   Human constructor
################################################################################

#' Human-PfMOI: Individual Constructor Parameters
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
#' @param moi initial multiplicity of infection (integer)
#' @param chx initial chemoprotection (boolean)
#' @param bite_algorithm 0 for poisson, 1 for negative binomial
#' @param bite_disp overdispersion parameter for negative binomial biting (NULL for Poisson)
#'
#' @export
human_pfmoi_conpars <- function(id,home_patch_id,trip_duration,trip_frequency,bweight,age,state,bite_algorithm,bite_disp=NaN){
  if(bite_algorithm==1 && (is.null(bite_disp) || is.nan(bite_disp) || is.na(bite_disp))){
    stop("if using negative binomial biting, please provide a valid numeric value for 'bite_disp'")
  }
  if(!(bite_algorithm %in% c(0,1))){
    stop("please use 0 for Poisson distributed biting and 1 for Negative Binomial distributed biting")
  }
  if(moi < 0){
    stop("moi must not be less than 0")
  }
  if(moi > 0 & chx){
    stop("cannot begin with moi > 1 (infections) and chemoprotection")
  }
  list(
    id = as.integer(id),
    home_patch_id = as.integer(home_patch_id),
    trip_duration = as.numeric(trip_duration),
    trip_frequency = as.numeric(trip_frequency),
    bweight = as.numeric(bweight),
    age = as.numeric(age),
    moi = as.integer(moi),
    chx = as.logical(chx),
    bite_algorithm = as.integer(bite_algorithm),
    bite_disp = as.numeric(bite_disp)
  )
}

# #' Human-PfMOI: Check Constructor Parameters for a Population
# #'
# #' Given a list where each element is output from \code{\link{human_pfsi_conpars}},
# #' double check to make sure all elements are valid constructor parameters for that human.
# #'
# #' @param par a list
# #'
# #' @export
# check_human_pfmoi_conpars <- function(par){
#
#   # check one human
#   check_one <- function(x){
#     with(x,{
#       if(!is.integer(id) | !is.integer(home_patch_id)){
#         stop(paste0("human: ",id," must have integer valued 'id' 'home_patch_id'\n"))
#       }
#       if(((id %% 1) != 0) | ((home_patch_id %% 1) != 0)){
#         stop(paste0("human: ",id," must have integer valued 'id' 'home_patch_id'\n"))
#       }
#       if(any(c(trip_duration,trip_frequency,bweight,age) < 0) | any(!is.numeric(c(trip_duration,trip_frequency,bweight,age)))){
#         stop(paste0("human: ",id," 'trip_duration', 'trip_frequency', 'bweight', 'age' must all be positive floating point values\n"))
#       }
#     })
#   }
#
#   invisible(sapply(par,check_one))
# }


################################################################################
#   PfMOI vaccination events
################################################################################

#' Human-PfMOI: Make parameters for a vaccination event
#'
#' Make a parameter list for a PE (sporozoite-blocking) vaccination event or GS (gametocyte-killing) vaccination event.
#' The entire parameters list for the population will be a list of size equal
#' to the number of people to recieve vaccination, each element of which is the output
#' of this function.
#'
#' @param id an integer id of a human to recieve vaccination
#' @param t a numeric vector giving times of vaccination events
#' @param treat a logical vector indicating if treatment is to accompany vaccinations
#' @param type "PE" (sporozoite-blocking) or "GS" (gametocyte-killing)
#'
#' @export
vaccination_pfmoi_conpars <- function(id,t,treat,type){

  if(!is.numeric(t) | t < 0){
    stop("time of pevaxx event must be a vector of positive floats")
  }

  if(!is.logical(treat)){
    stop("treatment must be a logical vector")
  }

  if(id < 0){
    stop("id must be a positive integer value")
  }

  if(!(type %in% c("PE","GS"))){
    stop("vaccine 'type' must be PE (sporozoite-blocking) or GS (gametocyte-killing)")
  }

  list(
    id = as.integer(id),
    tEvent = as.numeric(t),
    treat = as.logical(treat),
    type = as.character(type)
  )
}
