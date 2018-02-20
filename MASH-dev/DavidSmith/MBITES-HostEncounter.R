###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-BRO: Host Encounter
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################

#################################################################
#   Human Host Encounter
#################################################################

#' MBITES-BRO: Human Host Encounter for \code{\link{MosquitoFemale}}
#'
#' After calling \code{\link{mbites_chooseHost}}, the mosquito encounters the human host and attempts to feed.
#'  * This method is bound to \code{MosquitoFemale$humanEncounter()}.
#' @md
mbites_humanEncounter <- function(){
  if(runif(1) < 1-private$FemalePopPointer$get_MBITES_PAR("surviveH")){ # does not survive to probe
    private$state = 'D'
  } else { # survives to probe

    if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("probeH")){ # undeterred
      self$probing() # PATHOGEN-XX-Methods.R (infect host?) 
      if(runif(1) < 1-private$FemalePopPointer$get_MBITES_PAR("surviveprobeH")){ # does not survive probing
        private$state = 'D'
      } else { # survives probing

        if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("feedH")){ # successfully begins feeding
          self$BloodMeal() # MBITES-Energetics.R
          self$feeding() # PATHOGEN-XX-Methods.R
          private$history$historyFeed(privateEnv = private) # MOSQUITO-History.hpp
        }

      }

    }

  }

}


#################################################################
#   Non-human Host Encounter
#################################################################

#' MBITES: Non-Human Host Encounter for \code{\link{MosquitoFemale}}
#'
#' After calling \code{\link{mbites_chooseHost}}, the mosquito encounters the non-human host and attempts to feed.
#'  * This method is bound to \code{MosquitoFemale$zooEncounter()}.
#' @md
mbites_zooEncounter <- function(){

  if(runif(1) < 1-private$FemalePopPointer$get_MBITES_PAR("surviveZ")){ # does not survive to feed
    private$state = 'D'
  } else { # survives to feed

    if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("feedZ")){ # successfully begins feeding
      self$BloodMeal() # MBITES-Energetics.R
      private$history$historyFeed(privateEnv = private) # MOSQUITO-History.hpp
    }
  }
}


#################################################################
#   Probing & Infection
#################################################################

# Due to the different design of the R6/C++ OOP version of MASH-MICRO, all
# host probing and infection functions are moved to their respective PATHOGEN
# method files PATHOGEN-XX-Methods.R

# MosquitoFemale$ProbeHost() will be defined generically based on module chosen.
