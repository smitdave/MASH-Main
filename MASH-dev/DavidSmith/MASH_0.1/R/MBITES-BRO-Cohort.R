#################################################################
#
#   MASH
#   R6-ified
#   MBITES-BRO (Blood Feeding, Resting, Oviposition)
#   Simplified Cohort Simulation
#   David Smith, Hector Sanchez, Sean Wu
#   August 9, 2017
#
#################################################################

#################################################################
# MBITES-BRO-Cohort: Host Encounter
#################################################################

#################################################################
#   Human Host Encounter
#################################################################

#' MBITES-BRO-Cohort: Human Host Encounter for \code{\link{MicroMosquitoFemale}}
#'
#' After calling \code{\link{mbitesGeneric_chooseHost}}, the mosquito encounters a human host and attempts to feed.
#'  * This method is bound to \code{MicroMosquitoFemale$humanEncounter()}.
#' @md
mbitesBRO_cohort_humanEncounter <- function(){
  if(runif(1) < 1-private$FemalePopPointer$get_MBITES_PAR("surviveH")){ # does not survive to probe
    private$stateNew = "D"
  } else { # survives to probe

    if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("probeH")){ # undeterred
      if(runif(1) < 1-private$FemalePopPointer$get_MBITES_PAR("surviveprobeH")){ # does not survive probing
        private$stateNew = "D"
      } else { # survives probing

        if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("feedH")){ # successfully begins feeding
          self$BloodMeal() # MBITES-BRO-Energetics.R
          private$history$historyFeed(privateEnv = private) # MOSQUITO-History.hpp
          private$stateNew = "R"
        }

      }

    }

  }

}


#################################################################
#   Simulation
#################################################################

#' MBITES-BRO-Cohort: Run Simulation for \code{\link{MicroMosquitoFemale}}
#'
#' Run the M-BITES life cycle simulation algorithm until mosquito is dead.
#' This method calls \code{\link{mbitesBRO_oneBout}} to simulate each life stage.
#' For differences between cohort and microsimulation versions see \code{\link{MBITES.BRO.Cohort.Setup}}
#'  * This method is bound to \code{MicroMosquitoFemale$MBITES_Cohort()}.
#'
#' @md
mbitesBRO_cohort_oneMosquito_MBITES <- function(){

  # run algorithm until dead
  while(private$stateNew != "D"){
    self$oneBout()
  }

  history = self$get_history()
  return(history)
}


#################################################################
# MBITES-BRO-Cohort: Run Cohort Simulation
#################################################################

#' MBITES-BRO-Cohort: Human Host Encounter for \code{\link{MicroMosquitoFemale}}
#'
#' After calling \code{\link{mbitesGeneric_chooseHost}}, the mosquito encounters a human host and attempts to feed.
#'  * This method is bound to \code{MicroMosquitoPopFemale$simCohort()}.
#' @param N number of mosquitoes in cohort
#' @param writeJSON if \code{TRUE} write output to JSON in the directory initialized in the enclosing \code{\link{MicroTile}}, else return a list
#' @md
mbitesBRO_cohort_simCohort <- function(N, writeJSON){

  # allocate space for cohort
  private$pop = vector(mode="list",length=N)

  # randomly allocate to initial positions
  aquaN = private$LandscapePointer$AquaSitesN
  aquaIx = sample(x = aquaN,size = N,replace = TRUE)

  # assign cohort
  for(i in 1:N){
    private$pop[[i]] = MicroMosquitoFemale$new(id = as.character(i), time = 0, ix = aquaIx[i], genotype = 0L, state = private$initState)
    private$pop[[i]]$set_FemalePopPointer(self)
    private$pop[[i]]$set_TilePointer(private$TilePointer)
    private$pop[[i]]$set_LandscapePointer(private$LandscapePointer)

    # unneeded pointers
    # private$pop[[ix]]$set_MalePopPointer(private$MalePopPointer)
    # private$pop[[ix]]$set_HumansPointer(private$HumansPointer)
  }

  # do the sim
  nCores = parallel::detectCores()-2L
  cohortOut = parallel::mclapply(X = private$pop,FUN = function(x){x$MBITES_Cohort()},mc.cores = nCores)
  names(cohortOut) = 1:N

  # write out to JSON directory.
  if(writeJSON){

    # write JSON
    con = file(description = paste0(private$TilePointer$get_directory(),"MOSQUITO/","cohortBRO.json"),open = "wt")
    writeLines(text = jsonlite::toJSON(x = cohortOut,pretty = TRUE),con = con)
    close(con)

    # remove the temporary cohort and manually garbage collect to clear up memory
    private$pop = NULL
    gc()
    return(NULL)
  } else {
    # remove the temporary cohort and manually garbage collect to clear up memory
    private$pop = NULL
    gc()

    # return a list
    return(cohortOut)
  }
}
