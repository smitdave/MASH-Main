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

#' MBITES-BRO-Cohort: Human Host Encounter for \code{\link{MosquitoFemale}}
#'
#' After calling \code{\link{mbitesGeneric_chooseHost}}, the mosquito encounters a human host and attempts to feed.
#'  * This method is bound to \code{MosquitoFemale$humanEncounter()}.
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

#' MBITES-BRO-Cohort: Run Simulation for \code{\link{MosquitoFemale}}
#'
#' Run the M-BITES life cycle simulation algorithm until mosquito is dead.
#' This method calls \code{\link{mbitesBRO_oneBout}} to simulate each life stage.
#' For differences between cohort and microsimulation versions see \code{\link{MBITES.BRO.Cohort.Setup}}
#'  * This method is bound to \code{MosquitoFemale$MBITES_Cohort()}.
#'
#' @md
mbitesBRO_cohort_oneMosquito_MBITES <- function(){

  # run algorithm until dead
  while(private$stateNew != "D"){
    self$oneBout()
  }

  # if mosquito is dead output data if asked and remove it from the enclosing storage object
  if(private$stateNew == "D"){
    if(!is.null(private$TilePointer$get_FemaleHistoryCon())){
      cat(jsonlite::toJSON(x = self$get_history(),pretty = TRUE),",\n",sep="",file = private$TilePointer$get_FemaleHistoryCon())
    }
    self$rmSelf()
  }

}


#################################################################
# MBITES-BRO-Cohort: Run Cohort Simulation
#################################################################

#' MBITES-BRO-Cohort: Human Host Encounter for \code{\link{MosquitoFemale}}
#'
#' After calling \code{\link{mbitesGeneric_chooseHost}}, the mosquito encounters a human host and attempts to feed.
#'  * This method is bound to \code{MosquitoPopFemale$simCohort()}.
#' @param N number of mosquitoes in cohort
#' @param writeJSON if \code{TRUE} write output to JSON in the directory initialized in the enclosing \code{\link{MicroTile}}, else return a list
#' @md
mbitesBRO_cohort_simCohort <- function(N){

  # assign population
  aquaStart = sample(x = private$LandscapePointer$get_AquaSitesN(),size = N,replace = TRUE)
  self$push_pop(N=N,tEmerge=0,genotype=1L,ix=aquaStart)

  # begin json out
  writeLines(text = "[",con = private$TilePointer$get_FemaleHistoryCon())

  # run simulation
  private$pop$apply(tag="MBITES_Cohort",returnVal=FALSE)

  # clean up json out and close connection
  writeLines(text = jsonlite::toJSON(x = mbitesGeneric_NULL,pretty = TRUE),con = private$TilePointer$get_FemaleHistoryCon())
  writeLines(text = "]",con = private$TilePointer$get_FemaleHistoryCon())
  print(paste0("closing con: ",summary(private$TilePointer$get_FemaleHistoryCon())$description," please re-open before running more simulations"))
  close(private$TilePointer$get_FemaleHistoryCon())

  # manually call garbage collection
  gc()
}
