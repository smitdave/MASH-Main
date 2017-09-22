###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   MBITES-BRO: Cohort Methods
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################

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
#'
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
#'
mbitesBRO_cohort_oneMosquito_MBITES <- function(){

  # run algorithm until dead
  while(private$stateNew != "D"){
    self$oneBout()
  }

  # if mosquito is dead, output data and remove it from the enclosing storage object
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

#' MBITES-BRO-Cohort: Run Simulation for \code{\link{MosquitoPopFemale}}
#'
#' Run cohort simulation for population of mosquitoes and write bionomics to .json.
#'  * This method is bound to \code{MosquitoPopFemale$simCohort()}.
#' @param N number of mosquitoes in cohort
#'
mbitesBRO_cohort_simCohort <- function(N){

  # clean population
  private$pop$rmAll()
  gc()

  # assign population
  aquaStart = sample(x = private$LandscapePointer$get_AquaSitesN(),size = N,replace = TRUE)
  for(i in 1:N){
    # assign the mosquitoes
    myID = paste0("0_",i,"_1")
    private$pop$assign(key = myID, value = MosquitoFemale$new(id=myID,time=0,ix=aquaStart[i],genotype=1L,state=private$initState,eggT=self$get_MBITES_PAR("eggT"),eggP=self$get_MBITES_PAR("eggP"),energyPreG=self$get_MBITES_PAR("energyPreG")))

    private$pop$get(myID)$set_FemalePopPointer(self)
    private$pop$get(myID)$set_MalePopPointer(private$MalePopPointer)

    private$pop$get(myID)$set_TilePointer(private$TilePointer)
    private$pop$get(myID)$set_LandscapePointer(private$LandscapePointer)
    private$pop$get(myID)$set_HumansPointer(private$HumansPointer)
  }

  # begin json out
  count = 1
  fileName = paste0(private$TilePointer$get_MosquitoDirectory(),"MBITES_BRO_run",count,"_cohort.json")
  while(file.exists(fileName)){
    count = count + 1
    fileName = paste0(private$TilePointer$get_MosquitoDirectory(),"MBITES_BRO_run",count,"_cohort.json")
  }

  cat("writing M-BITES BRO Cohort bionomics to: ",fileName,"\n",sep="")
  private$TilePointer$set_FemaleHistoryCon(
    file(description = fileName,open = "wt")
  )

  writeLines(text = "[",con = private$TilePointer$get_FemaleHistoryCon())

  # run simulation
  private$pop$apply(tag="MBITES_Cohort",returnVal=FALSE)

  # clean up json out and close connection
  writeLines(text = "]",con = private$TilePointer$get_FemaleHistoryCon())
  cat("closing con: ",summary(private$TilePointer$get_FemaleHistoryCon())$description," please re-open before running more simulations\n",sep="")
  close(private$TilePointer$get_FemaleHistoryCon())

  # clean population
  private$pop$rmAll()
  gc()
  cat("M-BITES BRO Cohort run complete; please reset population and re-run appropriate setup functions before running full simulations\n",sep="")
}
