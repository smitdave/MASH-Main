# close encounters of the anopheline kind...
choosehost <- function(mosy){
  mosy$hostID <- get("landscape",.GlobalEnv)[[mosy$site]]$riskQ$sampleQ()
}


# human host encounter
humanEncounter <- function(mosy){

  p <- get("parameters",.GlobalEnv)$surviveH
  if(runif(1) < 1 - p){

    # dies on approach
    mosy$statenext <- "D"
    mosy$hist$cod <- "surviveH"

  } else {

    # survives to probe
    p <- get("parameters",.GlobalEnv)$probeH
    if(runif(1) < p){

      # undeterred, probes the host
      probeHost(mosy) # m -> h transmission (a pathogen module)
      # track history
      if(mosy$hist$feed){
        track_probe(mosy)
      }

      p <- get("parameters",.GlobalEnv)$surviveprobeH
      if(runif(1) < 1 - p){

        # does not survive to blood feed
        mosy$statenext <- "D"
        mosy$hist$cod <- "surviveprobeH"

      } else {

        # survives to blood feed
        p <- get("parameters")$feedH
        if(runif(1) < feedH){
          # successfully begins to blood feed

          feedHost(mosy) # h -> m transmission (a pathogen module)
          bloodmeal(mosy)

          # track history
          if(mosy$hist$feed){
            track_feed(mosy)
          }

        }

      }

    }

  }

}


# zoo host encounter
zooEncounter <- function(mosy){

  p <- get("parameters",.GlobalEnv)$surviveZ
  if(runif(1) < 1 - p){

    # does not survive to feed
    mosy$statenext <- "D"
    mosy$hist$cod <- "surviveZ"

  # survives to feed
  } else {

    p <- get("parameters",.GlobalEnv)$feedZ
    if(runif(1) < p){

      # successfully begins blood feeding
      self$bloodmeal()

      # track history
      if(mosy$hist$feed){
        track_probe(mosy)
        track_feed(mosy)
      }

    }
  }
}
