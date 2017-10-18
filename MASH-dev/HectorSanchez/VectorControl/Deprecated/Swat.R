fSwat = function(DF, hostID){
    #print("ITN")
    if(getHumanFromID(hostID)$SWAT == TRUE){
        print("Protected Human SWAT")
        DF = c(
          survive=INTERVENTIONS_PARAMETERS$SWAT$SurviveP,
          probe=INTERVENTIONS_PARAMETERS$SWAT$ProbeP,
          surviveprobe=INTERVENTIONS_PARAMETERS$SWAT$SurviveProbeP,
          feed=INTERVENTIONS_PARAMETERS$SWAT$FeedP
        )
    }
    #print(DF)
    DF
}

