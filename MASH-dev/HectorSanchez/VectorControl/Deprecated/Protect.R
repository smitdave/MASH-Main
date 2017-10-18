fProtection = function(DF, hostID){
    #print("ITN")
    if(getHumanFromID(hostID)$PROTECT == TRUE){
        print("Protected Human PROTECT")
        DF = c(
          survive=INTERVENTIONS_PARAMETERS$PROTECT$SurviveP,
          probe=INTERVENTIONS_PARAMETERS$PROTECT$ProbeP,
          surviveprobe=INTERVENTIONS_PARAMETERS$PROTECT$SurviveProbeP,
          feed=INTERVENTIONS_PARAMETERS$PROTECT$FeedP
        )
    }
    #print(DF)
    DF
}

