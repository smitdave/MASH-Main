fRepel = function(DF, hostID){
    #print("ITN")
    if(getHumanFromID(hostID)$REPEL == TRUE){
        print("Protected Human REPEL")
        DF = c(
          survive=INTERVENTIONS_PARAMETERS$REPEL$SurviveP,
          probe=INTERVENTIONS_PARAMETERS$REPEL$ProbeP,
          surviveprobe=INTERVENTIONS_PARAMETERS$REPEL$SurviveProbeP,
          feed=INTERVENTIONS_PARAMETERS$REPEL$FeedP
        )
    }
    #print(DF)
    DF
}

