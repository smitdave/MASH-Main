fITN = function(DF, hostID){
    #print("ITN")
    if(getHumanFromID(hostID)$ITN == TRUE){
        print("Protected Human ITN")
        DF = c(
          survive=INTERVENTIONS_PARAMETERS$ITN$SurviveP,
          probe=INTERVENTIONS_PARAMETERS$ITN$ProbeP,
          surviveprobe=INTERVENTIONS_PARAMETERS$ITN$SurviveProbeP,
          feed=INTERVENTIONS_PARAMETERS$ITN$FeedP
        )
    }
    #print(DF)
    DF
}
