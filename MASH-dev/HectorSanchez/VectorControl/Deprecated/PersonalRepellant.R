fPersonalRepellant = function(DF, hostID){
    #print("Personal Repellant")
    if(getHumanFromID(hostID)$PERSONAL.REPELLANT == TRUE){
        print("Protected Human PERSONAL.REPELLANT")
        DF = DF * c(
        survive =       INTERVENTIONS_PARAMETERS$PERSONAL.REPELLANT$SurviveP,
        probe =         INTERVENTIONS_PARAMETERS$PERSONAL.REPELLANT$ProbeP,
        surviveprobe =  INTERVENTIONS_PARAMETERS$PERSONAL.REPELLANT$SurviveProbeP,
        feed =          INTERVENTIONS_PARAMETERS$PERSONAL.REPELLANT$FeedP
        )
    }
    DF
}
