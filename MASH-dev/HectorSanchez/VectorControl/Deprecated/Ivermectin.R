fIvermectin = function(DF, hostID){
    print("Livestock host")
    if(hostID == -1){
        print("Ivermectin")
        DF = DF * c(
          survive =       INTERVENTIONS_PARAMETERS$IVERMECTIN$SurviveP,
          probe =         INTERVENTIONS_PARAMETERS$IVERMECTIN$ProbeP,
          surviveprobe =  INTERVENTIONS_PARAMETERS$IVERMECTIN$SurviveProbeP,
          feed =          INTERVENTIONS_PARAMETERS$IVERMECTIN$FeedP
        )
    }
    DF
}
