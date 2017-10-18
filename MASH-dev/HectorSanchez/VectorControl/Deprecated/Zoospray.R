fZooSpray = function(DF, hostID){
    #print("Livestock host")
    if(hostID == -1){
        print("Zoosprayed")
        DF = DF * c(
        survive =       CMParameters$ZOOSPRAY$SurviveP,
        probe =         CMParameters$ZOOSPRAY$ProbeP,
        surviveprobe =  CMParameters$ZOOSPRAY$SurviveProbeP,
        feed =          CMParameters$ZOOSPRAY$FeedP
        )
    }
    DF
}