# fBiologicalControl= function(EggQ){
#   for(i in 1:length(EggQ$batch)){
#     eggsNumber=EggQ$batch[[i]]$nEggs
#     lID=tempBatch$l.ix
#     if(lID>0){
#       if(LANDSCAPE$l$BIOLOGICAL.CONTROL[[lID]]==1){
#         eggsNumber=eggsNumber*(1-rnormInterventionEffect(INTERVENTIONS_PARAMETERS$BIOLOGICAL.CONTROL[[2]],INTERVENTIONS_PARAMETERS$BIOLOGICAL.CONTROL[[3]]))
#       }
#       EggQ$batch[[i]]$nEggs<-eggsNumber
#     }
#   }
#   EggQ
# }
# ###################################################
# sapply(EggQ$batch,function(x){x$nEggs})
# EggQ2=fBiologicalControlEggQ(EggQ)
# sapply(EggQ2$batch,function(x){x$nEggs})
# ###################################################


fBiologicalControl= function(){
  1 - LANDSCAPE$l$BIOLOGICAL.CONTROL * (1-INTERVENTIONS_PARAMETERS$BIOLOGICAL.CONTROL$SurviveP)
}
