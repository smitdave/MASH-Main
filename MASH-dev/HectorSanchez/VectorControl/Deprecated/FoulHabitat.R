# fFoulHabitat = function(EggQ){
#   for(i in 1:length(EggQ$batch)){
#     eggsNumber=EggQ$batch[[i]]$nEggs
#     lID=tempBatch$l.ix
#     if(lID>0){
#       if(LANDSCAPE$l$FOUL.HABITAT[[lID]]==1){
#         eggsNumber=eggsNumber*(1-rnormInterventionEffect(INTERVENTIONS_PARAMETERS$FOUL.HABITAT[[2]],INTERVENTIONS_PARAMETERS$FOUL.HABITAT[[3]]))
#       }
#       EggQ$batch[[i]]$nEggs<-eggsNumber
#     }
#   }
#   EggQ
# }
# ###################################################
# sapply(EggQ$batch,function(x){x$nEggs})
# EggQ2=fFoulHabitat(EggQ)
# sapply(EggQ2$batch,function(x){x$nEggs})
# ###################################################

fFoulHabitat = function(){
  1 - LANDSCAPE$l$FOUL.HABITAT * (1-INTERVENTIONS_PARAMETERS$FOUL.HABITAT$SurviveP)
}
