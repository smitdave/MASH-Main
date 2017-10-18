# fSourceReduction = function(EggQ){
#   for(i in 1:length(EggQ$batch)){
#     eggsNumber=EggQ$batch[[i]]$nEggs
#     lID=tempBatch$l.ix
#     if(lID>0){
#       if(LANDSCAPE$l$SOURCE.REDUCTION[[lID]]==1){
#         eggsNumber=eggsNumber*(1-rnormInterventionEffect(INTERVENTIONS_PARAMETERS$SOURCE.REDUCTION[[2]],INTERVENTIONS_PARAMETERS$SOURCE.REDUCTION[[3]]))
#       }
#       EggQ$batch[[i]]$nEggs<-eggsNumber
#     }
#   }
#   EggQ
# }
# ###################################################
# sapply(EggQ$batch,function(x){x$nEggs})
# EggQ2=fSourceReduction(EggQ)
# sapply(EggQ2$batch,function(x){x$nEggs})
# ###################################################

fSourceReduction = function(){
  1 - LANDSCAPE$l$SOURCE.REDUCTION * (1-INTERVENTIONS_PARAMETERS$SOURCE.REDUCTION$SurviveP)
}
