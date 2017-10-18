# fLarviciding = function(EggQ){
#   for(i in 1:length(EggQ$batch)){
#     eggsNumber=EggQ$batch[[i]]$nEggs
#     lID=tempBatch$l.ix
#     if(lID>0){
#       if(LANDSCAPE$l$LARVICIDING[[lID]]==1){
#         eggsNumber=eggsNumber*(1-rnormInterventionEffect(INTERVENTIONS_PARAMETERS$LARVICIDING[[2]],INTERVENTIONS_PARAMETERS$LARVICIDING[[3]]))
#       }
#       EggQ$batch[[i]]$nEggs<-eggsNumber
#     }
#   }
#   EggQ
# }
# ###################################################
# sapply(EggQ$batch,function(x){x$nEggs})
# EggQ2=fLarviciding(EggQ)
# sapply(EggQ2$batch,function(x){x$nEggs})
# ###################################################


fLarviciding= function(){
  1 - LANDSCAPE$l$LARVICIDING * (1-INTERVENTIONS_PARAMETERS$LARVICIDING$SurviveP)
}
