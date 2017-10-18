fImproveHome = function(M){
  notInIXFZero=ixf(M)
  if(notInIXFZero!=0){
    #print("ImproveHome")
    if(LANDSCAPE$f$AREA.REPEL[ixf(M)]==1){
        if(binomialEvent(1-INTERVENTIONS_PARAMETERS$IMPROVE.HOME$SurviveP)){
            print("Killed by Improve Home")
            M$bStateNew = "D"
        }else{
            if(binomialEvent(INTERVENTIONS_PARAMETERS$IMPROVE.HOME$RepelP)){
                print("Repelled by Improve Home")
                M$iwofle = 5
            }
        }
    }
  }
  M
}
