fAreaRepel = function(M){
  notInIXFZero=ixf(M)
  if(notInIXFZero!=0){
    #print("AreaRepel")
    if(LANDSCAPE$f$AREA.REPEL[ixf(M)]==1){
        if(binomialEvent(1-INTERVENTIONS_PARAMETERS$AREA.REPEL$SurviveP)){
            print("Killed by Area Repel")
            M$bStateNew = "D"
        }else{
            if(binomialEvent(INTERVENTIONS_PARAMETERS$AREA.REPEL$RepelP)){
                print("Repelled by Area Repel")
                M$lspot = "l"
            }
        }
    }
  }
  M
}
