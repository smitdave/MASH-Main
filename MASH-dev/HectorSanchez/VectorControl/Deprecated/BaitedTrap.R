fBaitedTrap = function(M){
  notInIXFZero=ixf(M)
  if(notInIXFZero!=0){
    if(LANDSCAPE$f$BAITED.TRAP[ixf(M)]==1){
      if(binomialEvent(1-INTERVENTIONS_PARAMETERS$BAITED.TRAP$SurviveP)){
        print("Killed by BAITED.TRAP")
        M$bStateNew = "D"
      }
    }
  }
  M
}
