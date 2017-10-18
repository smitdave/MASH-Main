fIRS = function(M){
    #print("IRS")
    notInIXFZero=ixf(M)
    if(notInIXFZero!=0){
      if(M$lspot=="i"){
          if(LANDSCAPE$f$IRS[notInIXFZero]==1){
              if(binomialEvent(1-INTERVENTIONS_PARAMETERS$IRS$SurviveP)){
                  print("* Killed by IRS1 *********************")
                  M$bStateNew  = "D"
              }
          }
      }else{
          if(LANDSCAPE$f$IRS[notInIXFZero]==1){
          #FOR NOW IT WILL TREAT INSIDE AND OUSTIDE WALLS AS EQUALLY EFFECTIVE. THIS SHOULD BE CHANGED LATER ON.
              if(binomialEvent(1-INTERVENTIONS_PARAMETERS$IRS$SurviveP)){
                  print("* Killed by IRS2 *********************")
                  M$bStateNew  = "D"
              }
          }
      }
    }
    M
}
