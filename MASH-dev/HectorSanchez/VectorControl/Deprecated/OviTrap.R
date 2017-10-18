fOvitrap = function(M){
  if(LANDSCAPE$aquaSites[[M$ix]]$OVITRAP){
    if(binomialEvent(1-INTERVENTIONS_PARAMETERS$OVITRAP$SurviveP)){
      print("Killed by ovitrap")
      M$stateNew="D"
    }
  }
  return(M)
}
