fEaveTube = function(M){
    #print("EAVE TUBE")
    if(LANDSCAPE$f$EAVE.TUBE[ixf(M)] == 1){
        if(binomialEvent(1-INTERVENTIONS_PARAMETERS$EAVE.TUBE$SurviveP)){
            print("* Killed by eave tube *********************")
            M$bStateNew = "D"
        }else{
            if(binomialEvent(INTERVENTIONS_PARAMETERS$EAVE.TUBE$RepelP)){
                print("* Repelled by eave tube *********************")
                M$iwofle = 5
            }
        }
    }
    M
}
