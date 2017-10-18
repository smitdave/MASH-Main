fAreaSpraying = function(M){
    #print("AreaSpray")
    if(LANDSCAPE$f$AREA.SPRAYING[M$f.i]==1){
        if(binomialEvent(1-CMParameters$AREA.SPRAYING$SurviveP)){
            print("Killed by Area Spraying")
            M$pState = "D"
        }else{
            if(binomialEvent(CMParameters$AREA.SPRAYING$RepelP)){
                print("Repelled by Area Spraying")
                M$iwofle = 5
            }
        }
    }
    M
}
