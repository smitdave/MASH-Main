//
//  SiteLaying.cpp
//  MASH
//

#include "SiteLaying.hpp"

SiteLaying::SiteLaying(){
    //@ Default constructor
    position={0,0,0};
    ovitrap=Ovitrap();
    larviciding=Larviciding();
    sourceReduction=SourceReduction();
    biologicalControl=BiologicalControl();
    areaRepellant=AreaRepellant();
    areaSpray=AreaSpray();
}
Ovitrap SiteLaying::getOvitrap(){
    //@ Return ovitrap
    return ovitrap;
}
Larviciding SiteLaying::getLarviciding(){
    //@ Return larviciding
    return larviciding;
}
SourceReduction SiteLaying::getSourceReduction(){
    //@ Return source reduction
    return sourceReduction;
}
BiologicalControl SiteLaying::getBiologicalControl(){
    //@ Return biological control
    return biologicalControl;
}
