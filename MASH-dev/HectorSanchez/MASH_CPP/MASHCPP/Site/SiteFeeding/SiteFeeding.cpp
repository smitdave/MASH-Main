//
//  SiteFeeding.cpp
//  MASH
//

#include "SiteFeeding.hpp"


SiteFeeding::SiteFeeding(){
    //@ Default constructor
    position={0,0,0};
    eaveTube=EaveTube();
    areaRepellant=AreaRepellant();
    areaSpray=AreaSpray();
    indoorResidualSpray=IndoorResidualSpray();
    baitedTrap=BaitedTrap();
}
EaveTube SiteFeeding::getEaveTube(){
    //@ Return eave tube
    return eaveTube;
}
IndoorResidualSpray SiteFeeding::getIndoorResidualSpray(){
    //@ Return indoor residual spray
    return indoorResidualSpray;
}
