//
//  SiteSugar.cpp
//  MASH
//

#include "SiteMating.hpp"

SiteMating::SiteMating(){
    //@ Default constructor
    position={0,0,0};
    swarmSpray=SwarmSpray();
    areaRepellant=AreaRepellant();
    areaSpray=AreaSpray();
}
SwarmSpray SiteMating::getSwarmSpray(){
    //@ Return swarm spray
    return swarmSpray;
}
