//  ***************************************************************************
//  SiteMating.hpp
//  MASH
//  Mating landmark site definition. Inherits from Site class.
//  ***************************************************************************

#ifndef SiteMating_hpp
#define SiteMating_hpp

#include <stdio.h>
#import "../Site.hpp"
#include "../../VectorControl/SwarmSpray/SwarmSpray.hpp"

//Inherits from Site
class SiteMating: public Site {
private:
    SwarmSpray swarmSpray;          //@@ Swarm spray object in site
public:
    //Constructors
    SiteMating();
    //Accessors
    SwarmSpray getSwarmSpray();
    //Mutators
};

#endif /* SiteMating_hpp */
