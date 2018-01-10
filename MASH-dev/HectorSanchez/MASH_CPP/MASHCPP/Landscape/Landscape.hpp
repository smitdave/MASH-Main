//  ***************************************************************************
//  Landscape.hpp
//  MASH
//  Defines the world in which the simulation will take place.
//  ***************************************************************************
#ifndef Landscape_hpp
#define Landscape_hpp

#include <stdio.h>
#include <vector>
#include "../Site/SiteSugar/SiteSugar.hpp"
#include "../Site/SiteMating/SiteMating.hpp"
#include "../Site/SiteFeeding/SiteFeeding.hpp"
#include "../Site/SiteLaying/SiteLaying.hpp"

class Landscape {
protected:
    std::vector<SiteSugar>      sugarSites;     //@@ Vector to store sugarSites
    std::vector<SiteLaying>     layingSites;    //@@ Vector to store layingSites
    std::vector<SiteMating>     matingSites;    //@@ Vector to store matingSites
    std::vector<SiteFeeding>    feedingSites;   //@@ Vector to store feedingSites
public:
    //Constructors
    Landscape();
};

#endif /* Landscape_hpp */
