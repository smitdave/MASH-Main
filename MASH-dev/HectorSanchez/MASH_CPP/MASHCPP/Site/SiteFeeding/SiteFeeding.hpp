//  ***************************************************************************
//  SiteFeeding.hpp
//  MASH
//  Blood-feeding site definition. Inherits from Site class.
//  ***************************************************************************

#ifndef SiteFeeding_hpp
#define SiteFeeding_hpp

#include <stdio.h>
#import "../Site.hpp"
#import "../../VectorControl/EaveTube/EaveTube.hpp"
#import "../../VectorControl/IRS/IRS.hpp"
#import "../../VectorControl/BaitedTrap/BaitedTrap.hpp"

//Inherits from Site
class SiteFeeding: public Site {
private:
    EaveTube                eaveTube;               //@@ Eave tube object in site
    IndoorResidualSpray     indoorResidualSpray;    //@@ Indoor Residual Spray object in site
    BaitedTrap              baitedTrap;             //@@ Baited trap object in site
public:
    //Constructors
    SiteFeeding();
    //Accessors
    EaveTube            getEaveTube();
    IndoorResidualSpray getIndoorResidualSpray();
    BaitedTrap          getBaitedTrap();
    //Mutators
};

#endif /* SiteFeeding_hpp */
