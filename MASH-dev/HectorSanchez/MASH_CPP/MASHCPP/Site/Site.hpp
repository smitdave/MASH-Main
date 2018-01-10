//  ***************************************************************************
//  Site.hpp
//  MASH
//  Defines the general variables a generic site should declare.
//  ***************************************************************************

#ifndef Site_hpp
#define Site_hpp

#include <stdio.h>
#include "../GlobalParameters/Globals.hpp"
#include "../AuxiliaryFunctions/AuxiliaryFunctions.hpp"
#include "./MovementKernel.hpp"
#include "../VectorControl/AreaRepellant/AreaRepellant.hpp"
#include "../VectorControl/AreaSpray/AreaSpray.hpp"

class Site {
protected:
    int             id;             //@@ Identifier of the site object
    coordinate      position;       //@@ X,Y,Z coordinate of the object in the landscape
    MovementKernel  movementKernel; //@@ Contains the distances to all other sites in the landscape
    AreaRepellant   areaRepellant;  //@@ Contains area repellant object
    AreaSpray       areaSpray;      //@@ Contains area spray object
public:
    //Constructors
    Site();
    Site(int idIn, coordinate positionIn);
    //Accessors
    coordinate getCoordinates();
    AreaRepellant       getAreaRepellant();
    AreaSpray           getAreaSpray();
    //Mutators
    //Actions
    double distanceToSite(Site otherSite);
};

#endif /* Site_hpp */
