//  ***************************************************************************
//  Cattle.hpp
//  MASH
//  Definition of a cattle group.
//  ***************************************************************************

#ifndef Cattle_hpp
#define Cattle_hpp

#include <stdio.h>
#include "../VectorControl/Ivermectin/Ivermectin.hpp"
#include "../VectorControl/Zoospray/Zoospray.hpp"

class Cattle{
protected:
    float quantity;             //@@ Cattle quantity in a given spot
    float attractiveness;       //@@ Cattle attractiveness as a funciton of quantity
    Ivermectin ivermectin;      //@@ Ivermectin presence in cattle
    Zoospray zoospray;          //@@ Zoospray presence in cattle
public:
    //Constructors
    Cattle();
    Cattle(float quantityIn);
    //Accessors
    float getQuantity();
    float getAttractiveness();
    Ivermectin getIvermectin();
    Zoospray getZoospray();
    //Functions
    void updateAttractiveness();
};

#endif /* Cattle_hpp */
