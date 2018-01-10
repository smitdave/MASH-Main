//  ***************************************************************************
//  MovementKernel.hpp
//  MASH
//  Distances from the current site to all other places in the landscape
//  ***************************************************************************
#ifndef MovementKernel_hpp
#define MovementKernel_hpp

#include <stdio.h>

class MovementKernel{
protected:
    //Each site will have a set of vectors that contain the distances to all the other sites in the landscape. These distances are static and no sites are created/destroyed. The distances should be calculated at startup.
    
    //float distancesToFeeding[FEEDING_SITES_NUMBER];
    //float distancesToSugar[SUGAR_SITES_NUMBER];
    //float distancesToMating[MATING_SITES_NUMBER];
    //float distancesToLaying[LAYING_SITES_NUMBER];
public:
};

#endif /* MovementKernel_hpp */
