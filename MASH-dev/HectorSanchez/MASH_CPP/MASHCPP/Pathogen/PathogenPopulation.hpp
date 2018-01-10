//  ***************************************************************************
//  PathogenPopulation.hpp
//  MASHCPP
//  This class contains the vector of pathogens currently in the simulation.
//  The vector should be initialised with a large enough length at startup to avoid reallocations.
//  As pathogens die and are "born" they are replaced in the vector.
//  Both humans and mosquitos will point to these pathogens.
//  ***************************************************************************

#ifndef PathogenPopulation_hpp
#define PathogenPopulation_hpp

#include <stdio.h>
#include <vector>
#include "./Pathogen.hpp"

class PathogenPopulation {
protected:
    std::vector<Pathogen>    pathogenPopulation;  //@@ Mosquito aquatic objects vector
public:
    //Constructors
    PathogenPopulation(int vectorSize);
    //Destructors
    ~PathogenPopulation();
};

#endif /* PathogenPopulation_hpp */
