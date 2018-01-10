//  ***************************************************************************
//  HumanPopulation.hpp
//  MASH
//  This class contains takes care of all the actions performed by Humans individuals
//  ***************************************************************************


#ifndef HumanPopulation_hpp
#define HumanPopulation_hpp

#include <stdio.h>
#include <vector>
#include "./Human.hpp"

class HumanPopulation {
protected:
    std::vector<Human>    humanPopulation;  //@@ Mosquito aquatic objects vector
public:
    //Constructors
    HumanPopulation(int vectorSize);
    //Destructors
    ~HumanPopulation();
};

#endif /* HumanPopulation_hpp */
