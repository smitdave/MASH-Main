//  ***************************************************************************
//  MosquitoPopulation.hpp
//  MASH
//  This class takes care of the actions performed by the actions of mosquitos in all life stages
//  ***************************************************************************

#ifndef MosquitoPopulation_hpp
#define MosquitoPopulation_hpp

#include <stdio.h>
#include <vector>
#include "./Mosquito.hpp"
#include "./MosquitoGeneric/MosquitoGenericMale/MosquitoGenericMale.hpp"
#include "./MosquitoGeneric/MosquitoGenericFemale/MosquitoGenericFemale.hpp"
#include "./MosquitoAquatic.hpp"

class MosquitoPopulation {
protected:
    std::vector<MosquitoAquatic>    aquaticPopulation;  //@@ Mosquito aquatic objects vector
    std::vector<Mosquito>           adultPopulation;    //@@ Mosquito adults objects vector
public:
    //Constructors
    MosquitoPopulation(int adultPopulationSize, int aquaticPopulationSize);
    MosquitoPopulation(int adultPopulationSize, char adultPopulationStage, int aquaticPopulationSize);
    //Accesors
    std::vector<Mosquito> getAdultPopulation();
    std::vector<MosquitoAquatic> getAquaticPopulation();
    //Actions
    int countDeadMosquitos();
    int countMosquitosInBout(char stage);
    void insertImagosIntoAdultPopulation();
    //Helpers
    void printAdultMosquitoStates();
};

#endif /* MosquitoPopulation_hpp */
