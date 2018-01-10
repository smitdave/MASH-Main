//
//  PathogenPopulation.cpp
//  MASHCPP
//

#include "PathogenPopulation.hpp"

PathogenPopulation::PathogenPopulation(int vectorSize){
    for(int i=0;i<vectorSize;i++){pathogenPopulation.push_back(Pathogen());}
}
PathogenPopulation::~PathogenPopulation(){
    
}
