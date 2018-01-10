//
//  HumanPopulation.cpp
//  MASHCPP
//

#include "HumanPopulation.hpp"

HumanPopulation::HumanPopulation(int vectorSize){
    for(int i=0;i<vectorSize;i++){humanPopulation.push_back(Human());}
}
HumanPopulation::~HumanPopulation(){

}
