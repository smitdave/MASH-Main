//
//  MosquitoPopulation.cpp
//  MASHCPP
//

#include "MosquitoPopulation.hpp"

MosquitoPopulation::MosquitoPopulation(int adultPopulationSize, int aquaticPopulationSize){
    //@ Constructor of void mosquito population
    //Generate a void adult population (this is most likely inefficient)
    for(int i=0;i<adultPopulationSize;i++){adultPopulation.push_back(Mosquito());}
    //Generate a void aquatic population
    for(int j=0;j<aquaticPopulationSize;j++){aquaticPopulation.push_back(MosquitoAquatic());}
}
MosquitoPopulation::MosquitoPopulation(int adultPopulationSize, char adultPopulationStage, int aquaticPopulationSize){
    //@ Constructor of void mosquito population
    //Generate a void adult population
    for(int i=0;i<adultPopulationSize;i++){adultPopulation.push_back(MosquitoGenericFemale(D));}
    //Generate a void aquatic population
    for(int j=0;j<aquaticPopulationSize;j++){aquaticPopulation.push_back(MosquitoAquatic());}
}
std::vector<Mosquito> MosquitoPopulation::getAdultPopulation(){
    //@ Returns the adult population
    return adultPopulation;
}
std::vector<MosquitoAquatic> MosquitoPopulation::getAquaticPopulation(){
    //@ Returns the aquatic population
    return aquaticPopulation;
}
int MosquitoPopulation::countDeadMosquitos(){
    //@ Counts the mosquitos that are currently dead in the vector (could be useful when dynamical resizing of the vector comes into play)
    int counter=0;
    for(int i=0;i<adultPopulation.size();i++){
        if(adultPopulation.at(i).getState()==D){counter++;}
    }
    return counter;
}
int MosquitoPopulation::countMosquitosInBout(char stage){
    //@ Counts the mosquitos currently performing a given bout
    int counter=0;
    for(int i=0;i<adultPopulation.size();i++){
        if(adultPopulation.at(i).getState()==stage){counter++;}
    }
    return counter;
}
void MosquitoPopulation::insertImagosIntoAdultPopulation(){
    //@ Takes mosquitos from imagoQueue and adds them into dead mosquito slots in the adults population. If no dead slots are found it appends the new mosquito to the end of the vector.
    //Temporary data that should come from the Imago Queue
    int neededSlots=3;                   //Placeholder
    MosquitoGenericFemale newMosquito=MosquitoGenericFemale();    //This mosquito should have the properties defined by the Imago and should be an array
    //Replace a dead mosquito with an emerged one from the imagoQueue
    int replacedMosquitosSoFar=0;
    for(int i=0;i<adultPopulation.size();i++){
        if(adultPopulation.at(i).getState()==D){
            adultPopulation.at(i)=newMosquito;
            replacedMosquitosSoFar++;
            if(replacedMosquitosSoFar>=neededSlots){break;}
        }
    }
    //If dead mosquitos slots were not enough push new ones at the end
    for(int j=0;j<(neededSlots-replacedMosquitosSoFar);j++){adultPopulation.push_back(newMosquito);}
}
void MosquitoPopulation::printAdultMosquitoStates(){
    //@ Prints the adultPopulation states of the individuals
    std::cout<<"Mosquito Population Stages: ";
    for(int i=0;i<adultPopulation.size();i++){
        std::cout<<adultPopulation.at(i).getState();
    }
    std::cout<<std::endl;
}
