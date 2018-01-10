//
//  MosquitoGenericFemale.cpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 1/18/17.
//  Copyright © 2017 MASH. All rights reserved.
//

#include "MosquitoGenericFemale.hpp"

//******* Constructors ******************//
MosquitoGenericFemale::MosquitoGenericFemale(){
    //@ Female mosquito basic constructor
    state=M;
    age=0;
    damage=0;
    timeBorn=0;
    bloodMealSize=0;
    eggBatchSize=0;
};
MosquitoGenericFemale::MosquitoGenericFemale(char stateIn){
    //@ Female mosquito basic constructor
    state=stateIn;
    age=0;
    damage=0;
    timeBorn=0;
    bloodMealSize=0;
    eggBatchSize=0;
};
//******* Accessors *********************//
//******* Mutators **********************//
//******* Main Actions ******************//
void MosquitoGenericFemale::boutF(){
    //@ Performs the blood-feeding search cycle
}
void MosquitoGenericFemale::boutB(){
    //@ Performs the blood-feeding action
}
void MosquitoGenericFemale::boutL(){
    //@ Performs the egg-laying search cycle
}
void MosquitoGenericFemale::boutO(){
    //@ Performs the egg-laying action
}
void MosquitoGenericFemale::boutM(){
    //@ Performs mating actions
}
void MosquitoGenericFemale::MBITES(){
    //@ Distributed hazards model actions selector according to the current mosquito stage
    switch(getState()){
        case(F):
            boutM();
            break;
        case(B):
            boutB();
            break;
        case(L):
            boutL();
            break;
        case(O):
            boutO();
            break;
        case(M):
            boutM();
            break;
        case(S):
            break;
        default:
            boutD();
    }
}
MosquitoGenericFemale::~MosquitoGenericFemale(){
    
}
