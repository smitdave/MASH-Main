//
//  MosquitoGenericMale.cpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 1/18/17.
//  Copyright © 2017 MASH. All rights reserved.
//

#include "MosquitoGenericMale.hpp"

//******* Constructors ******************//
MosquitoGenericMale::MosquitoGenericMale(){
    //@ Male mosquito basic constructor
    state=M;
    age=0;
    damage=0;
    timeBorn=0;
};
//******* Accessors *********************//
//******* Mutators **********************//
//******* Main Actions ******************//
void MosquitoGenericMale::boutM(){
    //@ Performs mating actions
}
void MosquitoGenericMale::MBITES(){
    //@ Distributed hazards model actions selector according to the current mosquito stage
    switch(getState()){
        case(M):
            boutM();
            break;
        case(S):
            boutS();
            break;
        case(R):
            boutR();
            break;
        case(E):
            boutE();
            break;
        default:
            boutD();
    }
}
MosquitoGenericMale::~MosquitoGenericMale(){

}
