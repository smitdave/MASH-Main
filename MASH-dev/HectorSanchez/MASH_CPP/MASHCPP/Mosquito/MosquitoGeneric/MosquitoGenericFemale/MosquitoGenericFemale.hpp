//
//  MosquitoGenericFemale.hpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 1/18/17.
//  Copyright © 2017 MASH. All rights reserved.
//

#ifndef MosquitoGenericFemale_hpp
#define MosquitoGenericFemale_hpp

#include <stdio.h>
#include "../../Mosquito.hpp"

//Inherits from Mosquito class
class MosquitoGenericFemale: public Mosquito {
private:
    float   bloodMealSize;          //@ Amount of blood currently contained in the mosquito
    float   eggBatchSize;           //@ Size of the egg batch developing in the mosquito
    //Distributions: 
public:
    //-- Constructors --------------------------
    MosquitoGenericFemale();        //Initializing empty mosquito
    MosquitoGenericFemale(char stateIn);
    //-- Accessors -----------------------------
    //-- Mutators ------------------------------
    //-- Actions -------------------------------
    void MBITES();
    void boutF();
    void boutB();
    void boutL();
    void boutO();
    void boutM();
    //-- Destructors ---------------------------
    ~MosquitoGenericFemale();
};

#endif /* MosquitoGenericFemale_hpp */
