//
//  MosquitoGenericMale.hpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 1/18/17.
//  Copyright © 2017 MASH. All rights reserved.
//

#ifndef MosquitoGenericMale_hpp
#define MosquitoGenericMale_hpp

#include <stdio.h>
#include "../../Mosquito.hpp"

//Inherits from Mosquito class
class MosquitoGenericMale: public Mosquito {
private:
public:
    //-- Constructors --------------------------
    MosquitoGenericMale();      //Initializing empty mosquito
    //-- Accessors -----------------------------
    //-- Mutators ------------------------------
    //-- Actions -------------------------------
    void MBITES();
    void boutM();
    //-- Destructors ---------------------------
    ~MosquitoGenericMale();
};

#endif /* MosquitoGenericMale_hpp */
