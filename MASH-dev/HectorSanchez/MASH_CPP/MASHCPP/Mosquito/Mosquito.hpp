//  ***************************************************************************
//  Mosquito.hpp
//  MASH
//  Mosquito object abstract class. This is the most important part of the simulation so extra
//  care should be taken when making changes and debugging these routines.
//  ***************************************************************************

#ifndef Mosquito_hpp
#define Mosquito_hpp

#include <iostream>
#include <stdio.h>
#include "boost/random.hpp"
#include "../GlobalParameters/Globals.hpp"
#include "./MBITES/BloodFeed.hpp"
#include "./MBITES/Estivate.hpp"
#include "./MBITES/Mate.hpp"
#include "./MBITES/SugarFeed.hpp"
#include "./MBITES/Flight.hpp"
#include "./MBITES/Swarm.hpp"
#include "./MosquitoLifeParameters.hpp"
#include "../VectorControl/GM/GM.hpp"
#include "../Pathogen/ZIKV/ZIKV.hpp"
#include "../Pathogen/CHIKV/CHIKV.hpp"
#include "../Pathogen/DENV/DENV.hpp"
#include "../Pathogen/PlasmodiumVivax/PlasmodiumVivax.hpp"
#include "../Pathogen/PlasmodiumFalciparum/PlasmodiumFalciparum.hpp"

class Mosquito {
protected:
    unsigned int            id;             //@@ Mosquito identifier
    char                    state;          //@@ Current life cycle stage
    float                   age;            //@@ Age of the mosquito
    float                   damage;         //@@ Damage to mosquito's wings
    int                     timeBorn;       //@@ Time in which mosquito was born
    float                   energy;         //@@ Mosquito energy level (0 to 1)
    float                   currentTime;    //@@
    float                   anthropophilia; //@@ Mosquito level of attraction towards humans
    GM                      gm;             //@@ GM object (vector control)
    std::vector<Pathogen>   pathogenVector;         //@@ This vector should contain all current pathogen infections
    //structure for history
    //Statistical distributions
public:
    //-- Constructors --------------------------
    //Mosquito();
    //Mosquito(char stateIn);
    //-- Accessors -----------------------------
    char    getState();
    float   getDamage();
    GM      getGM();
    //-- Mutators ------------------------------
    void setState(char stateIn);
    void setDamage(float damageIn);
    //-- Main Actions --------------------------
    void boutR();
    void boutS();
    void boutE();
    void boutD();
    //-- Auxiliary Actions ---------------------
};

#endif /* Mosquito_hpp */
