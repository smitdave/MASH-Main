//  ***************************************************************************
//  Human.hpp
//  MASH
//  This class contains all the routines for humans to develop in MASH
//  ***************************************************************************

#ifndef Human_hpp
#define Human_hpp

#include <stdio.h>
#include <vector>
#include "../VectorControl/PersonalProtection/PersonalProtection.hpp"
#include "../VectorControl/PersonalRepellant/PersonalRepellant.hpp"
#include "../VectorControl/ITN/ITN.hpp"
#include "../Pathogen/ZIKV/ZIKV.hpp"
#include "../Pathogen/CHIKV/CHIKV.hpp"
#include "../Pathogen/DENV/DENV.hpp"
#include "../Pathogen/PlasmodiumVivax/PlasmodiumVivax.hpp"
#include "../Pathogen/PlasmodiumFalciparum/PlasmodiumFalciparum.hpp"

class Human{
protected:
    int                     id;                     //@@ Individual identifier
    InsecticideTreatedNet*  itn;                    //@@ Insecticide-treated net object
    PersonalRepellant*      personalRepellant;      //@@ Personal repellant object
    PersonalProtection*     personalProtection;     //@@ Personal protection object
    std::vector<Pathogen*>  pathogenVector;         //@@ This vector should contain the pointers the all the current infections of the human
public:
    //Constructors
    Human();
    //Accessors
    int                     getID();
    InsecticideTreatedNet   getInsecticideTreatedNet();
    PersonalProtection      getPersonalProtection();
    PersonalRepellant       getPersonalRepellant();
    //Destructors
    ~Human();
};

#endif /* Human_hpp */
