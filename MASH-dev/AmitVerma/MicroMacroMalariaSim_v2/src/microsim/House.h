/*
 * House.h
 *
 *
 */

#ifndef HOUSE_H_
#define HOUSE_H_

#include<vector>
// Itamar edited 8/10/10
#include<cmath>
//
#include "Human.h"
#include "HumanV2.h"
#include "Distance.h"
#include "GlobalParameters.h"

using std::vector;

class Patch;
class Distance;
class location;

/**
 * \brief A class to hold information about houses.
 *
 * Houses contain Humans
 */

class House {
public:

    int houseID;    ///< Contains House ID
    double coordX;  ///< X coordinate of the House
    double coordY;  ///< Y coordinate of the House
    //int    	patchID;     // Location in patch space
    Patch* patch;   ///< reference to the home Patch
    double hMozzyAttractIndex; ///< House Biting Weight
    // Itamar edited 8/13/10
    double hMozzySearchEfficiency;  ///< ?
    int nHumans;    ///< Number of Humans in the House
    int dayNewItn;  ///< last day when the bednets were distributed
    double itnRepellingEffect;  ///< strength of the bednets
    int dayNewIrs;  ///< last day when the House was sprayed
    double irsRepellingEffect;  ///< strength of the spray
    double irsDecayRate;    ///< decay rate of the spray
    double itnDecayRate;    ///< decay rate of the bednets

    // Itamar edit 8/10/10
    int locationVector; ///< Location in globally sorted vector
    //
    double Q;   ///< variable for cattle in the House

    vector <Human*> humans; ///< List of Humans in the House
    vector <Distance*> nearestHouses;
    vector <Distance*> nearestPonds;

    location *loc;

    /// constructor
    House(int, double, double, double, double, int, int, double, double, double, double);
    /// default destructor
    ~House();
    void printState();  ///< prints state of the House
    void spray();       ///< sprays the House
    double itnEffect(); ///< gives the effectivity of the bednets
    double irsEffect(); ///< gives the effectivity of the spray

};

#endif /* HOUSE_H_ */
