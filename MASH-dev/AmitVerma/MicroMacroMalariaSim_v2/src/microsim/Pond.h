/*
 * Pond.h
 *
 */

#ifndef POND_H_
#define POND_H_
// Itamar edit 8/10/10
#include <cmath>
#include "GlobalParameters.h"
//#include "Distance.h"

class Patch;
class Distance;
class location;

/**
 * \brief A class to hold information about ponds.
 *
 * Ponds are the places for generation of new mosquitoes
 */

class Pond {
public:

    double lambda;  ///< Mosquito generation rate
    double coordX;  ///< X coordinate of the Pond
    double coordY;  ///< Y coordinate of the Pond
    //int    	patchID;     // Location in patch space
    Patch *patch;   ///< reference to home Patch
    double pMozzyAttractIndex; ///< Pond bite weight (check)
    // Itamar edited 8/13/10
    double pMozzySearchEfficiency;  ///< ?
    int pondID; ///< Contains Pond ID
    vector <Distance*> nearestHouses;

    location * loc;
    int locationVector; ///< ?

    Pond(int, double, double, int, double, double); ///< constructor
    ~Pond();    ///< default destructor
    void printState();  ///< prints state of the Pond

};
#endif /* POND_H_ */
