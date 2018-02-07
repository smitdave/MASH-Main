/*
 * Patch.h
 *
 */

#ifndef PATCH_H_
#define PATCH_H_


#include "House.h" //includes vector too
#include "Pond.h"
#include "HealthClinic.h"
#include "GlobalParameters.h"

/**
 * \brief A class to hold information about patches.
 *
 * Patches contain Houses which contain Humans into them
 */


class Patch {
public:
    int patchID; ///< Contains Patch ID
    double centroidX; ///< X coordinate of the Patch
    double centroidY; ///< Y coordinate of the Patch

    int nHouses; ///< Number of the Houses in the Patch
    //(Not accurate.. needs to be fixed)

    int nPonds; ///< Number of the Ponds in the Patch
    int neighborID[4]; ///< ID's of the four neighboring Patched
    vector <House*> houses; ///< List of Houses in the Patch
    vector <Pond*> ponds; ///< List of Ponds in the Patch

    // Itamar edited 8/11/10
    vector<Patch*> sortedPatchesLeft; ///< Neighbouring patches to the left
    vector<Patch*> sortedPatchesRight; ///< Neighbouring patches to the right

    HealthClinic * clinic;

    Patch(int, double, double, int, int, int, int, int, int); ///< constructor
    ~Patch(); ///< default destructor
    void addHouse(House*); ///< adds the new House to the Patch
    void addPond(Pond*); ///< adds the new Pond to the Patch
    void printState(); ///< prints state of the Patch
    int getTotalHumans(); ///< finds total number of Humans in the Patch
    void massVaccinate(double, double, double); ///< used for mass vaccinating the Patch
    void massDrugAdministrate(double); ///< used for mass drug administrating the Patch
    void sprayHouses(double); ///< sprays the Houses in the Patch
    void distributeNets(double); ///< distrubute bednets in the Patch
    House* findHouse(int); ///< finds House reference using its ID
    // Itamar edit 8/11/10
    Pond* findPond(int); ///< finds Pond reference using its ID
    int randomHouseIndex(); ///< returns a random House index from the Patch
    void setInitialInfection(double);

};

#endif /* PATCH_H_ */
