/*
 * Mosquito.h
 *
 *
 */


#ifndef MOSQUITO_H_
#define MOSQUITO_H_
#define NOT_RESTING 	-1
#define RESTING_OUTDOOR 0
#define RESTING_INDOOR 	1


#include<vector>
#include "Patch.h"
#include "House.h"
#include "Pond.h"
#include "GlobalParameters.h"
#include "location.h"
#include "Distance.h"

using std::vector;

extern vector<vector<location *> > sortedLocations;

/// A class to hold information about the mosquitoes

class Mosquito {
public:

    int mozzyID;

    // Birthday & Death day
    int bday; ///< Birthday
    int dday; ///< Deathday

    // Location
    location *currentLocation; ///< current location of the Mosquito
    Patch *patch; ///< reference to the home Patch
    //	int     pondID;              
    //	int     patchID;
    //	int     houseID;

    bool isInfected; ///< true if Mosquito is infected
    int infectionStartDay;
    int sporogonyDays;

    double probRestIndoor; ///< probability of resting indoors

    double nextActionDay; ///< day on which next step of Mosquito life cycle begins

    double restDays; ///< No. of days for rest
    int restStartDay; //to be set to zero when changed from rest to oviposite
    /**
     * resting indoor or outdoor
     *
     * intially set to NOT_RESTING
     */
    int whereResting;

    double ovipositDays; ///< No. of days for ovipositing
    int ovipositStartDay;

    //int		timeToBite; //initially 0

    double probAliveAtRest; ///< probability of staying alive at rest
    double probAliveAtOviposit; ///< probability of staying alive while ovipositing

    //int 	searchPatchID;
    Patch *searchPatch; ///< reference to the Patch where Mosquito goes to search for s blood meal
    double ySearch;

    double dayUsed;
    int stage;
    int dayNextStage;

    Mosquito(int, int, int, int, int, double, double, double, int, int, int); ///< constructor
    ~Mosquito(); ///< default destructor

    bool fly(bool searchHouse, bool reflect, double delta, double k, double time = .05);
    bool flyToHouse(double, int);
    bool flyToPond(double, int);
    bool searchHouse(vector<Patch*> &patches);
    bool searchHouse();
    House * chooseNextHouse(bool fromPond, bool flyLeft, int futureDay, double delta, double k = 1.);
    Pond * chooseNextPond(bool flyLeft, int futureDay, double delta, double k = 1.);
    bool biteHuman(House *hs); ///< looks into a house for a blood meal
    double getPrDeathAtRest(vector<Patch*> &patches); ///< gives probability of death at rest
    double getPrDeathAtRest(); ///< gives probability of death at rest
};

#endif /* MOSQUITO_H_ */
