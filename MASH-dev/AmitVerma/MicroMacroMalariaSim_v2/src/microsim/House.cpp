/*
 * House.cpp
 *
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include "House.h"
// Itamar edit 8/10/10
#include<cstdlib>
#include "Patch.h"
//
using namespace std;

House::House(int hid, double x, double y, double w, double q, int pid, int nh, double irsEffect, double itnEffect, double irsRate, double itnRate) {
    /**
     *
     * @param hid       integer variable used to set House::houseID
     * @param x         double variable used to set House::coordX
     * @param y         double variable used to set House::coordY
     * @param w         double variable used to set House::hMozzyAttractIndex
     * @param q         double variable used to set House::Q
     * @param pid       double variable used to set House::patch using findPatch()
     * @param nh        double variable used to set House::nHumans
     * @param irsEffect double variable used to set House::irsRepellingEffect
     * @param itnEffect double variable used to set House::itnRepellingEffect
     * @param irsRate   double variable used to set House::irsDecayRate
     * @param itnRate   double variable used to set House::itnDecayRate
     */
    houseID = hid;
    coordX = x;
    coordY = y;
    hMozzyAttractIndex = w;
    //patchID = pid;
    patch = findPatch(pid);
    nHumans = nh;
    Q = q;
    dayNewIrs = -10000; // equivalent to no irs initially
    itnRepellingEffect = itnEffect;
    irsRepellingEffect = irsEffect;
    irsDecayRate = irsRate;
    itnDecayRate = itnRate;
    //hMozzySearchEfficiency = (double)rand()/(double)RAND_MAX;
    hMozzySearchEfficiency = 1;
}

House::~House() {
}

void House::printState() {

    cout << "\n" << setw(15) << houseID << setw(15) << coordX << setw(15) << coordY << setw(15) << hMozzyAttractIndex << setw(15) << patch->patchID << setw(15) << nHumans;
    cout << "\n\nHumans in this house:\n";

    //cout<<"\nSIZE"<<humans.size();
    cout << "\n" << setw(15) << "humanID" << setw(15) << "bday" << setw(15) << "dday" << setw(15) << "houseID" << setw(15) << "patchID";
    cout << setw(15) << "w" << setw(15) << "c" << setw(15) << "b" << setw(15) << "itnUse";
    for (int i = 0; i < humans.size(); i++) {
        humans[i]->printState();

    }
}

void House::spray() {
    /**
     * Sets the House::dayNewIrs to current day
     */
    dayNewIrs = currentDay;

}

double House::itnEffect() {
    /**
     * Effectivity of the bednet is calculated using the House::dayNewItn and House::itnDecayRate
     * @return The effectivity of the bednets
     */
    return (1 - itnRepellingEffect * pow(1 - itnDecayRate, currentDay - dayNewItn + 1));
}

double House::irsEffect() {
     /**
     * Effectivity of the spray is calculated using the House::dayNewIrs and House::irsDecayRate
     * @return The effectivity of the bednets
     */
    return (1 - irsRepellingEffect * pow(1 - irsDecayRate, currentDay - dayNewIrs + 1));
}
