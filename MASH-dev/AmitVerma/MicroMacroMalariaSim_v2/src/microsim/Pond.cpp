/*
 * Pond.cpp
 *
 */

#include <iostream>
#include <iomanip>
#include<cstdlib>
#include "Pond.h"
#include "Patch.h"

using namespace std;

Pond::Pond(int poid, double x, double y, int pid, double w, double lmda) {
    /**
     *
     * @param poid  variable not used
     * @param x     double variable used to set Pond::coordX
     * @param y     double variable used to set Pond::coordY
     * @param pid   double variable used to set Pond::patch using findPatch()
     * @param w     double variable used to set Pond::pMozzyAttractIndex
     * @param lmda  double variable used to set Pond::lambda
     */
    pondID = poid;

    coordX = x;
    coordY = y;
    //patchID = pid;
    patch = findPatch(pid);
    pMozzyAttractIndex = w;
    lambda = lmda;
    //pMozzySearchEfficiency = (double)rand()/(double)RAND_MAX;
    pMozzySearchEfficiency = 1;
}

Pond::~Pond() {
}

void Pond::printState() {
    cout << "\n" << setw(15) << pondID << setw(15) << coordX << setw(15) << coordY << setw(15) << patch->patchID << setw(15) << pMozzyAttractIndex << setw(15) << lambda;
}
