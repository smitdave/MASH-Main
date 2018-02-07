/*
 * Patch.cpp
 *
 */

#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <cstdio>
#include "Patch.h"

using namespace std;

Patch::Patch(int pid, double centX, double centY, int nh, int np, int nbor0, int nbor1, int nbor2, int nbor3) {
    /**
     *
     * @param pid   integer variable used to set Patch::patchID
     * @param centX double variable used to set Patch::centroidX
     * @param centY double variable used to set Patch::centroidY
     * @param nh    integer variable used to set Patch::nHouses
     * @param np    integer variable used to set Patch::nPonds
     * @param nbor0 integer variable used to set Patch::neighborID[0]
     * @param nbor1 integer variable used to set Patch::neighborID[1]
     * @param nbor2 integer variable used to set Patch::neighborID[2]
     * @param nbor3 integer variable used to set Patch::neighborID[3]
     */
	patchID = pid;
	centroidX = centX;
	centroidY = centY;
	nHouses = nh;
	nPonds = np;
	//cout<<"\nconstructing patch: "<<patchID<<"\n";
	/*int size = sizeof(nbor) / sizeof(int);
	for (int i=0; i<size; i++)
		neighborID[i] = nbor[i];*/

	neighborID[0] = nbor0;
	neighborID[1] = nbor1;
	neighborID[2] = nbor2;
	neighborID[3] = nbor3;
	//cout<<"\nconstructED patch: "<<patchID<<"\n";
}

Patch::~Patch() {
}

void Patch::printState() {
	cout<<"\n\nPatch:";
	cout<<"\n"<<setw(15)<<"patchID"<<setw(15)<<"centroidX"<<setw(15)<<"centroidY"<<setw(15)<<"nHouses"<<setw(15)<<"nPonds";
	cout<<"\n-------------------------------------------------------------------------------------------";
	cout<<"\n"<<setw(15)<<patchID<<setw(15)<<centroidX<<setw(15)<<centroidY<<setw(15)<<nHouses<<setw(15)<<nPonds;
	cout<<"\n\nHouses in this Patch: \n";

	cout<<"\n"<<setw(15)<<"houseID"<<setw(15)<<"coordX"<<setw(15)<<"coordY"<<setw(15)<<"w"<<setw(15)<<"patchID"<<setw(15)<<"nHumans";
	cout<<"\n-------------------------------------------------------------------------------------------";
	for(int i=0; i<houses.size(); i++) 
		houses[i]->printState();
	cout<<"\n\n Ponds in this Patch: \n";

	cout<<"\n"<<setw(15)<<"pondID"<<setw(15)<<"coordX"<<setw(15)<<"coordY"<<setw(15)<<"patchID"<<setw(15)<<"w"<<setw(15)<<"lambda";
	cout<<"\n-------------------------------------------------------------------------------------------";
	for(int i=0; i<ponds.size(); i++) 
		ponds[i]->printState();
	char a = getchar();
	
}

void Patch::addHouse(House *h){
    /**
     *
     * @param h House* variable used to contain reference to the House which is addded to the Patch
     * @see addPond()
     */
	houses.push_back(h);
}

void Patch::addPond(Pond *p) {
    /**
     *
     * @param p Pond* variable used to contain reference to the Pond which is addded to the Patch
     * @see addHouse()
     */
	ponds.push_back(p);
}


House* Patch::findHouse(int hid) {
    /**
     *
     * @param hid integer variable used to contain House::houseID
     * @return House* variable containing the reference to House with House::HouseID = hid
     */
	for (int i=0; i<houses.size(); i++) {
		if (houses[i]->houseID == hid) { 
			return houses[i];
		}
	}
	cout<<"Error! House not found";
	exit(1);
}

Pond* Patch::findPond(int id) {
     /**
     *
     * @param id integer variable used to contain Pond::pondID
     * @return Pond* variable containing the reference to Pond with Pond::pondID = id
     */
	for (int i=0; i<ponds.size(); i++) {
		if (ponds[i]->pondID == id) { 
			return ponds[i];
		}
	}
	cout<<"Error! Pond not found";
	exit(1);
}

void Patch::setInitialInfection(double percentInf) {
    for (int i=0; i<houses.size(); i++) {
		for (int j=0; j<houses[i]->humans.size(); j++) {
            if (this == houses[i]->humans[j]->patch) {
                double unifRand = (double) rand() / (double) RAND_MAX;
                if (unifRand < percentInf) {
                    //cout<<"\npatch:"<<patchID<<" human:"<<humans[j]->getHumanID()<<" unifRand:"<<unifRand<<" percentInf:"<<percentInf;
                    //cout<<"\n+++++++++++setting human is Inf in patch: "<<patchID;
                    houses[i]->humans[j]->setInitialInf();
                }
            }
		}
	}
}

int Patch::getTotalHumans() {
    /**
     *
     * @return total number of Humans in the Patch
     */
	int totHumans=0;
	for (int i=0; i<houses.size(); i++) {
		totHumans = totHumans + houses[i]->nHumans;
	}
	return totHumans;
}

void Patch::massVaccinate(double vaccineCoverage, double peUpTake, double tbUpTake) {
    /**
     *
     * @param vaccineCoverage double variable containing fraction of Houses for mda coverage
     * @param peUpTake
     * @param tbUpTake
     * @see massDrugAdministrate()
     */
	for (int i=0; i<houses.size(); i++) {
		for (int j=0; j<houses[i]->humans.size(); j++) {
			double unifRand = (double)rand()/(double)RAND_MAX;
			if (unifRand < vaccineCoverage) {
				houses[i]->humans[j]->preErythrocytic(peUpTake);
				houses[i]->humans[j]->transBlocking(tbUpTake);
			}
		}
	}
}

void Patch::massDrugAdministrate(double mdaCoverage) {
    /**
     *
     * @param mdaCoverage double variable containing fraction of Houses for mda coverage
     * @see massVaccinate()
     */
	for (int i=0; i<houses.size(); i++) {
		for (int j=0; j<houses[i]->humans.size(); j++) {
			double unifRand = (double)rand()/(double)RAND_MAX;
			if (unifRand < mdaCoverage) {
				houses[i]->humans[j]->drugTreatment();
			}
		}
	}
}

void Patch::sprayHouses(double irsCoverage) {
    /**
     *
     * @param irsCoverage double variable contains the fraction of Houses to be sprayed
     */
	for (int i=0; i<houses.size(); i++) {
		double unifRand = (double)rand()/(double)RAND_MAX;
		if (unifRand < irsCoverage) {
			houses[i]->spray();
		}
	}
}

void Patch::distributeNets(double itnCoverage) {
    /**
     *
     * @param itnCoverage double variable contains the fraction of Houses for betnets distribution
     */
	for (int i=0; i<houses.size(); i++) {
		for (int j=0; j<houses[i]->humans.size(); j++) {
			double unifRand = (double)rand()/(double)RAND_MAX;
			if (unifRand < itnCoverage) {
				houses[i]->dayNewItn = currentDay;
				houses[i]->humans[j]->receiveNet();
			}
		}
	}
}

int Patch::randomHouseIndex() {
    /**
     *
     * @return an integer containing a random House index
     */
    double unifRand = (double)rand()/(double)RAND_MAX;
    return (int)(houses.size()*unifRand);
}
/*
House* Patch::findHouse(int hid) {
	for (int i=0; i<houses.size(); i++) {
		if (houses[i].houseID == hid) { 
			return &houses[i];
		}
	}
	cout<<"Error! House not found<<";
	exit(1);
}
*/
