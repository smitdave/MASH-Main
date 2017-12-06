/*
 * HealthClinic.cpp
 *
 *  Created on: Apr 17, 2012
 *      Author: amit.verma
 */

#include "HealthClinic.h"

void HealthClinic::humanVisit(Human* human) {
    if (human->hasFever) {
        dailyFevers++;
    }
    
    dailyTested++;
    clinicLog << "\n" << currentDay << " " << homePatchID << " 0 " << human->humanID << " " << human->house->houseID << " " << human->isInfected;
    if (testHuman(human)) {
        dailyPositive++;
        clinicLog << " 1";
        if (assignHumanRightDrug(human)) {
            dailyTreated++;
            human->drugTreatment();
            clinicLog << " 1";
        } else {
            clinicLog << " 0";
        }

        reportHuman(human);
    } else {
        clinicLog << " 0";
    }
}

int HealthClinic::getDailyTreated() {
    return dailyTreated;
}

int HealthClinic::getDailyFevers() {
    return dailyFevers;
}

int HealthClinic::getDailyTested() {
    return dailyTested;
}

int HealthClinic::getDailyPositive() {
    return dailyPositive;
}

void HealthClinic::resetDailyOutputs() {
    dailyFevers = 0;
    dailyTested = 0;
    dailyPositive = 0;
    dailyTreated = 0;
}

bool HealthClinic::assignHumanRightDrug(Human* human) {
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (unifRand < drugAssignmentAccuracy) {
        return true;
    }
    return false;
}

void HealthClinic::reactiveCaseDetection() {
    int last = rcdCapacity;
    if (rcdCapacity > rcdHouseQueue.size())
        last = rcdHouseQueue.size();
    rcdHouseQueue.erase(rcdHouseQueue.begin(), rcdHouseQueue.begin() + last - 1);
}

void HealthClinic::reportHuman(Human* human) {
    //write in a file
    if (caseInvestigation(human)) { //traveled abroad
        clinicLog << " 1";
        setAlert(1);
    } else {
        clinicLog << " 0";
        double unifRand = (double) rand() / (double) RAND_MAX;
        if (unifRand < acdResolution) {
            House * hs = human->house;
            addToRCDList(hs);
            //for (int i = 0; i <= rcdCoverage; i++) {    //fix this
            if(acdResolution==2) {
                int i=0;
                while(hs->nearestHouses[i]->d < rcdRadius) {
                    addToRCDList(hs->nearestHouses[i]->loc->house);
                    i += 1;
                }
            }
            if(acdResolution==1) {
                for(int i=0;i<hs->patch->nHouses;i++) {
                    addToRCDList(hs->patch->houses[i]);
                }
                
            }
        }
    }
}

bool HealthClinic::checkRCDList(House * hs) {
    for (int i = 0; i < rcdHouseQueue.size(); i++) {
        if (rcdHouseQueue[i] == hs)
            return true;
    }
    return false;
}

void HealthClinic::addToRCDList(House *hs) {
    if (!checkRCDList(hs)) {
        rcdHouseQueue.push_back(hs);
        clinicLog << " " << hs->houseID;
    }
}

bool HealthClinic::caseInvestigation(Human *human) {//whether traveled abroad
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (unifRand < human->propensityTravelAbroad) {
        dayLastForeignCase = currentDay;                                                                           
        return true;
    }
    return false;
}

void HealthClinic::vectorControl() {
    clinicLog << "\n" << currentDay << " " << homePatchID << " 1";
    for (int i = 0; i < patch->houses.size(); i++) {
        patch->houses[i]->spray();
    }
}

bool HealthClinic::testHuman(Human* human) {
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (human->isInfected) {
        if (unifRand < testAccuracyTP) {
            //human tested positive
            return true;
        }
    } else {
        if (unifRand < (1-testAccuracyTN)) {
            //human tested positive
            return true;
        }
    }

    return false;
}

void HealthClinic::setAlert(int al) { // (for later) define alert levels using macros
    alert = al;
}

HealthClinic::HealthClinic(Patch * p, double tp, double fn, double daa, int cap, int rad, int res, double trigprob) {
    alert = 0;
    patch = p;
    homePatchID = patch->patchID;
    dayLastForeignCase = 30; //?
    testAccuracyTP = tp;
    testAccuracyTN = fn;
    drugAssignmentAccuracy = daa;
    rcdCapacity = cap;
    //swampedTh = rcdCapacity * 5;
    //rcdCoverage = rcov;
    rcdRadius = rad;
    acdResolution = res;
    acdProbTrigger = trigprob;
    
    resetDailyOutputs();
}

HealthClinic::~HealthClinic() {
    // TODO Auto-generated destructor stub
}


