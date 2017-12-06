/*
 * HealthClinic.h
 *
 *  Created on: Apr 17, 2012
 *      Author: amit.verma
 */

#ifndef HEALTHCLINIC_H_
#define HEALTHCLINIC_H_

#include "GlobalParameters.h"
#include "Human.h"
#include "HumanV2.h"
#include "House.h"
#include "Patch.h"

//class Human;
//class Patch;
//class House;

class HealthClinic {
public:
    int dailyTested;
    int dailyPositive;
    int dailyFevers;
    int dailyTreated;
    int homePatchID;
    Patch *patch;
    int alert; //(1=high, 0=low)
    int dayLastForeignCase;
    double testAccuracyTP; //default 80%
    double testAccuracyTN; //default 2%
    double drugAssignmentAccuracy; //default 100%
    double acdProbTrigger;
    //int swampedTh;
    int rcdCapacity;
    //int rcdCoverage;//no. of houses nearby;
    double rcdRadius;
    int acdResolution;
    vector <House*> rcdHouseQueue;
    bool caseInvestigation(Human*);
    void reactiveCaseDetection();
    void vectorControl();
    void setAlert(int);
    bool testHuman(Human*);
    bool assignHumanRightDrug(Human*);
    void reportHuman(Human*);
    bool checkRCDList(House*);
    void addToRCDList(House*);
    void humanVisit(Human*);
    HealthClinic(Patch*, double, double, double, int, int, int, double);
    virtual ~HealthClinic();
    int getDailyTested();
    int getDailyPositive();
    int getDailyFevers();
    int getDailyTreated();
    void resetDailyOutputs();
};

#endif /* HEALTHCLINIC_H_ */
