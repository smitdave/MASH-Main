/*
 * Human.h
 *
 */

#ifndef HUMAN_H_
#define HUMAN_H_

#include "GlobalParameters.h"

class Patch;
class House;
class HealthClinic;

///A class to hold information about humans.

class Human {
public:

    int humanID;
    int bday;
    int dday;
    House* house;
    Patch* patch;
    double biteWeight; // w (omega): Individual Biting Weight
    double infectivity; // c: Rate of infecting Mosquito given human is infected.
    double rateGetInfected; // b: Rate of getting Infection given mosquito is infected
    double propensityTravelAbroad; // probability of foreign travel 
    bool itn; // true if owns a bednet
    double itnUse; // propensity to use a bednet
    bool isInfected;
    bool hasFever;
    bool underDrugTreatment;
    int nBites;
    int nInfectiousBites;
    virtual bool drugTreatment() = 0;
    virtual void renew() = 0;
    virtual void dailyDynamics() = 0;
    virtual void receiveInfection() = 0;
    virtual void printState() = 0;
    virtual void setInitialInf() = 0;
    virtual void receiveNet() = 0;
    virtual void preErythrocytic(double peUpTake) = 0;
    virtual void transBlocking(double tbUpTake) = 0;

};

#endif /* HUMAN_H_ */
