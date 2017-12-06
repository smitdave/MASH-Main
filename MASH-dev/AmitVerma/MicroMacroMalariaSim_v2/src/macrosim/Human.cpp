/* 
 * File:   Human.cpp
 * Author: amit
 * 
 * Created on September 21, 2011, 2:46 PM
 */
#include <iostream>
#include <iomanip>
#include <cmath>
#include <cstdlib>

#include "Human.h"

using namespace std;

void Human::addRoutinePatch(int id, Patch* pp, double ts) {
    routinePatchIDs.push_back(id);
    routinePatches.push_back(pp);
    timeSpentRP.push_back(ts);
}

Human::Human(int id, int hid, Patch* pp, int bd, int dd, double ww, double cc, double bb, double atR, double ptr, bool it, double iUse, double itnA, bool ir, double irsA, int nRP, bool isInf, int incd, int latcyd, int mUInfD) {
	isInfected = false;
	if(getIsInfected()){
		cout<<"\nThis is weird "<<isInf<<" "<<isInfected;
	}
	humanID = id;
    bday = bd;
    dday = dd;
    homePatchID = hid;
    homePatch = pp;
    biteWeight = ww;
    maxInfectivity = cc;
    rateGetInfected = bb;
    itn = it;
    itnUse = iUse;
    dayNewItn = -(int)itnA;
    irs = ir;
    dayNewIrs = -(int)irsA;
    infectivity = 0.0;
    numRP = nRP;
    //isInfected = isInf;
	startIDay = -1;
    clearIDay = -1;
    startFDay = -1;
    incubationDays = incd;
    latencyDays = latcyd;
    mUntreatedInfDays = mUInfD;
	drugTEffectStartDay = -1;
	drugTEffectEndDay = -1;
	underDrugTreatment = false;
    if (isInf) {
		setInitialInf();
	}
    malariaAge = 0;
    nBites = 0;
    nInfectiousBites = 0;
    pTreat = ptr;
    atRisk = atR;
	hasFever = false;
	calculateItnEff(0);
	calculateIrsEff(0);
}

void Human::setInitialInf() {
	setIsInfected(true);
	infectivity = maxInfectivity;
	double unifRand = (double) rand() / (double) RAND_MAX;
	clearIDay = getLatencyDays() + (int) (-mUntreatedInfDays * log(unifRand));
}


int Human::getHumanID() {
	return humanID;
}

Patch* Human::getHomePatch() {
	return homePatch;
}

int Human::getHomePatchID() {
	return homePatchID;
}

void Human::getSpray(int currentDay) {
    irs = true;
    dayNewIrs = currentDay;
}

int Human::getNumRoutinePatches() {
    return routinePatches.size();
}

Patch* Human::getRoutinePatch(int i) {
    return routinePatches[i]; //check in range
}

double Human::getRPTimeSpent(int i) {
    timeSpentRP[i];
}

void Human::setEpsilon(double eps) {
    epsilon = eps;
}

double Human::getEpsilon() {
    return epsilon;
}

double Human::getRateGetInfected() {
	return rateGetInfected;
}

void Human::preErythrocytic(double peUpTake) {
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (unifRand < peUpTake) {
        rateGetInfected = 0;
    }
}

void Human::transBlocking(double tbUpTake) {
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (unifRand < tbUpTake) {
        maxInfectivity = 0; // does this happen instantaneously?
    }
}

void Human::updateMalariaAge() {
    if (isInfected) {
        malariaAge = malariaAge + 1;
    }
}

int Human::getMalariaAge() {
	return malariaAge;
}

void Human::drugTreatment(int currentDay) {
	if (clearIDay > currentDay + 12) {
		setUnderDrugTreatment(true);
		setDrugTEffectStartDay(currentDay);
		setDrugTEffectEndDay(currentDay + 30);
		clearIDay = currentDay + 12;
		clearFDay = currentDay + 12;
	}
    //cout << "\n\t++++++++++++++ drug treatment ++++++++++++\n\n";
}

void Human::setUnderDrugTreatment(bool udt) {
	underDrugTreatment = udt;
}

int Human::getDrugTEffectStartDay() {
	return drugTEffectStartDay;
}

int Human::getDrugTEffectEndDay() {
	return drugTEffectEndDay;
}

void Human::setDrugTEffectStartDay(int d) {
	drugTEffectStartDay = d;
}

void Human::setDrugTEffectEndDay(int d) {
	drugTEffectEndDay = d;
}

void Human::setInfection(int currentDay) {
    //cout << "\n\n\t++++++++++++++ in getInfection ++++++++++++";
    double unifRand = (double) rand() / (double) RAND_MAX;
	if(underDrugTreatment) {
		return;
	}
    else if (!isInfected) { //simple infection case
        //cout << "\n\t++++++++++++++ simple infection ++++++++++++";
		if (startIDay < currentDay || startIDay > currentDay + getLatencyDays())
			startIDay = currentDay + getLatencyDays();	
        int clday = currentDay + getLatencyDays() + (int) (-mUntreatedInfDays * log(unifRand));
		if (clearIDay < clday)
			clearIDay = clday;
    } else { // super infection case
        //cout << "\n\t++++++++++++++ super infection ++++++++++++";
        int newClearIDay = currentDay + getLatencyDays() + (int) (-mUntreatedInfDays * log(unifRand));
        if (clearIDay < newClearIDay)
            clearIDay = newClearIDay;
    }

    //cout << "\n\t" << "startIDay: " << startIDay << "\t" << "clearIDay: " << clearIDay;
    feverMalariaAge(currentDay);

    //give clearI to people infected on day=0
}

int Human::getIncubationDays() {
    return incubationDays;
}

void Human::setStartIDay(int sid) {
	startIDay = sid;
}

int Human::getStartIDay() {
    return startIDay;
}

int Human::getStartFDay() {
    return startFDay;
}

int Human::getClearFDay() {
    return clearIDay;
}

void Human::setClearIDay(int cid) {
	clearIDay = cid;
}

int Human::getClearIDay() {
    return clearIDay;
}

int Human::getHasItn() {
	if (itn)
		return 1;
	else
		return 0;
}

double Human::getItnUse() {
	return itnUse;
}

int Human::getHasSprayed() {
	if (irs)
		return 1;
	else
		return 0;
}

void Human::calculateItnEff(int currDay) {
	itnEfficiency = exp(- 1.0 * pow((((double)currDay - dayNewItn)/(365/2)),2));
}

void Human::calculateIrsEff(int currDay) {
	irsEfficiency = exp(- 1.0 * pow((((double)currDay - dayNewIrs)/(365/4)),2));
}

int Human::getItnAge(int currDay) {
	return currDay - dayNewItn;
}


double Human::getItnMortality() {
	double d0 = 0.0;
	double d1 = 0.2;
	double d =  d0 + (d1-d0) * itnEfficiency;
	return d;
}

double Human::getItnRepelling() {
	double s0 = 0.95;
	double s1 = 0.3;
	double s =  s1	 + (s0-s1) * (1 - itnEfficiency);
	return s;
}

double Human::getIrsMortality() {
	double mu1 = 0.5;
	double mu0 = 0;
	double mu = (mu1-mu0) * irsEfficiency;
	return mu;
}

int Human::getLatencyDays() {
    return latencyDays;
}

void Human::getNet(int currentDay) {
    itn = true;
    dayNewItn = currentDay;
}

void Human::renew(int currentDay) {
    bday = currentDay;
    double unifRand = (double) rand() / (double) RAND_MAX;

    int temp1 = (int) (-(50 * 365) * log(unifRand));
    int temp2 = 85 * 365;
    if (temp1 > temp2)
        dday = currentDay + temp1;
    else
        dday = currentDay + temp2;

    startIDay = -1;
    clearIDay = -1;
    startFDay = -1;
    nBites = 0;
    nInfectiousBites = 0;
    infectivity = 0.0;
    isInfected = false;
    hasFever = false;
    malariaAge = 0;
	drugTEffectStartDay = -1;
	drugTEffectEndDay = -1;
	underDrugTreatment = false;



/*
    homePatch = pp;
    biteWeight = ww;
    maxInfectivity = cc;
    rateGetInfected = bb;
    itn = it;
    itnUse = iUse;
    dayNewItn = -(int)itnA;
    irs = ir;
    dayNewIrs = -(int)irsA;
    nInfectiousBites = 0;
    infectivity = 0.0;
    numRP = nRP;
    pTreat = ptr;
    atRisk = atR;
	hasFever = false;
	calculateItnEff(0);
	calculateIrsEff(0);
  */
}

void Human::feverMalariaAge(int currentDay) {
    double unifRand = (double) rand() / (double) RAND_MAX;
    //double probFever = exp(-0.4 * pow(malariaAge / 1000.0, 4));
    double probFever = 0.8 * exp(-1.0 * pow(malariaAge / 1000.0, 3));
    if (probFever < 0.01)
        probFever = 0.01;
    if (unifRand < probFever) {
        startFDay = currentDay + getIncubationDays();
    }
}

bool Human::seekTreatment(int currentDay) {
	    double unifRand = (double) rand() / (double) RAND_MAX;
        if (unifRand < pTreat) {
            drugTreatment(currentDay);
            return true;
        }
		else  {
			clearFDay = currentDay + 12;
			return false;
		}
}

void Human::setHasFever(bool b) {
	hasFever = b;
}

void Human::setInfectivity(double infty) {
    infectivity = infty;
}

double Human::getInfectivity() {
    return infectivity;
}

double Human::getMaxInfectivity() {
    return maxInfectivity;
}

double Human::getBiteWeight() {
    return biteWeight;
}

bool Human::getIsInfected() {
    return isInfected;
}

void Human::setIsInfected(bool inf) {
    isInfected = inf;
}

int Human::getDDay() {
    return dday;
}

void Human::printState(int currentDay) {
    cout << "\n" << setw(15) << humanID << setw(10) << homePatch->getPatchID() << setw(10) << bday << setw(10) << dday << setw(10) << getAge(currentDay);
    cout << setw(15) << biteWeight << setw(15) << maxInfectivity << setw(15) << rateGetInfected << setw(15) << atRisk;
    cout << setw(10) << pTreat << setw(5) << itn << setw(15) << itnUse << setw(15) << itnEfficiency << setw(8) << isInfected << setw(8) << numRP << setw(15);

    for (int i = 0; i < numRP; i++) {
        cout << " [" << routinePatchIDs[i] << ", " << timeSpentRP[i] << "]";
    }

}

int Human::getAge(int currentDay) {
    return (currentDay - bday) / 365;
}

double Human::getTimeSpentInP(Patch * pp) {
    for (int i = 0; i < routinePatches.size(); i++) {
        if (pp == routinePatches[i]) {
            return timeSpentRP[i];
        }
    }
	return -1.0;
}

Human::Human() {
}

Human::Human(const Human& orig) {
}

Human::~Human() {
}

