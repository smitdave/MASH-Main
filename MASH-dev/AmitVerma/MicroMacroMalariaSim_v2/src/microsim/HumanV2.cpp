/* 
 * File:   HumanV2.cpp
 * Author: amit
 * 
 * 
 * This Human class pertains to Humans with PF object model
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include <cstdlib>
#include "HumanV2.h"
#include "Patch.h"
#include "House.h"

void HumanV2::dailyDynamics() {
    if (currentDay == dday) {
        renew();
    }
    calculateItnEff();
    setBS_PD();
    setG_PD();
    setIG_PD();
    dailyP();
    dailyG();
    dailyPFImmunity();
    dailyFever();
    setInfectivity();
    dailyEPI();
}

void HumanV2::setInfectivity() {
    //Infection PARAMETERS 
    infectivity = (double) exp(a * G) / (exp(a * b) + exp(a * G));
}

void HumanV2::dailyP() {
    long sum = 0;
    for (auto &pf : pfInfections) {
        sum += pf->updateP(PD);
        if (pf->isActive()) isInfected = true;
    }
    P = sum;
}

void HumanV2::dailyG() {
    double sum = 0;
    for (auto &pf : pfInfections) {
        double surv = 0.75;
        sum += pf->updateG(G_PD, IG_PD, surv);
    }
    G = sum;
}

void HumanV2::setBS_PD() {
    //###############################################
    //# If more than one drug is in the system, get 
    //# the right PD kill rate for merozoites
    //###############################################
    double pk = 1;
    for (auto &rx : treatment) {
        if (currentDay < rx->endD) {
            int dd = currentDay - rx->startD + 1;
            pk = min(rx->drug.getPKill(dd), pk);
        }
    }
    PD = pk;
}

void HumanV2::setG_PD() {
    //###############################################
    //# If more than one drug is in the system, get 
    //# the right PD kill rate for merozoites
    //###############################################
    double gk = 1;
    for (auto &rx : treatment) {
        if (currentDay < rx->endD) {
            int dd = currentDay - rx->startD + 1;
            gk = rx->drug.getGKill(dd) * gk;
        }
    }
    G_PD = gk;
}

void HumanV2::setIG_PD() {//return array
    //###############################################
    //# If more than one drug is in the system, get 
    //# the right PD kill rate for merozoites
    //###############################################
    double igk[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
    for (auto &rx : treatment) {
        if (currentDay < rx->endD) {
            int dd = currentDay - rx->startD + 1;
            for (int i = 0; i < 10; i++) {
                igk[i] = min(rx->drug.getIGKill(i, dd), igk[i]);
            }
        }
    }
    for (int i = 0; i < 10; i++) {
        IG_PD[i] = igk[i];
    }
}

void HumanV2::dailyPFImmunity() {
    pHist.push_back(new unsigned(P));
    gHist.push_back(new unsigned(G));
    pDHist.push_back(new double(PD));
    mOIHist.push_back(new unsigned(MOI));
    cHist.push_back(new double(infectivity));
    fHist.push_back(new bool(hasFever));

}

void HumanV2::dailyFever() {
    hasFever = false;
    if (P > thresh) {
        hasFever = true;
    }
    if (hasFever) {
        dF += 1;
        nM += 1;
    }
    if (dF > 14) {
        hasFever = false;
    }
    if (!hasFever) {
        dF = 0;
    }
}

void HumanV2::dailyEPI() {
    dailyMOI();

    if (MOI > 0) {
        dI = dI + 1;
        dP = dP + 1;
    } else {
        dP = 0;
    }
}

void HumanV2::dailyMOI() {
    MOI = 0;
    for (auto &pf : pfInfections) {
        if (pf->isActive()) MOI++;
    }
}

void HumanV2::receiveInfection() {
    receiveInfection(currentDay);
}

void HumanV2::receiveInfection(int biteDay) {
    PF *pf = new PF(biteDay);
    pfInfections.push_back(pf);
}

void HumanV2::receiveNet() {
    itn = true;
    dayNewItn = currentDay;
}

void HumanV2::preErythrocytic(double peUpTake) {
    /**
     * @param peUpTake
     */
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (unifRand < peUpTake) {
        rateGetInfected = 0;
    }
}

void HumanV2::transBlocking(double tbUpTake) {
    /**
     *
     * @param tbUpTake
     */
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (unifRand < tbUpTake) {
        b = UINT_MAX; // does this happen instantaneously?
    }
}

void HumanV2::renew() {
    /**
     * Sets HumanV2::bday to current day and HumanV2::dday to a random number > 85*365. Clears the rest of Human variables.
     */
    bday = currentDay;
    double unifRand = (double) rand() / (double) RAND_MAX;

    int temp1 = (int) (-(50 * 365) * log(unifRand));
    int temp2 = 85 * 365;
    if (temp1 > temp2)
        dday = currentDay + temp1;
    else
        dday = currentDay + temp2;
    nBites = 0;
    nInfectiousBites = 0;
    infectivity = 0.0;
    isInfected = false;
    hasFever = false;
    underDrugTreatment = false;
    rateGetInfected = rateGetInfected_;
    a = 1;
    b = 7;
    pHist.clear();
    gHist.clear();
    pDHist.clear();
    mOIHist.clear();
    cHist.clear();
    fHist.clear();
    pfInfections.clear();
    treatment.clear();
}

bool HumanV2::drugTreatment() {
    receiveDrug(useDrug);
    return true;
}

void HumanV2::receiveDrug(const Drug& drug) {
    RXItem *rx = new RXItem(drug, currentDay, currentDay + drug.getTMax());
    treatment.push_back(rx);
}

void HumanV2::setInitialInf() {
    PF *pf = new PF(-6);
    pfInfections.push_back(pf);
}

double HumanV2::getInfectivity() {
    return infectivity;
}

double HumanV2::getBiteWeight() {
    return biteWeight;
}

bool HumanV2::getIsInfected() {
    return isInfected;
}

int HumanV2::getDDay() {
    return dday;
}

int HumanV2::getHasItn() {
    if (itn)
        return 1;
    else
        return 0;
}

double HumanV2::getItnMortality() {
    double d0 = 0.0;
    double d1 = 0.2;
    double d = d0 + (d1 - d0) * itnEfficiency;
    return d;
}

double HumanV2::getItnRepelling() {
    double s0 = 0.95;
    double s1 = 0.3;
    double s = s1 + (s0 - s1) * (1 - itnEfficiency);
    return s;
}

void HumanV2::calculateItnEff() {
    itnEfficiency = exp(-1.0 * pow((((double) currentDay - dayNewItn) / (365 / 2)), 2));
}

void HumanV2::printState() {
    cout << "\n" << setw(15) << humanID << setw(15) << bday << setw(15) << dday << setw(15) << house->houseID << setw(15) << patch->patchID;
    cout << setw(15) << biteWeight << setw(15) << infectivity << setw(15) << rateGetInfected << setw(15) << itnUse;
}

HumanV2::HumanV2(int ID, int bd, int dd, int H, int P, double w, double cmax,
        double b_, double ptr, bool it, double iUse, double itnA, bool isInf, bool hf,
        int rPID, int rHID, int mUinfD, int incd, int latcyd, double prTrAbroad, int drugP, int drugC) {

    isInfected = false;
    humanID = ID;
    bday = bd;
    dday = dd;
    patch = findPatch(P);
    house = patch->findHouse(H);
    routinePatch = findPatch(rPID);
    routineHouse = routinePatch->findHouse(rHID);
    propensityTravelAbroad = prTrAbroad;
    biteWeight = w;
    rateGetInfected = b_;
    rateGetInfected_ = b_;
    itn = it;
    itnUse = iUse;
    dayNewItn = -(int) itnA;
    infectivity = 0.0;
    hasFever = hf;
    underDrugTreatment = false;
    if (isInf) {
        setInitialInf();
    }
    nBites = 0;
    nInfectiousBites = 0;
    pTreat = ptr;
    calculateItnEff();
}

HumanV2::~HumanV2() {
}

