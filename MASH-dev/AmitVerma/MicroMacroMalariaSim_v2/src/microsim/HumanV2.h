/* 
 * File:   HumanV2.h
 * Author: amit
 * 
 * This Human class pertains to Humans with PF object model
 */

#ifndef HUMANV2_H
#define	HUMANV2_H

#include "GlobalParameters.h"
#include "PF.h"
#include "RXItem.h"
#include "Human.h"
#include "Drug.h"


///A class to hold information about humans. This Human class pertains to Humans with PF object model.

class HumanV2 : public Human {
public:
    House* routineHouse;
    Patch* routinePatch;

    /// probability of getting treatement
    double pTreat;
    int dayNewItn; // age of the bednet
    double itnEfficiency;
    double atRisk; // proportion of time at risk
    bool isInfected;
    bool hasFever;
    bool underDrugTreatment;
    vector<PF*> pfInfections;
    vector<RXItem*> treatment;
    unsigned a = 1;
    unsigned b = 7;
    // ------PF Epidemiology------- 
    // Pf Infection Status 
    unsigned P = 0; // Infected RBCs 
    unsigned G = 0; // Mature Gametocytes 
    double PD = 0; // Drug Kill 
    double G_PD = 1;
    double IG_PD[10] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
    unsigned MOI = 0; // Number of active clones
    // Immune Counters
    unsigned dF = 0; // days with continuous Fever 
    unsigned dI = 0; // cumulative days with Infection (lifetime)
    unsigned dP = 0; // cumulative days with continuous Infection 
    unsigned nM = 0; // number of Malaria Fevers 
    unsigned thresh = 10000;
    // PF Immunity
    vector<unsigned*> pHist;
    vector<unsigned*> gHist;
    vector<double*> pDHist;
    vector<unsigned*> mOIHist;
    vector<double*> cHist;
    vector<bool*> fHist;


    void dailyDynamics();
    void dailyP();
    void dailyG();
    void dailyPFImmunity();
    void dailyFever();
    void dailyEPI();
    void setBS_PD();
    void setG_PD();
    void setIG_PD();
    void dailyMOI();
    void setInfectivity();
    void receiveNet();
    void receiveInfection(int);
    void receiveInfection();
    void renew();
    void printState();
    void receiveDrug(const Drug&);
    void preErythrocytic(double peUpTake);
    void transBlocking(double tbUpTake);
    void updateMalariaAge();
    bool drugTreatment();
    void setInitialInf();
    void calculateItnEff();
    void setHasFever(bool);
    int getStartFDay();
    int getClearFDay();
    int getHasItn();
    double getItnUse();
    double getItnMortality();
    double getItnRepelling();
    int getItnAge();
    int getDDay();
    int getAge();
    double getInfectivity();
    double getRateGetInfected();
    double getBiteWeight();
    bool getIsInfected();
    HumanV2(int, int, int, int, int, double, double, double, double, bool, double, double, bool, bool, int, int, int, int, int, double, int, int);
    virtual ~HumanV2();

private:
    double rateGetInfected_;
};

#endif	/* HUMANV2_H */

