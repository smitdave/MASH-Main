
/* 
 * File:   Human.h
 * Author: amit
 *
 * Created on September 21, 2011, 2:46 PM
 */

#ifndef HUMAN_H
#define	HUMAN_H

#include "Patch.h"
#include "PF.h"
//class Patch;

class Human {
private:

    int humanID;

    // Birthday & Death day
    int bday; // Birth Day
    int dday; // Death Day
    // Routine Location
    int homePatchID;
    Patch* homePatch;

    int numRP;
    std::vector<int>routinePatchIDs;
    std::vector<Patch*> routinePatches;
    std::vector<double> timeSpentRP;

    // Individual Parameters
    double biteWeight; // w: Individual Biting Weight
    double maxInfectivity; // cMax: max infectivity
    double infectivity; // c: Rate of infecting Mosquito given human is infected

    double rateGetInfected; // b: Rate of getting Infection given mosquito is infected

    double pTreat;

    int incubationDays; //should go in infection
    int latencyDays; //should go in infection
    int mUntreatedInfDays;

    bool itn;
    double itnUse; // Propensity to Use an ITN
    int dayNewItn;
    double itnEfficiency;

    bool irs;
    int dayNewIrs;
    double irsEfficiency;

    double atRisk; //bool tmAtRisk ;
    double huExposure;

    double epsilon;
    int startIDay;
    int clearIDay;
    int startFDay; //start Fever Day
    int	clearFDay;//clear Fever Day
	int drugTEffectStartDay;
	int drugTEffectEndDay;

    bool isInfected;//introduce isIfectious in next version. Also need clearInfection function
	bool hasFever;
	bool underDrugTreatment;
	
    int malariaAge;
    int nBites;
    int nInfectiousBites;
    
    int pfP;
    int pfG;
    int pfiG[10];
    std::vector<PF*> pfInfections;

public:
    int getHumanID();
    Patch* getHomePatch();
    int getHomePatchID();
    int getNumRoutinePatches();
    Patch* getRoutinePatch(int);
    double getRPTimeSpent(int);
    void getNet(int); // this function is used to give itn to a human
    void preErythrocytic(double peUpTake); //resets b value to 0
    void transBlocking(double tdUpTake); // resets c value to 0
    void updateMalariaAge(); // pre-calculates the fever dates and finds whether human would get infection or not
    void setInfection(int); // takes care of infection and super-infection by setting values of c, startI, and clearI
    void drugTreatment(int); // resets the c value to 0 for a future day
    int getIncubationDays(); // returns days before catching fever after the bite from and infected mozzy
    int getLatencyDays(); // returns days before getting infected after the bite from and infected mozzy
    void renew(int); // resets the value of human after human dies
    void feverMalariaAge(int); //TD changed name from feverPremunition()
    int getMalariaAge();
    void printState(int); // just prints some variables in human on screen
    void addRoutinePatch(int, Patch*, double);
    void setEpsilon(double);
    double getEpsilon();
    bool getIsInfected();
    void setIsInfected(bool);
	void setInitialInf();
	void setHasFever(bool);
	int getStartFDay();
	int getClearFDay();
	int getHasItn();
	double getItnUse();
	int getHasSprayed();
	void getSpray(int);
	void calculateItnEff(int);
	void calculateIrsEff(int);
	double getItnMortality();
	double getItnRepelling();
	double getIrsMortality();
	int getItnAge(int currDay);
	bool seekTreatment(int);
    int getStartIDay();
    void setStartIDay(int);
	int getDrugTEffectStartDay();
	void setDrugTEffectStartDay(int);
	int getDrugTEffectEndDay();
	void setDrugTEffectEndDay(int);
	void setUnderDrugTreatment(bool);
    int getDDay();
    int getAge(int);
    double getTimeSpentInP(Patch*);
    double getInfectivity();
	double getRateGetInfected();
    void setClearIDay(int);
    int getClearIDay();
    double getMaxInfectivity();
    void setInfectivity(double);
    double getBiteWeight();
    Human(int, int, Patch*, int, int, double, double, double, double, double, bool, double, double, bool, double, int, bool, int, int, int);
    Human();
    Human(const Human& orig);
    virtual ~Human();

};

#endif	/* HUMAN_H */

