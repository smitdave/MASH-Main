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

    // Birthday & Death day
    /// birthday
    int bday;
    /// deathday
    int dday;

    // Location
    //int     houseID;              // House
    //int     patchID;

    /// House of residence
    House* house;
    /// Patch of residence
    /** This Patch contains Human's House of residence*/
    Patch* patch;

    // Routine Location
    //  int     routineHID;
    //  int     routinePID;

    /// routine house
    /** This is a House other than Human's fixed house where he/she spends 15% of the time due to some routine*/
    House* routineHouse;
    /// routine Patch
    /** This Patch contains Human's routine Pouse*/
    Patch* routinePatch;

    // Individual Parameters
    /// w (omega)
    double biteWeight; // w: Individual Biting Weight
    /// cMax
    /** Maximum infectivity*/
    double maxInfectivity;
    /// c
    /** Rate of infecting Mosquito given human is infected. Currently this variable can have only one of the two values {0, cMax}*/
    double infectivity;
    ///b
    /** Rate of getting Infection given mosquito is infected*/
    double rateGetInfected;

    /// probability of getting treatement
    double pTreat;

    int incubationDays; //should go in infection
    int latencyDays; //should go in infection
    int mUntreatedInfDays;
    int drugProphylaxisDays;
    int drugDaysToClearI;

    /// true if owns a bednet
    bool itn;
    /// propensity to use a bednet
    double itnUse;
    /// age of the bednet
    int dayNewItn;
    double itnEfficiency;

    /// proportion of time at risk
    double atRisk;
    //bool tmAtRisk ;


    /// probability of foreign travel
    double propensityTravelAbroad;

    /// infection start day
    int startIDay;
    /// infection clear day
    int clearIDay;
    /// fever start day
    int startFDay; //start Fever Day
    int clearFDay; //clear Fever Day
    int drugTEffectStartDay;
    int drugTEffectEndDay;

    /// true if infected
    bool isInfected;
    bool hasFever;
    bool underDrugTreatment;

    /// malaria age
    /** Total number of days when the human was infected*/
    int malariaAge;
    /// Total number of bites
    int nBites;
    /// Total number of infectious bites
    int nInfectiousBites;

    /// constructor
    Human(int, int, int, int, int, double, double, double, double, bool, double, double, bool, bool, int, int, int, int, int, double, int, int);

    ///default destructor
    ~Human();


    /// sets itn = true
    /** this function is used to give itn to a human
     * \see itn
     */
    void getNet();
    ///resets b value to 0.
    /**
     * @see transBlocking()
     */
    void preErythrocytic(double peUpTake);
    ///resets c value to 0
    /**
     * @see preErythrocytic()
     */
    void transBlocking(double tdUpTake);
    /// pre-calculates the fever dates and finds whether human would get infection or not
    /**
     * @see feverMalariaAge()
     */
    void updateMalariaAge();
    /// takes care of infection and super-infection by setting values of c, startI, and clearI
    void getInfection();
    /// resets the c value to 0 for a future day
    void drugTreatment();
    /// returns days before catching fever after the bite from and infected Mosquito
    int getIncubationDays();
    /// returns days before getting infected after the bite from and infected mozzy
    int getLatencyDays();
    /// resets the value of human after human dies
    void renew();
    /**
     * @see updateMalariaAge();
     */
    void feverMalariaAge();
    /// prints state of human
    bool getTreatment();
    void printState();
    void setInitialInf();
    void calculateItnEff();
    int getDrugTEffectStartDay();
    void setDrugTEffectStartDay(int);
    int getDrugTEffectEndDay();
    void setDrugTEffectEndDay(int);
    void setUnderDrugTreatment(bool);
    void setHasFever(bool);
    int getStartFDay();
    int getClearFDay();
    void setClearIDay(int);
    int getClearIDay();
    int getHasItn();
    double getItnUse();
    double getItnMortality();
    double getItnRepelling();
    int getItnAge();
    int getStartIDay();
    void setStartIDay(int);
    int getDDay();
    int getAge();
    double getInfectivity();
    double getRateGetInfected();
    double getMaxInfectivity();
    void setInfectivity(double);
    double getBiteWeight();
    bool getIsInfected();
    void setIsInfected(bool);
};

#endif /* HUMAN_H_ */
