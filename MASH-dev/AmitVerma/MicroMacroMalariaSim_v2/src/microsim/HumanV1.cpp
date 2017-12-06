/*
 * Human.cpp
 *
 */


#include <iostream>
#include <iomanip>
#include <cmath>
#include <cstdlib>
#include "Human.h"
#include "Patch.h"
#include "House.h"


using namespace std;

Human::Human(int ID, int bd, int dd, int H, int P, double w, double cmax, double b, double ptr, bool it, double iUse, double itnA, bool isInf, bool hf, int rPID, int rHID, int mUinfD, int incd, int latcyd, double prTrAbroad, int drugP, int drugC) {
    /**
     *
     * @param ID    integer variable used to set Human::humanID
     * @param bd    integer variable used to set Human::bday
     * @param dd    integer variable used to set Human::dday
     * @param H     integer variable used to set Human::house using findHouse()
     * @param P     integer variable used to set Human::patch using findPatch()
     * @param w     double variable used to set Human::biteWeight
     * @param cmax  double variable used to set Human::maxInfectivity
     * @param b     double variable used to set Human::rateGetInfected
     * @param ptr   double variable used to set Human::pTreat
     * @param it    bool variable used to set Human::itn
     * @param iUse  double variable used to set Human::itnUse
     * @param itnA  double variable used to set Human::itnAge
     * @param isInf bool variable used to set Human::isInfected
     * @param hf 	bool variable used to set Human::hasFever
     * @param rPID  integer variable used to set Human::routinePatch using findPatch()
     * @param rHID  integer variable used to set Human::routineHouse using findHouse()
     * @see findPatch(), Patch::findHouse()
     */
    isInfected = false;
    humanID = ID;
    bday = bd;
    dday = dd;
    //houseID = H;
    //patchID = P;
    //routinePID = rPID;
    //routineHID = rHID;
    patch = findPatch(P);
    house = patch->findHouse(H);
    routinePatch = findPatch(rPID);
    routineHouse = routinePatch->findHouse(rHID);
    propensityTravelAbroad = prTrAbroad;
    biteWeight = w;
    maxInfectivity = cmax;
    rateGetInfected = b;
    incubationDays = incd;
    latencyDays = latcyd;
    mUntreatedInfDays = mUinfD;
    itn = it;
    itnUse = iUse;
    dayNewItn = -(int) itnA;
    infectivity = 0.0;
    hasFever = hf;
    startIDay = -1;
    clearIDay = -1;
    startFDay = -1;
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
    calculateItnEff();
    drugProphylaxisDays = drugP;
    drugDaysToClearI = drugC;
}

Human::~Human() {
}

void Human::dailyDynamics() {
    if (currentDay == startIDay) {
        isInfected = true;
        infectivity = maxInfectivity;

        //cout<<"\n\n\n\n\t==========================\n\tFound INFECTED Human\n\t==========================\n\n\n";
    }
    if (currentDay == clearIDay) {
        isInfected = false;
        setStartIDay(-1);
        infectivity = 0.0;
    }
    if (currentDay == getDrugTEffectEndDay()) {
        //cout<<"------------------------OUT_OF_DRUG--------------------------";
        setUnderDrugTreatment(false);
        setDrugTEffectStartDay(-1);
        setDrugTEffectEndDay(-1);
    }

    if (currentDay == startFDay) {
        //newFevers[getHomePatchID()-1]++;
        hasFever = true;
        if (getTreatment()) {
            //    newTreatedHs[getHomePatchID()-1]++;
        }
    }
    if (currentDay == clearFDay) {
        hasFever = false;
    }
    if (currentDay == dday) {
        renew();
    }
    updateMalariaAge();
    calculateItnEff();
}

void HumanV1::receiveBite(Mosquito& m) {
    hs->humans[i]->nBites++;
    //cout<<"\n\n\n\n++*+*+*+*+*+*"<<hs.humans[i].nBites<<"+*+*+*+**++*+*+*+\n\n\n\n";
    if (isInfected) { // is mozzy infected
        hs->humans[i]->nInfectiousBites++;
        if (hs->humans[i]->isInfected)
            mihi++;
        else mihu++;
        unifRand = (double) rand() / (double) RAND_MAX;
        if (unifRand < hs->humans[i]->rateGetInfected) {
            hs->humans[i]->getInfection();
            biteLog << 1;
        } else biteLog << 0;
    } else {
        if (hs->humans[i]->isInfected) {
            muhi++;
            unifRand = (double) rand() / (double) RAND_MAX;
            if (unifRand < hs->humans[i]->infectivity) {
                infectionStartDay = currentDay + sporogonyDays;
                muhi_it++;
                biteLog << 2;
            } else biteLog << 0;
        } else {
            muhu++;
            biteLog << 0;
        }
    }
}

void Human::printState() {
    cout << "\n" << setw(15) << humanID << setw(15) << bday << setw(15) << dday << setw(15) << house->houseID << setw(15) << patch->patchID;
    cout << setw(15) << biteWeight << setw(15) << infectivity << setw(15) << rateGetInfected << setw(15) << itnUse;
}

void Human::calculateItnEff() {
    itnEfficiency = exp(-1.0 * pow((((double) currentDay - dayNewItn) / (365 / 2)), 2));
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

double Human::getItnMortality() {
    double d0 = 0.0;
    double d1 = 0.2;
    double d = d0 + (d1 - d0) * itnEfficiency;
    return d;
}

double Human::getItnRepelling() {
    double s0 = 0.95;
    double s1 = 0.3;
    double s = s1 + (s0 - s1) * (1 - itnEfficiency);
    return s;
}

void Human::preErythrocytic(double peUpTake) {
    /**
     * @param peUpTake
     */
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (unifRand < peUpTake) {
        rateGetInfected = 0;
    }
}

void Human::transBlocking(double tbUpTake) {
    /**
     *
     * @param tbUpTake
     */
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

void Human::drugTreatment() {
    /**
     * Sets the clear infection day to be drugDaysToClearI days from current day.
     */
    if (clearIDay > currentDay + drugDaysToClearI) {
        clearIDay = currentDay + drugDaysToClearI;
        clearFDay = currentDay + drugDaysToClearI;
    }
    setUnderDrugTreatment(true);
    setDrugTEffectStartDay(currentDay);
    setDrugTEffectEndDay(currentDay + drugProphylaxisDays);
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

void Human::getInfection() {
    /**
     * This function is used for getting human infected. Infection start and
     * clear days are calculated depending on whether it is a simple or super
     * infection. This function also calls feverMalariaAge()
     *
     * @see feverMalariaAge()
     */
    //cout << "\n\n\t++++++++++++++ in getInfection ++++++++++++";
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (underDrugTreatment) {
        return;
    } else if (!isInfected) { //simple infection case
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
    feverMalariaAge();

    //give clearI to people infected on day=0
}

int Human::getIncubationDays() {
    /**
     *
     * @return Returns the number of incubation days.
     */
    return incubationDays;
}

int Human::getLatencyDays() {
    /**
     *
     * @return Returns the number of latency days.
     */
    return latencyDays;
}

void Human::getNet() {
    itn = true;
    dayNewItn = currentDay;
}

void Human::renew() {
    /**
     * Sets Human::bday to current day and Human::dday to a random number > 85*365. Clears the rest of Human variables.
     */
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
}

void Human::feverMalariaAge() {
    /**
     * Probabilistically calculated if Human would get fever. If yes then sets fever start day.
     */
    double unifRand = (double) rand() / (double) RAND_MAX;
    //double probFever = exp(-0.4 * pow(malariaAge / 1000.0, 4));
    double probFever = 0.8 * exp(-1.0 * pow(malariaAge / 1000.0, 3));
    if (probFever < 0.01)
        probFever = 0.01;
    if (unifRand < probFever) {
        startFDay = currentDay + getIncubationDays();
        //how to set clearF day;
    }
}

bool Human::getTreatment() {
    double unifRand = (double) rand() / (double) RAND_MAX;
    if (unifRand < pTreat) {
        patch->clinic->humanVisit(this);
        return true;
    } else {
        clearFDay = currentDay + 12; // parameterize
        return false;
    }
}

void Human::setInitialInf() {
    isInfected = true;
    infectivity = maxInfectivity;
    double unifRand = (double) rand() / (double) RAND_MAX;
    clearIDay = getLatencyDays() + (int) (-mUntreatedInfDays * log(unifRand));
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



/*
void Human::isAlive() {
        if (currentday == deathday)
                Human();// overloaded constructor
}

void Human::setInfectiousness()
{
// Set the net infectiousness on each day for each human and for each house.
int i;

for (i=0; i<nHumans;i++)
{
  if (human[i].MOI > 1)
  {
        human[i].tX = human[i].c;
  }
}
}

void Human::updateImmunity(int i, int d, HUMAN *hum)
{
  // Update an individual's malaria age and the
  // level of premunition.

}

void Human::waningProtection()
{
  // Adjust the parameters that describe the protective effects of vaccines to
  // simulate waning effects over time.

}

void Human::moveHuman()
{
  // Determine the location of a human on a particular day.

}


 */
