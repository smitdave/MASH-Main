/*
 * MalariaSim.cpp
 *
 */

/**
 * \file MalariaSim.cpp
 * \brief This file contains the code for simulator
 * 
 * It contains the simEngine() and main()
 */

/*
#include<iostream>
#include<iomanip>
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<vector>
#include<algorithm>
#include<cstdlib>
#include<ctime>
#include<fstream>
#include<ctime>
#include<cmath>*/

#include "location.h"
#include "GlobalParameters.h"
#include "smoothsort.cpp"
#include "Patch.h"
#include "House.h"
#include "Pond.h"
#include "Human.h"
#include "HumanV2.h"
#include "Mosquito.h"
#include "Distance.h"
#include "HealthClinic.h"
#include "Drug.h"

const char* getDirAppend() {
    const char* dirappend;
#if defined(_WIN32)
    dirappend = "\\";
#elif defined(__MACH__)
    dirappend = "/";
#elif defined(__UNIX__)
    dirappend = "/";
#else
    dirappend = "/";
#endif
    return dirappend;
}

using namespace std;

//-------------------------------------------------------------------------

void readSimControlParameters();
void humanDynamics();
void intervene();
void generateMozzies();
//void simHouseMozzies();
//void simRestingMozzies();
//void simOvipositingMozzies();
void mozzyDynamics();
void simMozzies();
void killMozzies();
void clinicDynamics();
//void killAgedMozzies ();
void simEngine();
int getnInfMozzies();
int getnTestMozzies();
int getnBSearchMozzies();
int getnRestMozzies();
int getnPSearchMozzies();
int getnOvipositMozzies();
int getInfHumans();
vector<int> getnInfHumans(int minAge, int maxAge);
void readPatches();
void readHouses();
void readHumans();
void readPondLambda();
void readPonds();
void readDrugs();
void populateObjects();
void showObjects();
void setAcdCountsToZero();
Patch* findPatch(int patchID);
//int findLocation(location *);
bool compare_distances(Distance*, Distance*);

//-------------------------------------------------------------------------
// use double_t for future versions
//int date;

int currentDay = 0;
Drug useDrug;
bool PF_MODEL = true;
// Itamar edited 8/10/10
static long mozzyID = 1000000;
int windForecast;
//
int nBites = 0;
int muhu = 0, mihu = 0, mihu_it = 0, muhi = 0, muhi_it = 0, mihi = 0, mihi_it = 0;

ofstream biteLog;
ofstream mozzyLog;
ofstream clinicLog;
//ofstream testLog;
ofstream mozzyTestLog;



//unsigned *clinicFevers;
//unsigned *clinicTested;
//unsigned *clinicPositive;
//unsigned *clinicTreated;

unsigned *acdFevers;
unsigned *acdTested;
unsigned *acdPositive;
unsigned *acdTreated;


ofstream clinicFeversFile;
ofstream clinicTestedFile;
ofstream clinicPositiveFile;
ofstream clinicTreatedFile;

ofstream acdFeversFile;
ofstream acdTestedFile;
ofstream acdPositiveFile;
ofstream acdTreatedFile;


vector<Patch*> patches;
// Itamar edit 8/11/10
vector<House*> houses;
vector<Pond*> ponds;
vector<HealthClinic*> clinics;
vector<Drug> drugs;

bool operator>=(location &loc1, location &loc2) {
    double x1, x2;
    if (loc1.isHouse) x1 = cos(windForecast) * loc1.house->coordX - sin(windForecast) * loc1.house->coordY;
    else x1 = cos(windForecast) * loc1.pond->coordX - sin(windForecast) * loc1.pond->coordY;
    if (loc2.isHouse) x2 = cos(windForecast) * loc2.house->coordX - sin(windForecast) * loc2.house->coordY;
    else x2 = cos(windForecast) * loc2.pond->coordX - sin(windForecast) * loc2.pond->coordY;
    if (x1 == x2) {
        double y1, y2;
        if (loc1.isHouse) y1 = sin(windForecast) * loc1.house->coordX + cos(windForecast) * loc1.house->coordY;
        else y1 = sin(windForecast) * loc1.pond->coordX + cos(windForecast) * loc1.pond->coordY;
        if (loc2.isHouse) y2 = sin(windForecast) * loc2.house->coordX + cos(windForecast) * loc2.house->coordY;
        else y2 = sin(windForecast) * loc2.pond->coordX + cos(windForecast) * loc2.pond->coordY;
        return (y1 >= y2);
    }
    return (x1 >= x2);
}

vector<location*> locations;
vector< vector<location*> > sortedLocations;
vector< vector<House*> > nearestHouses;
vector< vector<Pond*> > nearestPonds;
//
//vector<Mosquito> pondMozzies, restingMozzies, ovipositingMozzies;
vector<Mosquito*> mozzies;
//vector<Human*> humanPop;
vector<Human*> humanPop;
vector<int> massVaccinationDays;
vector<int> massDrugAdministrationDays;
vector<int> sprayHouseDays;
vector<int> distributeNets;

//double popInfected = 0.6; // proportion of people infected in the beginning of simulation
double dailyLambda[365];


/*  -------------------------------------------------------------------------------------------------------------------------
 *  simulation parameters
 *  -------------------------------------------------------------------------------------------------------------------------
 */
int nDays, restDays, ovipositDays, mozzyLifeSpan, mozzySporogonyDays;
int searchAlgo, nSearchHouse, nSearchPond;
double probRestIndoor, probAliveAtRest, probAliveAtOviposit, itnRepellingEffect, irsRepellingEffect, irsDecayRate, itnDecayRate;
int nMassVaccinations, nMassDrugAdministration, nSprayHouses, nDistributeNets;
double vaccinationCoverage, peUptake, tbUptake, mdaCoverage, irsCoverage, itnCoverage;
double clinicTestTP, clinicTestTN, clinicDrugAccuracy;
int acdResolution;
double acdProbTrigger, acdRadius, acdIntervention, clinicAcdCapacity;

vector<int> daysMassVaccination, daysMassDrugAdministration, daysSprayHouses, daysDistributeNets;

int nRuns, hIncubationDays, hLatencyDays, mHUntreatedInfDays, nDaysPerOutput, drugProphylaxisDays, drugDaysToClearI;
bool readInfFile;
double initialPopInfected, prTravelAbroad;
string patchFileName, humanFileName, houseFileName, lambdaFileName, pondFileName, drugFileName, initialHumanInfFileName;
string configFileName, dirChar, inDir = "", outDir = "";
//  -------------------------------------------------------------------------------------------------------------------------

void readSimControlParameters() {
    string line;

    char * c, *p;
    //printf("\nreading file: %s\n",filename);
    ifstream myfile(configFileName.c_str());
    cout << "\n\nSimulation Features!\n--------------------------";
    if (myfile.is_open()) {
        while (!myfile.eof()) {
            //cout<<"INSIDE WHILE";
            getline(myfile, line);
            cout << "\n" << line;

            //cout<<"\n \n";
            c = new char [line.size() + 1];
            strcpy(c, line.c_str());

            p = strtok(c, " =,");

            while (p != NULL) {
                if (p[0] == '#')
                    break;
                if (!strcmp(p, "NUM_DAYS_OF_SIMULATION")) {
                    p = strtok(NULL, " =,");
                    nDays = atoi(p);
                }
                if (!strcmp(p, "NUM_RUNS")) {
                    p = strtok(NULL, " =,");
                    nRuns = atoi(p);
                }
                if (!strcmp(p, "PATCH_WISE_INITIAL_POPULATION_INFECTED")) {
                    p = strtok(NULL, " =,");
                    readInfFile = (bool)atoi(p);
                }
                if (!strcmp(p, "PROPORTION_INITIAL_POPULATION_INFECTED")) {
                    p = strtok(NULL, " =,");
                    initialPopInfected = atof(p);
                }
                if (!strcmp(p, "NUM_INCUBATION_DAYS")) {
                    p = strtok(NULL, " =,");
                    hIncubationDays = atoi(p);
                }
                if (!strcmp(p, "NUM_LATENCY_DAYS")) {
                    p = strtok(NULL, " =,");
                    hLatencyDays = atoi(p);
                }
                if (!strcmp(p, "MOZZY_NUM_REST_DAYS")) {
                    p = strtok(NULL, " =,");
                    restDays = atoi(p);
                }
                if (!strcmp(p, "MOZZY_NUM_OVIPOSIT_DAYS")) {
                    p = strtok(NULL, " =,");
                    ovipositDays = atoi(p);
                }
                if (!strcmp(p, "MOZZY_LIFE_SPAN_DAYS")) {
                    p = strtok(NULL, " =");
                    mozzyLifeSpan = atoi(p);
                }
                if (!strcmp(p, "MOZZY_SPOROGONY_DAYS")) {
                    p = strtok(NULL, " =");
                    mozzySporogonyDays = atoi(p);
                }
                if (!strcmp(p, "MOZZY_PROB_REST_INDOORS")) {
                    p = strtok(NULL, " =,");
                    probRestIndoor = atof(p);
                }
                if (!strcmp(p, "MOZZY_PROB_STAYING_ALIVE_RESTING")) {
                    p = strtok(NULL, " =,");
                    probAliveAtRest = atof(p);
                }
                if (!strcmp(p, "MOZZY_PROB_STAYING_ALIVE_OVIPOSITING")) {
                    p = strtok(NULL, " =,");
                    probAliveAtOviposit = atof(p);
                }
                if (!strcmp(p, "NUM_DAYS_PER_OUTPUT")) {
                    p = strtok(NULL, " =,");
                    nDaysPerOutput = atoi(p);
                }
                if (!strcmp(p, "PATCHES_FILE")) {
                    p = strtok(NULL, " =,");
                    int pathTill = patchFileName.rfind(dirChar);
                    if (pathTill == string::npos) {
                        patchFileName = inDir;
                    }
                    patchFileName += string(p).substr(0, strlen(p) - 1);
                }
                if (!strcmp(p, "HOUSES_FILE")) {
                    p = strtok(NULL, " =,");
                    int pathTill = houseFileName.rfind(dirChar);
                    if (pathTill == string::npos) {
                        houseFileName = inDir;
                    }
                    houseFileName += string(p).substr(0, strlen(p) - 1);
                }
                if (!strcmp(p, "PONDS_FILE")) {
                    p = strtok(NULL, " =,");
                    int pathTill = pondFileName.rfind(dirChar);
                    if (pathTill == string::npos) {
                        pondFileName = inDir;
                    }
                    pondFileName += string(p).substr(0, strlen(p) - 1);
                }
                if (!strcmp(p, "DRUGS_FILE")) {
                    p = strtok(NULL, " =,");
                    int pathTill = drugFileName.rfind(dirChar);
                    if (pathTill == string::npos) {
                        drugFileName = inDir;
                    }
                    drugFileName += string(p).substr(0, strlen(p) - 1);
                }
                if (!strcmp(p, "HUMANS_FILE")) {
                    p = strtok(NULL, " =,");
                    int pathTill = humanFileName.rfind(dirChar);
                    if (pathTill == string::npos) {
                        humanFileName = inDir;
                    }
                    humanFileName += string(p).substr(0, strlen(p) - 1);
                }
                if (!strcmp(p, "HUMAN_PROB_TRAVEL_ABROAD")) {
                    p = strtok(NULL, " =,");
                    probRestIndoor = atof(p);
                }
                if (!strcmp(p, "LAMBDA_FILE")) {
                    p = strtok(NULL, " =,");
                    int pathTill = lambdaFileName.rfind(dirChar);
                    if (pathTill == string::npos) {
                        lambdaFileName = inDir;
                    }
                    lambdaFileName += string(p).substr(0, strlen(p) - 1);
                }
                if (!strcmp(p, "INITIAL_HUMAN_INFECTION_FILE")) {
                    p = strtok(NULL, " =,");
                    int pathTill = initialHumanInfFileName.rfind(dirChar);
                    if (pathTill == string::npos) {
                        initialHumanInfFileName = inDir;
                    }
                    initialHumanInfFileName += string(p).substr(0, strlen(p) - 1);
                }
                if (!strcmp(p, "MEAN_UNTREATED_INFECTION_DAYS")) {
                    p = strtok(NULL, " =,");
                    mHUntreatedInfDays = atoi(p);
                }
                if (!strcmp(p, "NUM_DAYS_OF_PROPHYLAXIS")) {
                    p = strtok(NULL, " =,");
                    drugProphylaxisDays = atoi(p);
                }
                if (!strcmp(p, "NUM_DAYS_TO_CLEAR_INFECTION")) {
                    p = strtok(NULL, " =,");
                    drugDaysToClearI = atoi(p);
                }
                if (!strcmp(p, "CLINIC_DRUG_ACCURACY")) {
                    p = strtok(NULL, " =,");
                    clinicDrugAccuracy = atof(p);
                }
                if (!strcmp(p, "CLINIC_TEST_TP")) {
                    p = strtok(NULL, " =,");
                    clinicTestTP = atof(p);
                }
                if (!strcmp(p, "CLINIC_TEST_TN")) {
                    p = strtok(NULL, " =,");
                    clinicTestTN = atof(p);
                }
                if (!strcmp(p, "ACD_RESOLUTION")) {
                    p = strtok(NULL, " =,");
                    acdResolution = atoi(p);
                }
                if (!strcmp(p, "ACD_INTERVENTION")) {
                    p = strtok(NULL, " =,");
                    acdIntervention = atoi(p);
                }
                if (!strcmp(p, "ACD_PROB_TRIGGER")) {
                    p = strtok(NULL, " =,");
                    acdProbTrigger = atof(p);
                }
                if (!strcmp(p, "ACD_RADIUS")) {
                    p = strtok(NULL, " =,");
                    acdRadius = atof(p);
                }
                if (!strcmp(p, "ACD_CAPACITY")) {
                    p = strtok(NULL, " =,");
                    clinicAcdCapacity = atoi(p);
                }
                if (!strcmp(p, "itnRepellingEffect")) {
                    p = strtok(NULL, " =,");
                    itnRepellingEffect = atof(p);
                }
                if (!strcmp(p, "irsRepellingEffect")) {
                    p = strtok(NULL, " =,");
                    irsRepellingEffect = atof(p);
                }
                if (!strcmp(p, "irsDecayRate")) {
                    p = strtok(NULL, " =,");
                    irsDecayRate = atof(p);
                }
                if (!strcmp(p, "itnDecayRate")) {
                    p = strtok(NULL, " =,");
                    itnDecayRate = atof(p);
                }
                if (!strcmp(p, "nMassVaccinations")) {
                    p = strtok(NULL, " =,");
                    nMassVaccinations = atoi(p);
                    //cout<<"\ndone mvacine\n";
                }
                if (!strcmp(p, "daysMassVaccination")) {
                    for (int i = 0; i < nMassVaccinations; i++) {
                        p = strtok(NULL, " =,");
                        daysMassVaccination.push_back(atoi(p));
                    }
                }
                if (!strcmp(p, "nMassDrugAdministration")) {
                    //cout<<"\nAFTER days mvaccin\n";
                    p = strtok(NULL, " =,");
                    nMassDrugAdministration = atoi(p);
                }
                if (!strcmp(p, "daysMassDrugAdministration")) {
                    for (int i = 0; i < nMassDrugAdministration; i++) {
                        p = strtok(NULL, " =,");
                        daysMassDrugAdministration.push_back(atoi(p));
                    }
                }
                if (!strcmp(p, "nSprayHouses")) {
                    p = strtok(NULL, " =,");
                    nSprayHouses = atoi(p);
                }
                if (!strcmp(p, "daysSprayHouses")) {
                    for (int i = 0; i < nSprayHouses; i++) {
                        p = strtok(NULL, " =,");
                        daysSprayHouses.push_back(atoi(p));
                    }
                }
                if (!strcmp(p, "nDistributeNets")) {
                    p = strtok(NULL, " =,");
                    nDistributeNets = atoi(p);
                }

                if (!strcmp(p, "daysDistributeNets")) {
                    for (int i = 0; i < nDistributeNets; i++) {
                        p = strtok(NULL, " =,");
                        daysDistributeNets.push_back(atoi(p));
                    }
                }
                if (!strcmp(p, "vaccinationCoverage")) {
                    p = strtok(NULL, " =,");
                    vaccinationCoverage = atof(p);
                }
                if (!strcmp(p, "peUptake")) {
                    p = strtok(NULL, " =,");
                    peUptake = atof(p);
                }
                if (!strcmp(p, "tbUptake")) {
                    p = strtok(NULL, " =,");
                    tbUptake = atof(p);
                }
                if (!strcmp(p, "mdaCoverage")) {
                    p = strtok(NULL, " =,");
                    mdaCoverage = atof(p);
                }
                if (!strcmp(p, "irsCoverage")) {
                    p = strtok(NULL, " =,");
                    irsCoverage = atof(p);
                }
                if (!strcmp(p, "itnCoverage")) {
                    p = strtok(NULL, " =,");
                    itnCoverage = atof(p);
                }
                if (!strcmp(p, "MOZZY_SEARCH_ALGO")) {
                    p = strtok(NULL, " =,");
                    searchAlgo = atoi(p);
                }
                if (!strcmp(p, "MOZZY_NUM_SEARCH_HOUSES")) {
                    p = strtok(NULL, " =,");
                    nSearchHouse = atoi(p);
                }
                if (!strcmp(p, "MOZZY_NUM_SEARCH_PONDS")) {
                    p = strtok(NULL, " =,");
                    nSearchPond = atoi(p);
                }
                p = strtok(NULL, " =,");

            }
            //char sdsd = getchar();
        }
        myfile.close();

        sort(daysMassVaccination.begin(), daysMassVaccination.end());
        sort(daysMassDrugAdministration.begin(), daysMassDrugAdministration.end());
        sort(daysSprayHouses.begin(), daysSprayHouses.end());
        sort(daysDistributeNets.begin(), daysDistributeNets.end());

        cout << "\nDone reading simulation parameters!";

    } else cout << "\nUnable to open file: default.in";
}

void testBites() {

    /*	for (int i=0; i<patches.size(); i++) {
            for (int j=0; j<patches[i].houses.size(); j++) {
                for (int k=0; k<patches[i].houses[j].humans.size(); k++) {
                    cout<<"\n"<<patches[i].houses[j].humans[k].nBites<<"\t"<<patches[i].houses[j].humans[k].nInfectiousBites;
                }
            }
            //char c = getchar();
        }*/
    for (int i = 0; i < humanPop.size(); i++) {
        cout << "\n" << humanPop[i]->nBites << "\t" << humanPop[i]->nInfectiousBites;
    }
}

void clinicDynamics() {
    for (int i = 0; i < clinics.size(); i++) {
        int cap = clinicAcdCapacity;
        while (clinics[i]->rcdHouseQueue.size() > 0 && cap > 0) {
            House *hs = clinics[i]->rcdHouseQueue[0];
            for (int j = 0; j < hs->humans.size(); j++) {
                //cout<<"\nchecking human: "<<hs->humans[j]->humanID;
                if (hs->humans[j]->hasFever) {
                    acdFevers[i]++;
                }

                if (acdIntervention == 2) {
                    hs->humans[j]->drugTreatment();
                    acdTreated[i]++;
                } else if (acdIntervention == 1) {
                    acdTested[i]++;
                    if (clinics[i]->testHuman(hs->humans[j])) {
                        acdPositive[i]++;
                        hs->humans[j]->drugTreatment();
                        acdTreated[i]++;
                    }
                }
            }
            cap--;
            //cout<<"\n--check clini rcd queue: "<<clinics[i]->rcdHouseQueue.size();
            clinics[i]->rcdHouseQueue.erase(clinics[i]->rcdHouseQueue.begin());
            //cout<<"--check clini rcd queue: "<<clinics[i]->rcdHouseQueue.size()<<"\n";
        }
    }

    /*    for (int i = 0; i < clinics.size(); i++) {
            if (clinics[i]->rcdHouseQueue.size() > clinics[i]->swampedTh) {
                cout << "\n--------Vector Control (Patch: " << clinics[i]->homePatchID << ")-----------------\n";
                clinics[i]->vectorControl();
                clinics[i]->rcdHouseQueue.clear();
            }
            if (clinics[i]->alert == 1 && currentDay - clinics[i]->dayLastForeignCase >= 30) {
                clinics[i]->setAlert(0);
            }
        }
     * */
}


void humanDynamics() {
    for (int i = 0; i < humanPop.size(); i++) {
        humanPop[i]->dailyDynamics();      
    }
}


void intervene() {
    if (daysMassVaccination[0] == currentDay) {
        cout << "\n------------------Mass Vaccination-------------------\n";
        for (int j = 0; j < patches.size(); j++) {
            patches[j]->massVaccinate(vaccinationCoverage, peUptake, tbUptake);
        }
        int inday = daysMassVaccination[0];
        daysMassVaccination.erase(daysMassVaccination.begin());
        daysMassVaccination.push_back(inday);
    }

    if (daysMassDrugAdministration[0] == currentDay) {
        cout << "\n------------------Mass Drug Administration-------------------\n";
        for (int j = 0; j < patches.size(); j++) {
            patches[j]->massDrugAdministrate(mdaCoverage);
        }
        int inday = daysMassDrugAdministration[0];
        daysMassDrugAdministration.erase(daysMassDrugAdministration.begin());
        daysMassDrugAdministration.push_back(inday);
    }

    if (daysSprayHouses[0] == currentDay) {
        cout << "\n------------------Mass Spraying-------------------\n";
        for (int j = 0; j < patches.size(); j++) {
            patches[j]->sprayHouses(irsCoverage);
        }
        int inday = daysSprayHouses[0];
        daysSprayHouses.erase(daysSprayHouses.begin());
        daysSprayHouses.push_back(inday);
    }

    if (daysDistributeNets[0] == currentDay) {
        cout << "\n------------------Mass Net Distribution-------------------\n";
        for (int j = 0; j < patches.size(); j++) {
            patches[j]->distributeNets(itnCoverage);
        }
        int inday = daysDistributeNets[0];
        daysDistributeNets.erase(daysDistributeNets.begin());
        daysDistributeNets.push_back(inday);
    }
}

void generateMozzies() {
    int n = 0;
    int mozzyDD;
    for (int i = 0; i < patches.size(); i++) {
        for (int j = 0; j < patches[i]->ponds.size(); j++) {
            //int nAdultEmergingMozzies = patches[i].getTotalHumans() * patches[i].ponds[j].lambda * dailyLambda[currentDay % 365];
            int nAdultEmergingMozzies = (int) (patches[i]->getTotalHumans() * patches[i]->ponds[j]->lambda * dailyLambda[currentDay % 365] * 0.01);
            n = n + nAdultEmergingMozzies;
            for (int k = 0; k < nAdultEmergingMozzies; k++) {
                //pondMozzies.push_back(Mosquito(currentDay, currentDay + mozzyLifeSpan, patches[i]->patchID, patches[i]->ponds[j]->pondID, probRestIndoor, probAliveAtRest, probAliveAtOviposit, restDays, ovipositDays)); //check constructor
                double unifRand = (double) rand() / (double) RAND_MAX;
                mozzyDD = currentDay + (int) (-mozzyLifeSpan * log(unifRand));
                mozzies.push_back(new Mosquito(mozzyID++, currentDay, mozzyDD, patches[i]->patchID, patches[i]->ponds[j]->pondID, probRestIndoor, probAliveAtRest, probAliveAtOviposit, restDays, ovipositDays, mozzySporogonyDays)); //check constructor
            }
        }
    }
    cout << "newgen: " << setw(8) << n;
}

void mozzyDynamics() {
    time_t t1, t2, t3, t4, t5;
    // Itamar edited 8/10/10
    /*	for(int h = 0; h < houses.size(); h++){
        houses[h]->sortDestinations(houses, windForecast, 9);
        houses[h]->sortDestinations(ponds, windForecast, 9);
    }
    for(int p = 0; p < ponds.size(); p++){
        ponds[p]->sortDestinations(houses, windForecast, 9);
    }*/
    //
    time(&t1);
    if (searchAlgo == 1) {
        for (int i = 0; i < sortedLocations[windForecast].size(); i++) {
            if (sortedLocations[windForecast][i]->isHouse) sortedLocations[windForecast][i]->house->locationVector = i;
            else if (sortedLocations[windForecast][i]->isPond) sortedLocations[windForecast][i]->pond->locationVector = i;
        }
    }
    time(&t2);
    generateMozzies();
    //cout<<"\ntest3"<<endl;
    time(&t3);
    //simHouseMozzies ();
    //simRestingMozzies ();
    //simOvipositingMozzies ();
    //killAgedMozzies ();
    time(&t4);
    simMozzies();
    time(&t5);

    //cout<<"\tTIME: "<<difftime(t2,t1)<<" "<<difftime(t3,t2)<<" "<<difftime(t4,t3)<<" "<<difftime(t5,t4)<<"\n";
    cout << "time: " << setw(3) << difftime(t5, t1);

}

void simMozzies() {
    double unifRand = 0.0;
    double probDeath = 0.0;
    int flag = 0;
    int n1 = 0;
    int n2 = 0;
    //for(int ind=0;ind<mozzies.size();ind++) {
    //	mozzyTestLog <<"\n"<<mozzies[ind].mozzyID<<" "<<currentDay<<" "<<mozzies[ind].dayNextStage<<" "<<mozzies[ind].stage<<" mozzy dynamics";
    //}

    for (int i = 0; i < mozzies.size(); i++) {
        if (mozzies[i]->infectionStartDay == currentDay) {
            mozzies[i]->isInfected = true;
        }
        if (mozzies[i]->dday <= currentDay)
            n2++;
        if (mozzies[i]->dday <= currentDay) {
            //cout<<"\nMozzy Died Naturally \n"<< mozzies.size();

            /*int p, id, pid;
            if (mozzies[i]->currentLocation.isHouse){
                if (mozzies[i]->currentLocation.isPond) {
                    cout<< "\n\n\nANAMOLY!!!";
                    exit(1);
                }
                id = mozzies[i]->currentLocation.house->houseID;
                pid = mozzies[i]->currentLocation.house->patch->patchID;
                p=1;
            }
            else {
                if (mozzies[i]->currentLocation.isHouse) {
                    cout<< "\n\n\nANAMOLY!!!";
                    exit(1);
                }
                id = mozzies[i]->currentLocation.pond->pondID;
                pid = mozzies[i]->currentLocation.pond->patch->patchID;
                p=2;
            }*/
            //should be implemented using a function call
            mozzyLog << "\n" << mozzies[i]->mozzyID << " " << mozzies[i]->isInfected << " " << mozzies[i]->bday << " " << mozzies[i]->patch->patchID << " " << currentDay << " " << mozzies[i]->stage << " ";
            mozzyLog << "natural" << " ";
            //mozzyLog <<p<<" "<<id<<" "<<pid;
            //delete(mozzies[i]);
            mozzies.erase(mozzies.begin() + i);
            n1 = n1 + 1;
            i = i - 1;
            //cout<<" "<< mozzies.size();
        }
    }

    //for(int ind=0;ind<mozzies.size();ind++) {
    //	mozzyTestLog <<"\n"<<mozzies[ind].mozzyID<<" "<<currentDay<<" "<<mozzies[ind].dayNextStage<<" "<<mozzies[ind].stage<<" sim mozzy";
    //}

    //cout<<"\ntest4"<<endl;

    for (int i = 0; i < mozzies.size(); i++) {
        mozzyTestLog << mozzies[i]->mozzyID << " " << currentDay << " " << mozzies[i]->dayNextStage << " " << mozzies[i]->stage << endl;

        clock_t t1, t2;
        //flag died=0;
        int tempID = mozzies[i]->mozzyID;
        int tempStage = mozzies[i]->stage;

        t1 = clock();
        do {
            flag = 0;
            switch (mozzies[i]->stage) {
                    //fly to house
                case 1: //cout<<"\ntest5"<<endl;
                    if (!(mozzies[i]->flyToHouse(0.1, 2))) {
                        //cout<<"\ntest6"<<endl;
                        break; //check weather location is set properly or not
                    } else {
                        //cout<<"\ntest12"<<endl;
                        double timeSearchHouse = 0.01;
                        mozzies[i]->dayUsed = mozzies[i]->dayUsed + timeSearchHouse; //assumption timeSearchHouse < 1 day
                        if (!(mozzies[i]->searchHouse())) {
                            if (mozzies[i]->dayUsed >= 1.0) {
                                mozzies[i]->dayUsed = mozzies[i]->dayUsed - 1;
                                break;
                            } else {
                                flag = 1;
                                break;
                            }
                        } else {
                            mozzies[i]->stage = 2;
                            mozzies[i]->dayNextStage = currentDay + (int) (mozzies[i]->dayUsed + mozzies[i]->restDays); //issue

                            //testLog <<"\n"<<mozzies[i]->mozzyID<<" "<<currentDay<<" "<<mozzies[i]->dayNextStage<<" "<<mozzies[i]->stage;
                            if (currentDay > mozzies[i]->dayNextStage)
                                //testLog <<" "<<"anamoly made";
                                mozzies[i]->dayUsed = (mozzies[i]->dayUsed + mozzies[i]->restDays) - (int) (mozzies[i]->dayUsed + mozzies[i]->restDays);
                        }
                    }

                    //rest
                case 2: unifRand = (double) rand() / (double) RAND_MAX;
                    //testLog<<"\n"<<mozzies[i]->mozzyID<<" "<<currentDay<<" "<<mozzies[i]->dayNextStage<<" "<<mozzies[i]->stage<<" rest";
                    probDeath = mozzies[i]->getPrDeathAtRest();
                    if (probDeath > unifRand) {
                        mozzyLog << "\n" << mozzies[i]->mozzyID << " " << mozzies[i]->isInfected << " " << mozzies[i]->bday << " " << mozzies[i]->patch->patchID << " " << currentDay << " " << mozzies[i]->stage << " ";
                        mozzyLog << "rest" << " " << 1 << " " << mozzies[i]->currentLocation->house->houseID << " " << mozzies[i]->currentLocation->house->patch->patchID;
                        //delete(mozzies[i]);
                        //cout<<"\ndied: "<<mozzies[i]->mozzyID<<endl;
                        mozzies.erase(mozzies.begin() + i);
                        //died=1;
                        i = i - 1;
                        break;
                    }
                    if (mozzies[i]->dayNextStage > currentDay)
                        break;
                    if (mozzies[i]->dayNextStage < currentDay) {
                        //testLog<<"\n"<<mozzies[i]->mozzyID<<" "<<currentDay<<" "<<mozzies[i]->dayNextStage<<" "<<mozzies[i]->stage<<" anamoly found";
                        break;
                    }
                    mozzies[i]->stage = 3;
                    if (mozzies[i]->dayUsed >= 1.0) {
                        mozzies[i]->dayUsed = mozzies[i]->dayUsed - 1;
                        break;
                    }
                    //fly to pond
                case 3: //cout<<"\nIn case3, i="<<i<<" MozzyID="<<mozzies[i]->mozzyID<<" size"<<mozzies.size()<<endl; 
                    if (!(mozzies[i]->flyToPond(0.1, 2))) break;
                    mozzies[i]->stage = 4;
                    mozzies[i]->dayNextStage = currentDay + (int) (mozzies[i]->dayUsed + mozzies[i]->ovipositDays);
                    //if(!(currentDay<mozzies[i]->dayNextStage))
                    //cout<<"\ncatch anamoly: "<<currentDay<<" "<<mozzies[i]->dayNextStage;
                    mozzies[i]->dayUsed = (mozzies[i]->dayUsed + mozzies[i]->restDays) - (int) (mozzies[i]->dayUsed + mozzies[i]->restDays);
                    //oviposit
                case 4: unifRand = (double) rand() / (double) RAND_MAX;
                    probDeath = 1 - mozzies[i]->probAliveAtOviposit;
                    if (probDeath > unifRand) {
                        mozzyLog << "\n" << mozzies[i]->mozzyID << " " << mozzies[i]->isInfected << " " << mozzies[i]->bday << " " << mozzies[i]->patch->patchID << " " << currentDay << " " << mozzies[i]->stage << " ";
                        mozzyLog << "oviposit" << " " << 2 << " " << mozzies[i]->currentLocation->pond->pondID << " " << mozzies[i]->currentLocation->pond->patch->patchID;
                        //delete(mozzies[i]);
                        //cout<<"\ndied: "<<mozzies[i]->mozzyID<<endl;
                        mozzies.erase(mozzies.begin() + i);
                        //died=1;
                        i = i - 1;
                        break;
                    }
                    if (mozzies[i]->dayNextStage > currentDay)
                        break;
                    if (mozzies[i]->dayNextStage < currentDay) {
                        cout << "\nAnamoly 1";
                        break;
                    }
                    mozzies[i]->stage = 1;
                    if (mozzies[i]->dayUsed >= 1.0) {
                        mozzies[i]->dayUsed = mozzies[i]->dayUsed - 1;
                        break;
                    }
                    flag = 1;
            }
        } while (flag); /*
        if (mozzies[i]->dday == currentDay) {
			//cout<<"\nMozzy Died Naturally \n"<< mozzies.size();
            mozzies.erase(mozzies.begin() + i);
			n = n+1;
			//cout<<" "<< mozzies.size();
        }
		 */

        t2 = clock();
        float tt = ((float) t2 - (float) t1)*1000 / CLOCKS_PER_SEC;
        if (tt > 0.0) {
            mozzyTestLog << "\n" << currentDay << " " << tempID << " " << tempStage << " " << tt;
        }
    }
    int s1 = 0, s2 = 0, s3 = 0, s4 = 0;
    for (int j = 0; j < mozzies.size(); j++) {
        if (mozzies[j]->stage == 1)
            s1++;
        else if (mozzies[j]->stage == 2)
            s2++;
        else if (mozzies[j]->stage == 3)
            s3++;
        else if (mozzies[j]->stage == 4)
            s4++;
    }
    cout << "hSearch: " << setw(8) << s1 << "Rest: " << setw(8) << s2 << "Pond: " << setw(8) << s3 << "Ovi: " << setw(8) << s4;
    //cout<<"\nNo. of mozzies died: "<<n1;
    //cout<<"\nNo. of mozzies that should have died: "<<n2;

}

/**
void killMozzies() {
    //for(list<Mosquito>::iterator mozzI = mozzies.begin();mozzI!=mozzies.end();mozzI++) {
    for(int i = 0; i < mozzies.size(); i++) {
        //cout<<"\nin killMozz: "<<mozzI->mozzyID<<endl;
        Mosquito* mozzI = mozzies[i];
        if (mozzI->dday <= currentDay) {
            if(mozzI->preMatureDeath) {
                //cout<<"this is here1"<<endl;
                if (mozzI->stage==2) {
                    mozzyLog <<"\n"<<mozzI->mozzyID<<" "<<mozzI->isInfected<<" "<<mozzI->bday<<" "<<mozzI->patch->patchID<<" "<<currentDay<<" "<<mozzI->stage<<" ";
                    mozzyLog <<"rest"<<" "<<1<<" "<<mozzI->currentLocation->house->houseID<<" "<<mozzI->currentLocation->house->patch->patchID;
                }
                if (mozzI->stage==4) {
                    mozzyLog <<"\n"<<mozzI->mozzyID<<" "<<mozzI->isInfected<<" "<<mozzI->bday<<" "<<mozzI->patch->patchID<<" "<<currentDay<<" "<<mozzI->stage<<" ";
                    mozzyLog <<"oviposit"<<" "<<2<<" "<<mozzI->currentLocation->pond->pondID<<" "<<mozzI->currentLocation->pond->patch->patchID;
                }
            }
            else {
                //cout<<"this is here"<<endl;
                mozzyLog <<"\n"<<mozzI->mozzyID<<" "<<mozzI->isInfected<<" "<<mozzI->bday<<" "<<mozzI->patch->patchID<<" "<<currentDay<<" "<<mozzI->stage<<" ";
                mozzyLog <<"natural"<<" ";
            }
            mozzies.erase(mozzies.begin() + i);
            //mozzI = mozzies.erase(mozzI);
            //mozzI--;
            i=i-1;
        }
    }
}
 */

/*
 *---------------------------------------------------------------------
 * This Function runs the Malaria Simulator
 *---------------------------------------------------------------------
 */

void simEngine() {
    time_t t1, t2, t3, t4, tStart, tEnd;

    /*
    char filename0[100];
    char filename1[100]; // Would contain the absolute filename of the file
    char filename2[100];
    char filename3[100];
    char filename4[100];

    
    
    

    if (outDir.compare("")) {
        strcpy(filename0, outDir.c_str());
        strcpy(filename1, outDir.c_str()); // Directory path
        strcpy(filename2, outDir.c_str());
        strcpy(filename3, outDir.c_str());
        strcpy(filename4, outDir.c_str());
    } else {
        strcpy(filename0, "");
        strcpy(filename1, "");
        strcpy(filename2, "");
        strcpy(filename3, "");
        strcpy(filename4, "");
    }*/


    int minAge = 2, maxAge = 10;
    ofstream myfile;


    acdFevers = new unsigned[patches.size()];
    acdTested = new unsigned[patches.size()];
    acdPositive = new unsigned[patches.size()];
    acdTreated = new unsigned[patches.size()];

    setAcdCountsToZero();

    //mozzyTestLog.open(strcat(filename4, "MozzyTestLog.txt"));
    mozzyTestLog.open((outDir + "MozzyTestLog.txt").c_str());

    ////test files
    //mozzyTestLog << "currentDay MozzyID stage time";
    //cout<<"outdir: "<<outDir;
    //mozzyTestLog.close();
    //exit(0);


    clinicFeversFile.open((outDir + "clinicFevers.txt").c_str());
    clinicTestedFile.open((outDir + "clinicTested.txt").c_str());
    clinicPositiveFile.open((outDir + "clinicPositive.txt").c_str());
    clinicTreatedFile.open((outDir + "clinicTreated.txt").c_str());
    acdFeversFile.open((outDir + "acdFevers.txt").c_str());
    acdTestedFile.open((outDir + "acdTested.txt").c_str());
    acdPositiveFile.open((outDir + "acdPositive.txt").c_str());
    acdTreatedFile.open((outDir + "acdTreated.txt").c_str());


    clinicLog.open((outDir + "clinicLog.txt").c_str());
    clinicLog << "day clinicPatchID swamped humanID houseID human_isInf human_foundInf human_assignedRightDrug travelledAbroad Rcd_Added";

    biteLog.open((outDir + "BitesLog.txt").c_str());
    biteLog << "day" << " " << "MozzyID" << " " << "M_IsInfected" << " " << "HumanID" << " " << "H_Isinfected" << " " << "HouseID" << " " << "PatchID" << " " << "infectionTrans";

    mozzyLog.open((outDir + "MozzyLog.txt").c_str());
    mozzyLog << "mozzyID" << " " << "isInfected" << " " << "bday" << " " << "bPatchID" << " " << "dday" << " " << "dStage" << " " << "dType" << " " << "dTimeLocation" << " " << "dPatch";

    myfile.open((outDir + "MalSimOutput.txt").c_str());
    myfile << "day" << " " << "nHumans" << " " << "nInfHumans" << " " << "totalMozzies" << " " << "nInfMozzies" << " ";
    myfile << "bite_searching_Mozzies" << " " << "resting_mozzies" << " " << "pond_searching_mozzies" << " " << "ovipositing_mozzies" << " ";
    myfile << "total_bites" << " " << "mUhU" << " " << "mIhU" << " " << "mIhU_ITrans" << " " << "mUhI" << " " << "mUhI_ITrans" << " " << "mIhI" << " " << "mIhI_ITrans";

    //myfile <<setw(15)<<"nHumansAge("<<minAge<<"-"<<maxAge<<")"<<setw(15)<<"nInfHumansAge("<<minAge<<"-"<<maxAge<<")\n";

    //cout<<"\n"<<setw(12)<<"currDay"<<setw(12)<<"n_pondMozz"<<setw(12)<<"n_restMozz"<<setw(12)<<"n_ovipMozz"<<setw(12)<<"nInfMozzies";
    //cout<<"\n"<<setw(12)<<"currDay"<<setw(12)<<"nMozzies"<<setw(12)<<"nInfMozzies";
    time(&tStart);
    for (int i = 0; i < nDays; i++) {

        //for(int ind=0;ind<mozzies.size();ind++) {
        //	mozzyTestLog <<"\n"<<mozzies[ind].mozzyID<<" "<<currentDay<<" "<<mozzies[ind].dayNextStage<<" "<<mozzies[ind].stage<<" before day";
        //}
        cout << left << "\n" << "Day: " << setw(6) << i + 1;
        nBites = 0;
        muhu = 0;
        mihu = 0;
        mihu_it = 0;
        muhi = 0;
        muhi_it = 0;
        mihi = 0;
        mihi_it = 0;

        time(&t1);
        windForecast = rand() % 360;
        time(&t3);
        //cout<<"\ntest1"<<endl;
        mozzyDynamics();
        //cout<<"\ntest2"<<endl;
        time(&t4);
        humanDynamics();
        clinicDynamics();
        intervene();
        time(&t2);

        if (currentDay % nDaysPerOutput == 0) {
            vector<int> humanSamples = getnInfHumans(minAge, maxAge);
            myfile << "\n" << currentDay << " " << humanSamples[0] << " " << humanSamples[1];
            myfile << " " << mozzies.size() << " " << getnInfMozzies() << " " << getnBSearchMozzies() << " " << getnRestMozzies();
            myfile << " " << getnPSearchMozzies() << " " << getnOvipositMozzies();
            myfile << " " << nBites << " " << muhu << " " << mihu << " " << mihu_it << " " << muhi << " " << muhi_it << " " << mihi << " " << mihi_it;
            cout << "tot: " << setw(9) << mozzies.size() << "inf: " << setw(9) << getnInfMozzies() << "totHum: " << setw(9) << humanPop.size() << "infHum: " << setw(9) << getInfHumans();

            clinicFeversFile << currentDay + 1;
            clinicTestedFile << currentDay + 1;
            clinicPositiveFile << currentDay + 1;
            clinicTreatedFile << currentDay + 1;
            acdFeversFile << currentDay + 1;
            acdTestedFile << currentDay + 1;
            acdPositiveFile << currentDay + 1;
            acdTreatedFile << currentDay + 1;

            for (int i = 0; i < patches.size(); i++) {

                clinicFeversFile << " " << clinics[i]->dailyFevers;
                clinicTestedFile << " " << clinics[i]->dailyTested;
                clinicPositiveFile << " " << clinics[i]->dailyPositive;
                clinicTreatedFile << " " << clinics[i]->dailyTreated;
                clinics[i]->resetDailyOutputs();

                acdFeversFile << " " << acdFevers[i];
                acdTestedFile << " " << acdTested[i];
                acdPositiveFile << " " << acdPositive[i];
                acdTreatedFile << " " << acdTreated[i];
            }

            clinicFeversFile << "\n";
            clinicTestedFile << "\n";
            clinicPositiveFile << "\n";
            clinicTreatedFile << "\n";
            acdFeversFile << "\n";
            acdTestedFile << "\n";
            acdPositiveFile << "\n";
            acdTreatedFile << "\n";

            setAcdCountsToZero();
        }
        currentDay++;
    }
    time(&tEnd);
    cout << "\n\nTotal simulation time: " << difftime(tEnd, tStart);
    cout << "\n";
    myfile.close();
    biteLog.close();
    mozzyLog.close();
    clinicLog.close();
    mozzyTestLog.close();

    clinicFeversFile.close();
    clinicTestedFile.close();
    clinicPositiveFile.close();
    clinicTreatedFile.close();
    acdFeversFile.close();
    acdTestedFile.close();
    acdPositiveFile.close();
    acdTreatedFile .close();

}
// End of simEngine()

void setAcdCountsToZero() {
    for (int i = 0; i < patches.size(); i++) {

        acdFevers[i] = 0;
        acdTested[i] = 0;
        acdPositive[i] = 0;
        acdTreated[i] = 0;
    }
}

int getnTestMozzies() {
    int n = 0;
    for (int i = 0; i < mozzies.size(); i++) {
        if (mozzies[i]->dayNextStage < currentDay)
            n++;
    }
    return n;
}

int getnInfMozzies() {
    int nInfMozzies = 0;
    /*
    for (int i=0; i<pondMozzies.size(); i++) {
        if (pondMozzies[i].isInfected)
            nInfMozzies ++;
    }
    for (int i=0; i<restingMozzies.size(); i++) {
        if (restingMozzies[i].isInfected)
            nInfMozzies ++;
    }
    for (int i=0; i<ovipositingMozzies.size(); i++) {
        if (ovipositingMozzies[i].isInfected)
            nInfMozzies ++;
    }*/
    for (int i = 0; i < mozzies.size(); i++) {
        if (mozzies[i]->isInfected)
            nInfMozzies++;
    }
    return nInfMozzies;
}

int getnBSearchMozzies() {
    int n = 0;
    for (int i = 0; i < mozzies.size(); i++) {
        if (mozzies[i]->stage == 1)
            n++;
    }
    return n;
}

int getnRestMozzies() {
    int n = 0;
    for (int i = 0; i < mozzies.size(); i++) {
        if (mozzies[i]->stage == 2)
            n++;
    }
    return n;
}

int getnPSearchMozzies() {
    int n = 0;
    for (int i = 0; i < mozzies.size(); i++) {
        if (mozzies[i]->stage == 3)
            n++;
    }
    return n;
}

int getnOvipositMozzies() {
    int n = 0;
    for (int i = 0; i < mozzies.size(); i++) {
        if (mozzies[i]->stage == 4)
            n++;
    }
    return n;
}

int getInfHumans() {
    int num = 0;
    for (int i = 0; i < humanPop.size(); i++) {
        if (humanPop[i]->isInfected) {
            num++;
        }
    }
    return num;
}

vector<int> getnInfHumans(int minAge, int maxAge) {
    vector<int> humanSamples;
    int nHumans = 0, nInfHumans = 0, nHumansAge = 0, nInfHumansAge = 0;
    /*for (int i=0; i<patches.size(); i++){
        for (int j=0; j<patches[i].houses.size(); j++) {
            for (int k=0; k<patches[i].houses[j].humans.size(); k++) {
                nHumans ++;
                if (patches[i].houses[j].humans[k].isInfected){
                //if (patches[i].houses[j].humans[k].startIDay < currentDay && patches[i].houses[j].humans[k].startIDay < currentDay &&
                    //	patches[i].houses[j].humans[k].startIDay < currentDay && patches[i].houses[j].humans[k].clearIDay < currentDay)
                    nInfHumans ++;
                }
                int age = (currentDay - patches[i].houses[j].humans[k].bday + 1)/365;
                if (age >= minAge && age <= maxAge) {
                    nHumansAge ++;
                    if (patches[i].houses[j].humans[k].isInfected)
                        nInfHumansAge ++;

                }
            }
        }
    }*/
    for (int i = 0; i < humanPop.size(); i++) {
        nHumans++;
        if (humanPop[i]->isInfected) {
            //if (patches[i].houses[j].humans[k].startIDay < currentDay && patches[i].houses[j].humans[k].startIDay < currentDay &&
            //	patches[i].houses[j].humans[k].startIDay < currentDay && patches[i].houses[j].humans[k].clearIDay < currentDay)
            nInfHumans++;
        }
        int age = (currentDay - humanPop[i]->bday + 1) / 365;
        if (age >= minAge && age <= maxAge) {
            nHumansAge++;
            if (humanPop[i]->isInfected)
                nInfHumansAge++;

        }
    }

    //cout<<"\n\n\t...............................................................";
    //cout<<"\n\tMonthly"<<"\t"<<nHumans<<"\t"<<nInfHumans<<"\t"<<nHumansAge<<"\t"<<nInfHumansAge;
    //cout<<"\n\n\t...............................................................";

    humanSamples.push_back(nHumans);
    humanSamples.push_back(nInfHumans);
    humanSamples.push_back(nHumansAge);
    humanSamples.push_back(nInfHumansAge);
    return humanSamples;
}

void readPatches() {
    FILE *f; // File pointer
    int i = 0; // Index counter to array houses
    char temp[100]; // Temporary string to store header from the input file

    int pid;
    double centx, centy;
    int nh, np;

    if (patchFileName.length() == 0) {
        cout << "Patches file not specified! Exiting.";
        exit(1);
    }

    if ((f = fopen(patchFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout << "\n\nError! Could not open file " << endl << patchFileName.c_str() << "\n\n";
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 100, f); // Read header into temporary variable
        while (!feof(f)) {
            fflush(f);
            int nbor0, nbor1, nbor2, nbor3;
            //cout<<"\nreading patchline";
            //char ccc=getchar();
            fscanf(f, "%d %lG %lG %d %d %d %d %d %d\n", &pid, &centx, &centy, &nh, &np, &nbor0, &nbor1, &nbor2, &nbor3); // Read record from the file
            //cout<<"\ntrying to construct";
            Patch *temppatch = new Patch(pid, centx, centy, nh, np, nbor0, nbor1, nbor2, nbor3);
            HealthClinic *hlc = new HealthClinic(temppatch, clinicTestTP, clinicTestTN, clinicDrugAccuracy, clinicAcdCapacity, acdRadius, acdResolution, acdProbTrigger);
            temppatch->clinic = hlc;
            clinics.push_back(hlc);
            //cout<<"before pushing";
            //ccc=getchar();
            patches.push_back(temppatch);
            //cout<<"\nCONSTRUCTED";
            //ccc=getchar();
            i++; // Increase the value of index counter
        }
    }
    fclose(f);
    cout << "\nDone reading patches!";
}

void readHouses() {
    FILE *f; // File pointer
    int i = 0; // Index counter to array houses
    char temp[100]; // Temporary string to store header from the input file

    int hid;
    double x, y, w, q;
    int pid, nh;

    if (houseFileName.length() == 0) {
        cout << "\nHouses file not specified! Exiting.\n";
        exit(1);
    }

    if ((f = fopen(houseFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout << "\n\nError! Could not open file " << endl << houseFileName.c_str() << "\n\n";
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 100, f); // Read header into temporary variable
        fgets(temp, 100, f); // currently disregarding 0th house
        while (!feof(f)) {
            fflush(f);
            fscanf(f, "%d %lG %lG %lG %lG %d %d\n", &hid, &x, &y, &w, &q, &pid, &nh); // Read record from the file
            //cout<<"\nRead record: "<<hid;
            //char cc=getchar();
            // Itamar edit 8/12/10
            House *house = new House(hid, x, y, w, q, pid, nh, irsRepellingEffect, itnRepellingEffect, irsDecayRate, itnDecayRate);
            findPatch(pid)->addHouse(house);
            houses.push_back(house);
            location * lc = new location(house, NULL);
            house->loc = lc;
            locations.push_back(lc);
            //
            i++; // Increase the value of index counter
        }
    }
    fclose(f);

    cout << "\nDone reading houses!";
}

void readHumans() {


    FILE *f; // File pointer
    int i = 0; // Index counter to array houses
    char temp[100]; // Temporary string to store header from the input file

    int ID, bday, dday, H, P, it;
    double w, c, b, atRisk, pTreat, itnUse, itnAge;
    bool itn;

    if (humanFileName.length() == 0) {
        cout << "Humans file not specified! Exiting.";
        exit(1);
    }

    if ((f = fopen(humanFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout << "\n\nError! Could not open file " << endl << humanFileName.c_str() << "\n\n";
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 100, f); // Read header into temporary variable
        while (!feof(f)) { //&& i<950

            //cout<<"\ntrying to read...";
            fflush(f);
            fscanf(f, "%d %d %d %d %d %lG %lG %lG", &ID, &bday, &dday, &H, &P, &w, &c, &b); // Read record from the file
            fscanf(f, "%lG %lG %d %lG %lG\n", &atRisk, &pTreat, &it, &itnUse, &itnAge);

            if (it)
                itn = true;
            else
                itn = false;


            double unifRand = (double) rand() / (double) RAND_MAX;
            bool isInf = false;
            if (unifRand < initialPopInfected)
                isInf = true;


            //cout<<"\nHuman # "<<ID;
            unifRand = (double) rand() / (double) RAND_MAX;
            //int routinePatchID = patches.size() * unifRand;
            int routinePatchID = patches[(int) (patches.size() * unifRand)]->patchID;
            //int routineHouseID = patches[routinePatchID]->randomHouseIndex();
            int routineHouseID = findPatch(routinePatchID)->houses[findPatch(routinePatchID)->randomHouseIndex()]->houseID;

            Human *hu = new HumanV2(ID, bday, dday, H, P, w, c, b, pTreat, itn, itnUse, itnAge, isInf, false, routinePatchID, routineHouseID, mHUntreatedInfDays, hIncubationDays, hLatencyDays, prTravelAbroad, drugProphylaxisDays, drugDaysToClearI);
            humanPop.push_back(hu);
            findPatch(P)->findHouse(H)->humans.push_back(hu);
            //patches[P-1].findHouse(H).humans.push_back(Human(ID, bday, dday, H, P, w, c, b, itnUse));
            //cout<<"\nHuman made# "<<ID;

            findPatch(routinePatchID)->findHouse(routineHouseID)->humans.push_back(hu);
            findPatch(routinePatchID)->findHouse(routineHouseID)->nHumans++;
            if (itn)
                findPatch(P)->findHouse(H)->dayNewItn = 0;

            i++; // Increase the value of index counter
            //
        }

        cout << "\nDone reading humans!";

    }
    fclose(f);
}

void readPondLambda() {

    FILE *f; // File pointer
    int i = 0; // Index counter to array houses
    char temp[100]; // Temporary string to store header from the input file

    double la, lg, lf;

    if (lambdaFileName.length() == 0) {
        cout << "Lambda file not specified! Exiting.";
        exit(1);
    }

    if ((f = fopen(lambdaFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout << "\n\nError! Could not open file " << endl << lambdaFileName.c_str() << "\n\n";
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 100, f); // Read header into temporary variable
        while (!feof(f) && i < 365) {
            fflush(f);
            fscanf(f, "%lG", &la); // Read record from the file
            dailyLambda[i] = la;
            i++; // Increase the value of index counter
        }
    }
    fclose(f);
}

void readPonds() {
    FILE *f; // File pointer
    int i = 0; // Index counter to array houses
    char temp[100]; // Temporary string to store header from the input file

    int poid, pid;
    double x, y, w, lambda;

    if (pondFileName.length() == 0) {
        cout << "Ponds file not specified! Exiting.";
        exit(1);
    }

    if ((f = fopen(pondFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout << "\n\nError! Could not open file " << endl << pondFileName.c_str() << "\n\n";
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 100, f); // Read header into temporary variable
        while (!feof(f)) {
            fflush(f);
            fscanf(f, "%lG %lG %lG %d %lG\n", &x, &y, &w, &pid, &lambda); // Read record from the file
            Pond *pond = new Pond(i + 1, x, y, pid, w, lambda);
            findPatch(pid)->addPond(pond);
            // Itamar edit 8/ 11/10
            ponds.push_back(pond);
            location *lc = new location(NULL, pond);
            pond->loc = lc;
            locations.push_back(lc);
            //
            i++; // Increase the value of index counter
        }
    }
    fclose(f);
    cout << "\nDone reading ponds!";
}

string getStringFromLine(string inp, int pos) {
    stringstream ss(inp);
    string elem;
    for (int i = 0; i <= pos; i++) {
        std::getline(ss, elem, ',');
    }
    return elem;
}


void readDrugs() {
    if (drugFileName.length() == 0) {
        cout << "Drugs file not specified! Exiting.";
        exit(1);
    }
    ifstream fin(drugFileName.c_str());

    if (!fin.good()) { // Check for successful file open
        cout << "\n\nError! Could not open file " << endl << drugFileName.c_str() << "\n\n";
        exit(1); // Exit if file not opened
    } else {
        string line;
        getline(fin, line);
        int numDrugs = atoi(getStringFromLine(line, 1).c_str());
        for (int i=0; i<numDrugs; i++) {
            getline(fin, line);
            string drugName = getStringFromLine(line, 1);
            getline(fin, line);
            unsigned tmax = atoi(getStringFromLine(line, 1).c_str());            
            getline(fin, line);
            vector<double> pkill;
            for (int j=0; j<tmax; j++) {
                pkill.push_back(atof(getStringFromLine(line, j+1).c_str()));
            }
            getline(fin, line);
            vector<double> gkill;
            for (int j=0; j<tmax; j++) {
                gkill.push_back(atof(getStringFromLine(line, j+1).c_str()));
            }
            vector<vector<double> > igkill(10);
            for (int k=0; k<10; k++) {
                getline(fin, line);
                for (int j=0; j<tmax; j++) {
                    igkill[k].push_back(atof(getStringFromLine(line, j+1).c_str()));
                }
            }
            Drug d(drugName,tmax,pkill,gkill,igkill);
            d.print();
            drugs.push_back(d);
        }
    }
    useDrug = drugs[1];
    fin.close();
    cout << "\nDone reading drugs!";
}

void setHumanInf() {
    FILE *f;
    char temp[100]; // Temporary string to store header from the input file
    int pid;
    double percentInf;
    Patch * patch;

    if (initialHumanInfFileName.length() == 0) {
        cout << "Patch wise human initial infection file not specified! Exiting.";
        exit(1);
    }

    if ((f = fopen(initialHumanInfFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout << "\n\nError! Could not open file " << endl << initialHumanInfFileName.c_str() << "\n\n";
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 100, f); // Read header into temporary variable
        while (!feof(f)) {
            fflush(f);
            fscanf(f, "%d %lG", &pid, &percentInf); // Read record from the file

            patch = findPatch(pid);
            if (patch != NULL) {
                //cout<<"\n======setting inf in patch:"<<pid<<" percent:"<<percentInf;
                patch->setInitialInfection(percentInf);
            }
        }
        int count = 0;
        for (int i = 0; i < humanPop.size(); i++) {
            if (humanPop[i]->isInfected) {
                count++;
            }
        }
        fclose(f);
        cout << "\nDone setting patch wise initial infection!";
        cout << "\nInfected " << count << " humans out of " << humanPop.size() << "\n";
    }
}

void populateObjects() {
    cout << "\n\nreading input files ...";
    //cout<<"\nbefore reading default.in";
    readSimControlParameters();
    //cout<<"\nbefore reading patches\n";
    readPatches();
    //cout<<"\nbefore reading houses\n";
    readHouses();
    //cout<<"\nbefore reading humans\n";
    readHumans();
    //cout<<"\nbefore reading lambda\n";
    readPondLambda();
    //cout<<"\nbefore reading ponds\n";
    readPonds();
    readDrugs();
    // Edited Itamar 8/16/10
    /*	for(windForecast = 0; windForecast < 360; windForecast++){
        for(int i = 0; i < sortedLocations[windForecast].size(); i++){
            cout<<"\n"<<i<<"		Wind: "<<windForecast<<"		ID: ";
            if(sortedLocations[windForecast][i].isHouse) cout<<sortedLocations[windForecast][i].house->houseID<<"		"<<(cos(windForecast)*sortedLocations[windForecast][i].house->coordX - sin(windForecast)*sortedLocations[windForecast][i].house->coordY);
            else cout<<sortedLocations[windForecast][i].pond->pondID<<"		"<<(cos(windForecast)*sortedLocations[windForecast][i].pond->coordX - sin(windForecast)*sortedLocations[windForecast][i].pond->coordY);
        }
    }*/
    /*	for(int h = 0; h < houses.size(); h++)
        houses[h]->initializeDestinations(houses, ponds, windDirections);
    for(int p = 0; p < ponds.size(); p++)
        ponds[p]->initializeDestinations(houses, windDirections);
    cout<<"\nDone intializing wind direction forecasts!";*/
}

void preProcessLocations() {
    if (searchAlgo == 1) {
        sortedLocations.push_back(locations);
        for (windForecast = 0; windForecast < 360; windForecast++) {
            smoothsort(&(sortedLocations[windForecast][0]), sortedLocations[windForecast].size());
            if (windForecast < 360) {
                vector<location*> next(sortedLocations[windForecast]);
                sortedLocations.push_back(next);
            }
        }
    } else if (searchAlgo == 2) {
        cout << "\nin preProcessingLocations()" << endl;

        if (nSearchHouse > houses.size()) {
            cout << "\nnSearchHouse > total number of houses. Setting nSearchHouse = houses.size().";
            nSearchHouse = houses.size();
        }
        if (nSearchPond > ponds.size()) {
            cout << "\nnSearchPond > total number of ponds. Setting nSearchPond = ponds.size().";
            nSearchPond = ponds.size();
        }
        for (int i = 0; i < locations.size(); i++) {

            vector <Distance*> *sorted;

            if (locations[i]->isHouse) {
                sorted = &locations[i]->house->nearestHouses;
                //cout<<"\n house no: "<<locations[i]->house->houseID<<endl;
            } else if (locations[i]->isPond) {
                sorted = &locations[i]->pond->nearestHouses;
                //cout<<"\n pond no: "<<locations[i]->pond->pondID<<endl;
            }

            //sorted = new vector<Distance*>();
            for (int j = 0; j < houses.size(); j++) {
                //cout<<"\n allocate"<<endl;
                Distance *dNode = new Distance(houses[j]->loc, locations[i]->x, locations[i]->y);
                //cout<<"\nfrom: "<<locations[i]->house->houseID<<" to house no: "<<houses[j]->houseID<<endl;
                sorted->push_back(dNode);
                //sorted.push_back(new Distance(houses[j]->loc,locations[i]->x,locations[i]->y));
                //cout<<"\n11111111111 "<<sorted.size()<<endl;
                inplace_merge(sorted->begin(), sorted->end() - 1, sorted->end(), compare_distances);
                //cout<<"\n222222222222 "<<sorted.size()<<endl;
                if (sorted->size() > nSearchHouse) {
                    //cout<<"\nfull"<<endl;
                    //free(sorted[sorted.size()-1]);
                    sorted->erase(sorted->end() - 1);
                    //cout<<"\nfull2"<<endl;
                }
            }
            //cout<<" size: "<<sorted->size();

            if (locations[i]->isHouse) {
                vector <Distance*> *sortedP;
                sortedP = &locations[i]->house->nearestPonds;
                //sortedP = new vector<Distance*>();
                //cout<<"\nIIIIIII"<<endl;
                for (int j = 0; j < ponds.size(); j++) {
                    Distance *dNode = new Distance(ponds[j]->loc, locations[i]->x, locations[i]->y);
                    sortedP->push_back(dNode);
                    //cout<<"\nTTTT"<<endl;
                    inplace_merge(sortedP->begin(), sortedP->end() - 1, sortedP->end(), compare_distances);
                    if (sortedP->size() > nSearchPond) {
                        //cout<<"assadfdsfdsf"<<endl;
                        sortedP->erase(sortedP->end() - 1);
                    }
                }
                //cout<<"\n"<<locations[i].house->nearestPonds.size();

            }
        }//need to connect to the house
    }
}

bool compare_distances(Distance* a, Distance* b) {
    return (a->d < b->d);
}

void showObjects() {
    for (int i = 0; i < patches.size(); i++) {
        patches[i]->printState();

    }

}

int main(int argc, char* argv []) {

    srand(time(0));

    dirChar += getDirAppend();

    if (argc < 2) { // Check for number of input arguments
        cout << "\nError! Not enough arguments. Please specify the configuration file.";
        exit(1); // Exit if not enough arguments
    } else {
        configFileName += argv[1];
    }

    if (argc == 4) {
        if (strcmp(argv[2], "-o")) {
            printf("\nError! Check arguments.\n\n");
            exit(1); // Exit not correct arguments
        } else {
            outDir += argv [3];
            outDir += dirChar;
        }
    }


    string path;
    path += configFileName;
    int pathTill = path.rfind(dirChar);
    if (pathTill != string::npos) {
        inDir = path.substr(0, pathTill + 1);
    }

    //cout<<"\n\nReceived arguments\n";
    populateObjects();
    cout << "\n\nJust made the world!\n++++++++++++++++++++++++++++++++++++++++++++\n\n\n";

    preProcessLocations();
    cout << "\n\nPreprocessing done!\n++++++++++++++++++++++++++++++++++++++++++++\n\n\n";


    House* h = houses[100];
    //cout<<"House TEST: "<<h->nearestHouses.size();
    //for(int i=0;i<h->nearestHouses.size();i++){
    //	cout<<"\n"<<h->nearestHouses[i]->d<<endl;
    //}
    //showObjects();
    simEngine();

    //cout<<"\n\nBite-Test\n+++++++++++++++";
    //testBites();

    cout << "\n\n\n\tSimulation Completed!\n\n\n\n\n\n";

    return 0;

}

Patch* findPatch(int patchID) {
    for (int p = 0; p < patches.size(); p++)
        if (patchID == patches[p]->patchID)
            return patches[p];
    return NULL;
}

