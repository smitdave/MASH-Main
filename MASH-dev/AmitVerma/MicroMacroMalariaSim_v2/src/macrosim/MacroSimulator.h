/* 
 * File:   MacroSimulator.h
 * Author: amit
 *
 * Created on September 21, 2011, 2:45 PM
 */

#ifndef MACROSIMULATOR_H
#define	MACROSIMULATOR_H

#include "Human.h"
#include "Patch.h"

class MacroSimulator {
private:
    int nDays;
    int nRuns;
    int runNum;
    int currentDay;
    int simModel;
    std::vector<Human*> humanPop;
    std::vector<Patch*> patches;
    double initialPopInfected;
    double dailyLambda[365];
    int nIterPerDay;
    int* newInfections;
    int* newFevers;
    int* newTreatedHs;
    int hLatencyDays;
    int hIncubationDays;
    int nDaysPerOutput;
    int mHUntreatedInfDays;
	bool readInfFile;
    std::string configFileName;
    std::string patchFileName;
    std::string humanFileName;
    std::string lambdaFileName;
	std::string initialHumanInfFileName;
    std::string inDir;
    std::string dirChar;
	std::string outStrAppend;
	void setHumanInf();
public:
    void simulate();
    void simulateOneRun();
    void reset();
    void setCountsToZero();
    void patchDynamics();
    void humanDynamics();
    std::vector<int> getnInfHumans(int, int);
    int getnInfHumans(int);
    void intervene();
    void populateObjects();
    void printObjects();
    void simEngine();
    void readPatches();
    void readHumans();
    void readLambda();
    void readSimControlParameters();
    void printPatches();
    void printHumans();
    void printSimParams();
    void setInitialPopInfected(double);
    void setNeighborhoods();
	double getUTotal();
	double getUTotal(int);
	double getYTotal();
	double getYTotal(int);
	double getZTotal();
	double getZTotal(int);
	//double getZRTotal();
	double getMeanMalariaAge();

    int nMassVaccinations;
    int nMassDrugAdministration;
    int nDistributeNets;
    int nMassSprays;
    double vaccinationCoverage;
    double peUptake;
    double tbUptake;
    double mdaCoverage;
    double itnCoverage;
    double irsCoverage;
    std::vector<int> daysMassVaccination;
    std::vector<int> daysMassDrugAdministration;
    std::vector<int> daysDistributeNets;
    std::vector<int> daysMassSprays;

    Patch* findPatch(int);
    //MacroSimulator(double);
    MacroSimulator(char *, char *, char *);
    //MacroSimulator();
    MacroSimulator(const MacroSimulator& orig);
    virtual ~MacroSimulator();
};

#endif	/* MACROSIMULATOR_H */


