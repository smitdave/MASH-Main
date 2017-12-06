/* 
 * File:   MacroSimulator.cpp
 * Author: amit
 * 
 * Created on September 21, 2011, 2:45 PM
 */
#include<cstdlib>
#include<iostream>
#include<iomanip>
#include<fstream>
#include<cstring>
#include<string>
#include<algorithm>
#include<cmath>
#include <sstream>


#include "MacroSimulator.h"
#include "MacroModel.h"

using namespace std;

void MacroSimulator::simulate() {
	if (nRuns < 0) {
		cout<<"\n\nNumber of runs < 0! \n\n";
		return;
	}
	runNum = 1;
	while(runNum <= nRuns) {
		cout<<"\n\nRUN: "<<runNum;
		simulateOneRun();
		runNum ++;
	}
}

void MacroSimulator::simulateOneRun() {
	populateObjects();
	simEngine();
	reset();
}

void MacroSimulator::reset() {
	for(int i=0; i<humanPop.size();i++) {
		delete(humanPop[i]);
	}
	humanPop.clear();
	for(int i=0; i<patches.size();i++) {
        delete(patches[i]->getModel());
		delete(patches[i]);
	}
	patches.clear();
	sort(daysMassVaccination.begin(), daysMassVaccination.end());
	sort(daysMassDrugAdministration.begin(), daysMassDrugAdministration.end());
	sort(daysDistributeNets.begin(), daysDistributeNets.end());
	sort(daysMassSprays.begin(), daysMassSprays.end());
}

void MacroSimulator::populateObjects() {
    cout << "\n\nreading input files ...";
    //cout<<"\nbefore reading default.in";
    //readSimControlParameters();
    readPatches();
    setNeighborhoods();
    readHumans();
    //printPatches();
    //printHumans();
	if (readInfFile)
		setHumanInf();
	readLambda();
	cout<<"\n\n";
}

void MacroSimulator::printObjects() {
    cout << "\nSimulation Params:\n";
    printSimParams();
    cout << "\nPatches:\n";
    printPatches();
    cout << "\nHumans:\n";
    printHumans();
}

void MacroSimulator::printSimParams() {
    cout << "\n\nnDays = " << nDays;
    cout << "initialPopInfected = " << initialPopInfected;

    cout << "\nnMassVaccinations = " << nMassVaccinations;
    cout << "\ndaysMassVaccination =";
    for (int i = 0; i < nMassVaccinations; i++) {
        cout << " " << daysMassVaccination[i];
    }
    cout << "\nvaccinationCoverage = " << vaccinationCoverage;
    cout << "\npeUptake = " << peUptake;
    cout << "\ntbUptake = " << tbUptake;

    cout << "\nnMassDrugAdministration = " << nMassDrugAdministration;
    cout << "\ndaysMassDrugAdministration =";
    for (int i = 0; i < nMassDrugAdministration; i++) {
        cout << " " << daysMassDrugAdministration[i];
    }
    cout << "\nmdaCoverage = " << mdaCoverage;


    cout << "\nnDistributeNets = " << nDistributeNets;
    cout << "\ndaysDistributeNets =";
    for (int i = 0; i < nDistributeNets; i++) {
        cout << " " << daysDistributeNets[i];
    }
    cout << "\nitnCoverage = " << itnCoverage;

}

void MacroSimulator::printPatches() {
    cout << "\n\n" << setw(10) << "ID" << setw(10) << "lambda" << setw(10) << "p0" << setw(10) << "f0" << setw(10) << "psi" << setw(10) << "tau" << setw(10) << "l" << setw(10) << "nHumans"<< setw(10) << "NOut" << setw(10) << "<ID, d>\t\t\t\tNIn\t\n";
    for (int i = 0; i < patches.size(); i++) {
        patches[i]->printState();
    }
    cout << "\n";
}

void MacroSimulator::printHumans() {
    cout << "\n\n" << setw(15) << "ID" << setw(10) << "home" << setw(10) << "bday" << setw(10) << "dday" << setw(10) << "age" << setw(15) << "w" << setw(15) << "c" << setw(15) << "b";
    cout << setw(15) << "atRisk" << setw(10) << "pTreat" << setw(5) << "itn" << setw(15) << "itnUse" << setw(15) << "itnAge" << setw(8) << "isInf" << setw(8) << "numRP" << setw(25) << "<pID, timeFrac>\n";
    for (int i = 0; i < humanPop.size(); i++) {
        humanPop[i]->printState(currentDay);
    }
    cout << "\n";
}

Patch* MacroSimulator::findPatch(int patchID) {
    for (int p = 0; p < patches.size(); p++)
        if (patchID == patches[p]->getPatchID())
            return patches[p];
    return NULL;
}

void MacroSimulator::setNeighborhoods() {
    for (int i = 0; i < patches.size(); i++) {
    	//cout<<"\nPatch"<<patches[i]->getPatchID()<<"Number of neighs:"<<patches[i]->getNumNeighborsOut()<<endl;

    	for (int j = 0; j < patches[i]->getNumNeighborsOut(); j++) {
            Patch * neighb = findPatch(patches[i]->getNeighborOutID(j));
            patches[i]->setNeighborOut(neighb, j);
            //cout<<"\nFrom patch:"<<patches[i]->getPatchID()<<" To Patch:"<<patches[i]->getNeighborOut(j)->getPatchID()<<endl;
            patches[i]->getNeighborOut(j)->addNeighborIn(patches[i]->getPatchID(), patches[i], patches[i]->getDOut(j));
        }
    }
}

void MacroSimulator::simEngine() {
	currentDay = 0;
	newInfections = new int[patches.size()];
    newFevers = new int[patches.size()];
    newTreatedHs = new int[patches.size()];

    setCountsToZero();

	int minAge = 0, maxAge = 5;
	double U, Y, Z, ZR;
	int sumIH, sumFH, sumTH;

    ostringstream runName;
    runName <<"Run"<<runNum;
    string runNStr = runName.str();

    string outFileName = "MacroSimOutput." + outStrAppend + runNStr + ".txt";
    string infHFileName = "InfHumans." + outStrAppend + runNStr + ".txt";
    string mozzyFileName = "Mosquitoes." + outStrAppend + runNStr + ".txt";
    string infMFileName = "InfMosquitoes." + outStrAppend + runNStr + ".txt";
    string newIFileName = "NewInfHumans." + outStrAppend + runNStr + ".txt";
    string newFFileName = "NewFevers." + outStrAppend + runNStr + ".txt";
    string newTFileName = "NewTreatments." + outStrAppend + runNStr + ".txt";

    ofstream outFile;
    ofstream infHFile;
    ofstream mozzyFile;
    ofstream infMFile;
    ofstream newIFile;
    ofstream newFFile;
    ofstream newTFile;


    outFile.open(outFileName.c_str());
    infHFile.open(infHFileName.c_str()); 
    mozzyFile.open(mozzyFileName.c_str());
    infMFile.open(infMFileName.c_str());
    newIFile.open(newIFileName.c_str());
    newFFile.open(newFFileName.c_str());
    newTFile.open(newTFileName.c_str());

	//--------------------------------------------------------------
	//    Headers for Outputs
	//--------------------------------------------------------------


    outFile << "day nMozzy nInfectedMozzy nInfectiousMozzy nHumans nInfHumans nNewInfections nNewFevers nNewTreatedHs meanMalariaAge\n";
    cout << "day nMozzy nInfectedMozzy nInfectiousMozzy nHumans nInfHumans nNewInfections nNewFevers nNewTreatedHs meanMalariaAge\n";


    //myfile << "day nMozzy nInfectedMozzy nInfectiousMozzy nFeedingInfectiousM nHumans nInfHumans nHumansAge(" << minAge << "-" << maxAge << ")" << " " << "nInfHumansAge(" << minAge << "-" << maxAge << ")\n";
    //cout << "day nMozzy nInfectedMozzy nInfectiousMozzy  nFeedingInfectiousM nHumans nInfHumans nHumansAge(" << minAge << "-" << maxAge << ")" << " " << "nInfHumansAge(" << minAge << "-" << maxAge << ")\n";

    //--------------------------------------------------------------

    while (currentDay < nDays) {
        patchDynamics();
        humanDynamics();
        intervene();

        if (currentDay % nDaysPerOutput == 0) {
        	U = getUTotal();
        	Y = getYTotal();
        	Z = getZTotal();
        	//ZR = getZRTotal();
        	vector<int> humanSamples = getnInfHumans(minAge, maxAge);
        	sumIH = 0; sumFH = 0; sumTH = 0;




			//--------------------------------------------------------------
			//    Printing to Output Files
			//--------------------------------------------------------------

			infHFile << currentDay+1;
		    mozzyFile << currentDay+1;
		    infMFile << currentDay+1;
		    newIFile << currentDay+1;
		    newFFile << currentDay+1;
		    newTFile << currentDay+1;

			for(int i=0;i<patches.size();i++) {

				infHFile <<" "<<getnInfHumans(i);
				mozzyFile <<" "<<patches[i]->getModel()->getTotMosquitoes();
				infMFile <<" "<<patches[i]->getModel()->getInfectiousMosquitoes();
				newIFile <<" "<<newInfections[i];
				newFFile <<" "<<newFevers[i];
				newTFile <<" "<<newTreatedHs[i];

				sumIH = sumIH + newInfections[i];
				sumFH = sumFH + newFevers[i];
				sumTH = sumTH + newTreatedHs[i];
			}

			infHFile << "\n";
		    mozzyFile << "\n";
		    infMFile << "\n";
		    newIFile << "\n";
		    newFFile << "\n";
		    newTFile << "\n";

	       	//outFile <<currentDay+1<<" "<<U+Y+Z<<" "<<Y+Z<<" "<<Z<<" "<<ZR<<" "<<humanSamples[0]<<" "<<humanSamples[1]<<" "<<sumIH<<" "<<sumFH<<" "<<sumTH<<" "<<getMeanMalariaAge()<<"\n";
			//cout <<currentDay+1<<" "<<U+Y+Z<<" "<<Y+Z<<" "<<Z<<" "<<ZR<<" "<<humanSamples[0]<<" "<<humanSamples[1]<<" "<<sumIH<<" "<<sumFH<<" "<<sumTH<<" "<<getMeanMalariaAge()<<"\n";

			outFile <<currentDay+1<<" "<<U+Y+Z<<" "<<Y+Z<<" "<<Z<<" "<<humanSamples[0]<<" "<<humanSamples[1]<<" "<<sumIH<<" "<<sumFH<<" "<<sumTH<<" "<<getMeanMalariaAge()<<"\n";
			cout <<currentDay+1<<" "<<U+Y+Z<<" "<<Y+Z<<" "<<Z<<" "<<humanSamples[0]<<" "<<humanSamples[1]<<" "<<sumIH<<" "<<sumFH<<" "<<sumTH<<" "<<getMeanMalariaAge()<<"\n";

            
            setCountsToZero();
			//--------------------------------------------------------------

			//myfile <<currentDay+1<<" "<<U+Y+Z<<" "<<Y+Z<<" "<<Z<<" "<<ZR<<" "<<humanSamples[0]<<" "<<humanSamples[1]<<" "<<humanSamples[2]<<" "<<humanSamples[3]<<"\n";
			//cout <<currentDay+1<<" "<<U+Y+Z<<" "<<Y+Z<<" "<<Z<<" "<<ZR<<" "<<humanSamples[0]<<" "<<humanSamples[1]<<" "<<humanSamples[2]<<" "<<humanSamples[3]<<"\n";
        	//cout<<"\n"<< currentDay<< " " << (int)getYTotal(0) <<" "<< (int)getYTotal(1) <<" "<<(int)getYTotal(9);

        }
        currentDay++;
    }
    cout << "\n";
	
	//for (int i = 0; i < patches.size(); i++) {
		//patches[i]->finishWriting();
	//}

    infHFile.close();
    mozzyFile.close();
    infMFile.close();
    newIFile.close();
    newFFile.close();
    newTFile.close();
    outFile.close();
    delete [] newInfections;
    delete [] newFevers;
    delete [] newTreatedHs;
}

void MacroSimulator::setCountsToZero() {
	for (int i=0;i<patches.size();i++) {
		newInfections[i] = 0;
		newFevers[i] = 0;
		newTreatedHs[i] = 0;
	}
}

void MacroSimulator::patchDynamics() {
    for (int i = 0; i < patches.size(); i++) {
        patches[i]->dailyUpdate(dailyLambda[currentDay % 365]);
    }
    //cout<<"\nnIter:"<<nIterPerDay;
    if (simModel==1) nIterPerDay=1;
	for (int j=0; j<nIterPerDay; j++) {
		for (int i = 0; i < patches.size(); i++) {
			//if (currentDay % nDaysPerOutput == 0) {
				//if (i==0) {//looking at individual patch
					//patches[i]->printMNums(currentDay, j);
				//}
			//}
            patches[i]->calculateModelParams();
		}
		for (int i = 0; i < patches.size(); i++) {
            patches[i]->updateModelParams();
			//patches[i]->outputToFile(currentDay);
			
			//patches[i]->printMNums(currentDay, j);
		}
	}
    for (int i = 0; i < patches.size(); i++) {
        patches[i]->updateHEpsilons();
    }
}

void MacroSimulator::humanDynamics() {

    for (int i = 0; i < humanPop.size(); i++) {
        //humanPop[i]->updateEpsilon();

        double unifRand = (double) rand() / (double) RAND_MAX;
		double eps = humanPop[i]->getEpsilon();
		
		//Poisson
		int poissEps = -1;
		double sum = 0;
		while(sum <= eps) {
			sum = sum -log((double)rand()/(double) RAND_MAX);
			poissEps++;
        }
		
		double probGetInfected = 1 - pow((1 - humanPop[i]->getRateGetInfected()), poissEps);
		//cout<< "\nProbGetInf: "<<probGetInfected<<"\tRandom Prop: "<<unifRand<<"\tPoissEps: "<<poissEps;
		
        if (unifRand < probGetInfected) {			
            humanPop[i]->setInfection(currentDay);
            newInfections[humanPop[i]->getHomePatchID()-1]++;
        }

        if (currentDay == humanPop[i]->getStartIDay()) {
            humanPop[i]->setIsInfected(true);
            humanPop[i]->setInfectivity(humanPop[i]->getMaxInfectivity());
            //cout << "\n\n\n\n\t==========================\n\tFound INFECTED Human\n\t==========================\n\n\n";
        }
        if (currentDay == humanPop[i]->getClearIDay()) {
            humanPop[i]->setIsInfected(false);
			humanPop[i]->setStartIDay(-1);
            humanPop[i]->setInfectivity(0.0);
        }
		
		//taken care of inside humanPop[i]->drugTreatment()
		//if (currentDay == humanPop[i]->getDrugTEffectStartDay()) {
        //    humanPop[i]->setUnderDrugInfected(true);
		//}
		if (currentDay == humanPop[i]->getDrugTEffectEndDay()) {
			//cout<<"------------------------OUT_OF_DRUG--------------------------";
            humanPop[i]->setUnderDrugTreatment(false);
			humanPop[i]->setDrugTEffectStartDay(-1);
			humanPop[i]->setDrugTEffectEndDay(-1);
        }
        if (currentDay == humanPop[i]->getStartFDay()) {
        	newFevers[humanPop[i]->getHomePatchID()-1]++;
			humanPop[i]->setHasFever(true);
			if(humanPop[i]->seekTreatment(currentDay))
				newTreatedHs[humanPop[i]->getHomePatchID()-1]++;
        }
        if (currentDay == humanPop[i]->getClearFDay()) {
			humanPop[i]->setHasFever(false);
        }
        if (currentDay == humanPop[i]->getDDay()) {
            humanPop[i]->renew(currentDay);
        }
        humanPop[i]->updateMalariaAge();
        humanPop[i]->calculateItnEff(currentDay+1);
    	humanPop[i]->calculateIrsEff(currentDay+1);
    }
}

void MacroSimulator::intervene() {
    if (daysMassVaccination[0]-1 == currentDay) {
        for (int j = 0; j < patches.size(); j++) {
            patches[j]->massVaccinate(vaccinationCoverage, peUptake, tbUptake);
        }
		int inday = daysMassVaccination[0];
        daysMassVaccination.erase(daysMassVaccination.begin());
		daysMassVaccination.push_back(inday);
    }

    if (daysMassDrugAdministration[0]-1 == currentDay) {
        for (int j = 0; j < patches.size(); j++) {
            patches[j]->massDrugAdministrate(currentDay, mdaCoverage);
        }
		int inday = daysMassDrugAdministration[0];
        daysMassDrugAdministration.erase(daysMassDrugAdministration.begin());
		daysMassDrugAdministration.push_back(inday);
    }


    if (daysMassSprays[0]-1 == currentDay) {
        for (int j = 0; j < patches.size(); j++) {
            patches[j]->massSpray(currentDay, irsCoverage);
        }
		int inday = daysMassSprays[0];
        daysMassSprays.erase(daysMassSprays.begin());
		daysMassSprays.push_back(inday);
    }

    if (daysDistributeNets[0]-1 == currentDay) {
        for (int j = 0; j < patches.size(); j++) {
            patches[j]->distributeNets(currentDay, itnCoverage);
        }
		int inday = daysDistributeNets[0];
        daysDistributeNets.erase(daysDistributeNets.begin());
		daysDistributeNets.push_back(inday);
    }
}

double MacroSimulator::getMeanMalariaAge() {
	double sum = 0.0;
	for (int i = 0; i < humanPop.size(); i++) {
		sum = sum + humanPop[i]->getMalariaAge();
	}
	return sum/humanPop.size();
}

double MacroSimulator::getUTotal() {
	double Utot = 0;
	for (int i=0;i<patches.size();i++) {
		Utot = Utot + (patches[i]->getModel()->getTotMosquitoes() - patches[i]->getModel()->getInfectiousMosquitoes() - patches[i]->getModel()->getInfectedButNotInfectiousMosquitoes());
	}
	return Utot;
}

double MacroSimulator::getUTotal(int pid) {
	return patches[pid]->getModel()->getTotMosquitoes() - patches[pid]->getModel()->getInfectiousMosquitoes() - patches[pid]->getModel()->getInfectedButNotInfectiousMosquitoes();
}

double MacroSimulator::getYTotal() {
	double Ytot = 0;
	for (int i=0;i<patches.size();i++) {
		Ytot = Ytot + patches[i]->getModel()->getInfectedButNotInfectiousMosquitoes();
	}
	return Ytot;
}

double MacroSimulator::getYTotal(int pid) {
	return patches[pid]->getModel()->getInfectedButNotInfectiousMosquitoes();
}

double MacroSimulator::getZTotal() {
	double Ztot = 0;
	for (int i=0;i<patches.size();i++) {
		Ztot = Ztot + patches[i]->getModel()->getInfectiousMosquitoes();
	}
	return Ztot;
}

double MacroSimulator::getZTotal(int pid) {
	return patches[pid]->getModel()->getInfectiousMosquitoes();
}

vector<int> MacroSimulator::getnInfHumans(int minAge, int maxAge) {
    vector<int> humanSamples;
    int nHumans = 0, nInfHumans = 0, nHumansAge = 0, nInfHumansAge = 0;

    for (int i = 0; i < humanPop.size(); i++) {
        nHumans++;
        if (humanPop[i]->getIsInfected()) {
            nInfHumans++;
        }
        int age = humanPop[i]->getAge(currentDay);
        if (age >= minAge && age <= maxAge) {
            nHumansAge++;
            if (humanPop[i]->getIsInfected())
                nInfHumansAge++;

        }
    }
    humanSamples.push_back(nHumans);
    humanSamples.push_back(nInfHumans);
    humanSamples.push_back(nHumansAge);
    humanSamples.push_back(nInfHumansAge);
    return humanSamples;
}

int MacroSimulator::getnInfHumans(int pid) {
	return patches[pid]->getNumInfHumans();
}

void MacroSimulator::setHumanInf() {
	FILE *f;
	char temp[100]; // Temporary string to store header from the input fil
	int pid;
	double percentInf;
	Patch * patch;
	
    if (initialHumanInfFileName.length() == 0) {
    	cout<<"Patch wise human initial infection file not specified! Exiting.";
    	exit(1);
    }

    if ((f = fopen(initialHumanInfFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout<<"\n\nError! Could not open file "<<endl<<initialHumanInfFileName.c_str()<<"tt this this\n\nlen:"<<initialHumanInfFileName.length();
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 100, f); // Read header into temporary variable
        while (!feof(f)) {
            fflush(f);
            fscanf(f, "%d %lG", &pid, &percentInf); // Read record from the file
			
			patch = findPatch(pid);			
			if(patch!=NULL) {
				//cout<<"\n======setting inf in patch:"<<pid<<" percent:"<<percentInf;
				patch->setInitialInfection(percentInf);
			}
		}
		int count=0;
		for (int i=0; i<humanPop.size();i++) {
			if(humanPop[i]->getIsInfected()) {
				count++;
			}
		}
		fclose(f);
		cout << "\nDone setting patch wise initial infection!";
		cout<<"\nInfected "<<count<<" humans out of "<<humanPop.size()<<"\n";
	}
}

void MacroSimulator::readSimControlParameters() {
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
                if (!strcmp(p, "SIMULATION_MODEL")) {
                    p = strtok(NULL, " =,");
                    simModel = atoi(p);
                }
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
                if (!strcmp(p, "NUM_FEEDING_ITERATIONS_PER_DAY")) {
					p = strtok(NULL, " =,");
					nIterPerDay = atoi(p);
                }
                if (!strcmp(p, "NUM_DAYS_PER_OUTPUT")) {
					p = strtok(NULL, " =,");
					nDaysPerOutput = atoi(p);
                }
                if (!strcmp(p, "PATCHES_FILE")) {
					p = strtok(NULL, " =,");
					int pathTill = patchFileName.rfind(dirChar);
					if(pathTill==string::npos){
						patchFileName = inDir;
					}
					patchFileName += string(p).substr(0,strlen(p)-1);
					//patchFileName += string(p);
					//if(patchFileName[patchFileName.length()-1]=='\0')
					//	patchFileName = patchFileName.substr(0,patchFileName.length()-1);
					//cout<<"patch:"<<patchFileName;
                }
                if (!strcmp(p, "HUMANS_FILE")) {
					p = strtok(NULL, " =,");
					int pathTill = humanFileName.rfind(dirChar);
					if(pathTill==string::npos){
						humanFileName = inDir;
					}
					humanFileName += string(p).substr(0,strlen(p)-1);
                }
                if (!strcmp(p, "LAMBDA_FILE")) {
					p = strtok(NULL, " =,");
					int pathTill = lambdaFileName.rfind(dirChar);
					if(pathTill==string::npos){
						lambdaFileName = inDir;
					}
					lambdaFileName += string(p).substr(0,strlen(p)-1);
                }
				if (!strcmp(p, "INITIAL_HUMAN_INFECTION_FILE")) {
					p = strtok(NULL, " =,");
					int pathTill = initialHumanInfFileName.rfind(dirChar);
					if(pathTill==string::npos){
						initialHumanInfFileName = inDir;
					}
					initialHumanInfFileName += string(p).substr(0,strlen(p)-1);
                }
                if (!strcmp(p, "MEAN_UNTREATED_INFECTION_DAYS")) {
					p = strtok(NULL, " =,");
					mHUntreatedInfDays = atoi(p);
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
                if (!strcmp(p, "nMassSprays")) {
                    p = strtok(NULL, " =,");
                    nMassSprays = atoi(p);
                }

                if (!strcmp(p, "daysMassSprays")) {
                    for (int i = 0; i < nMassSprays; i++) {
                        p = strtok(NULL, " =,");
                        daysMassSprays.push_back(atoi(p));
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
                if (!strcmp(p, "itnCoverage")) {
                    p = strtok(NULL, " =,");
                    itnCoverage = atof(p);
                }
                if (!strcmp(p, "irsCoverage")) {
					p = strtok(NULL, " =,");
					irsCoverage = atof(p);
				}
                p = strtok(NULL, " =,");

            }
            //char sdsd = getchar();
        }
        myfile.close();

        sort(daysMassVaccination.begin(), daysMassVaccination.end());
        sort(daysMassDrugAdministration.begin(), daysMassDrugAdministration.end());
        sort(daysDistributeNets.begin(), daysDistributeNets.end());
        sort(daysMassSprays.begin(), daysMassSprays.end());

        cout << "\nDone reading simulation parameters!";

    } else cout << "\nUnable to open file: macro.in";
}

void MacroSimulator::readPatches() {
    FILE *f; // File pointer
    int i = 0; // Index counter to array houses
    char temp[100]; // Temporary string to store header from the input file

    double lambda, p, a, psi, ll, nD;
    int pid, nN, pidN, tau;

    if (patchFileName.length() == 0) {
    	cout<<"Patches file not specified! Exiting.";
    	exit(1);
    }

    if ((f = fopen(patchFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout<<"\n\nError! Could not open file "<<endl<<patchFileName.c_str()<<"\n\n";
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 100, f); // Read header into temporary variable
        while (!feof(f)) {
            fflush(f);
            //cout<<"\nreading patchline";
            //char ccc=getchar();
            fscanf(f, "%d %lG %lG %lG %lG %d %lG %d", &pid, &lambda, &p, &a, &psi, &tau, &ll, &nN); // Read record from the file
            //Patch *temppatch = new Patch(pid, lambda, p, pow(p,1.0/nIterPerDay), pow(a,1.0/nIterPerDay), psi, tau, pow(ll,1.0/nIterPerDay), nN);
            Patch *temppatch = new Patch(pid, lambda, p, pow(p,1.0/nIterPerDay), a, psi, tau, ll, nN, simModel);
            for (i = 0; i < nN; i++) {
                fscanf(f, " %d %lG", &pidN, &nD);
                temppatch->addNeighborOut(pidN, NULL, nD);
            }
			temppatch->calculateDtilda();
            fscanf(f, "\n");
            //cout<<"\ntrying to construct";
            patches.push_back(temppatch);
            //cout<<"\nCONSTRUCTED";
            //ccc=getchar();
            i++; // Increase the value of index counter
        }
    }
    fclose(f);
    cout << "\nDone reading patches!";
}

void MacroSimulator::readHumans() {

    FILE *f; // File pointer
    char temp[1000]; // Temporary string to store header from the input file

    int ID, home, it, ir, nRP, pidRP;
    double bday, dday, w, c, b, atRisk, pTreat, itnUse, itnAge, irsAge, timeRP;
    bool itn, irs;

    if (humanFileName.empty()) {
    	cout<<"Humans file not specified! Exiting.";
    	exit(1);
    }

    if ((f = fopen(humanFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout<<"\n\nError! Could not open file "<<humanFileName<<"\n\n";
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 1000, f); // Read header into temporary variable
        while (!feof(f)) { //&& i<950

            //cout<<"\ntrying to read...";
            fflush(f);
            fscanf(f, "%d %d %lG %lG %lG %lG %lG", &ID, &home, &bday, &dday, &w, &c, &b); // Read record from the file
            fscanf(f, "%lG %lG %d %lG %lG %d %lG %d", &atRisk, &pTreat, &it, &itnUse, &itnAge, &ir, &irsAge, &nRP);

            if (it)
                itn = true;
            else
                itn = false;

            if (ir)
            	irs = true;
			else
				irs = false;


            double unifRand = (double) rand() / (double) RAND_MAX;
            bool isInf = false;
            if (!readInfFile && unifRand < initialPopInfected) {
				//cout<<"\nSETTING INF nonpatchwise!";
                isInf = true;
            }
            Human *hu = new Human(ID, home, findPatch(home), (int) bday, (int) dday, w, c, b, atRisk, pTreat, itn, itnUse, itnAge, irs, irsAge, nRP, isInf, hIncubationDays, hLatencyDays, mHUntreatedInfDays);

            for (int i = 0; i < nRP; i++) {
                fscanf(f, " %d %lG", &pidRP, &timeRP);
                //if(i==0)
                //cout<<"["<<pidRP<<" ,"<<timeRP<<"] ";
				Patch * hrp = findPatch(pidRP);
                hu->addRoutinePatch(pidRP, hrp, timeRP);
				hrp->addHuman(hu);
            }
            fscanf(f, "\n");
            humanPop.push_back(hu);
        }

        cout << "\nDone reading humans!";

    }
    fclose(f);
}

void MacroSimulator::readLambda() {
    FILE *f; // File pointer
    int i = 0; // Index counter to array houses
    int dd;
    char temp[100]; // Temporary string to store header from the input file

    if (lambdaFileName.empty()) {
    	cout<<"Lambda file not specified! Exiting.";
    	exit(1);
    }

    double la, lg, lf;

    if ((f = fopen(lambdaFileName.c_str(), "r")) == NULL) { // Check for successful file open
        cout<<"\n\nError! Could not open file "<<lambdaFileName<<"\n\n";
        exit(1); // Exit if file not opened
    } else {
        fgets(temp, 100, f); // Read header into temporary variable
        while (!feof(f) && i<365) {
            fflush(f);
            fscanf(f, "%lG", &la); // Read record from the file
            dailyLambda[i] = la;
            i++; // Increase the value of index counter
        }
    }
    fclose(f);
}

void MacroSimulator::setInitialPopInfected(double ip) {
    initialPopInfected = ip;
}

MacroSimulator::MacroSimulator(char* cfile, char* dirCh, char* strApp) {
	outStrAppend += strApp;
	if(outStrAppend.length() > 1)
		outStrAppend += ".";
	configFileName += cfile;
	dirChar += dirCh;
	string path;
	path += cfile;
	int pathTill = path.rfind(dirChar);
	if(pathTill!=string::npos){
		inDir = path.substr(0,pathTill+1);
	}
}

MacroSimulator::MacroSimulator(const MacroSimulator& orig) {
}

MacroSimulator::~MacroSimulator() {
}

