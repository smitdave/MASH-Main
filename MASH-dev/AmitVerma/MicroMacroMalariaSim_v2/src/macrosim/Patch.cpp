/* 
 * File:   Patch.cpp
 * Author: amit
 * 
 * Created on September 21, 2011, 2:44 PM
 */

#include <vector>
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>

#include "Patch.h"
#include "Human.h"
#include "DistHazardsModel.h"
#include "SimpleRossMacdonaldModel.h"

using namespace std;

MacroModel* Patch::getModel() {
    return model;
}

void Patch::dailyUpdate(double dailyLambda) {
    model->dailyUpdate(dailyLambda);
}

void Patch::calculateModelParams() {
    model->calculateParameters();
}

void Patch::updateModelParams() {
    model->updateParameters();
}

void Patch::calculateSumHExposure() {
    double ex = 0.0;
    for (int i = 0; i < humans.size(); i++) {
        ex = ex + (1 - (1 - humans[i]->getItnRepelling()) * humans[i]->getHasItn() * humans[i]->getItnUse()) * humans[i]->getBiteWeight() * humans[i]->getTimeSpentInP(this);
    }
    sumHExposure = ex;
}

double Patch::getSumHExposure() {
    return sumHExposure;
}

void Patch::calculateEffNHumans() {
    double sum = 0;
    for (int i = 0; i < nHumans; i++) {
        sum = sum + humans[i]->getTimeSpentInP(this);
    }
    effectiveNHumans = sum;
}

void Patch::calculateDtilda() {
    double dt = 0.0;
    for (int i = 0; i < neighborsOut.size(); i++) {
        dt = dt + dOut[i];
    }
    dTilda = 1 - dt;
}

void Patch::updateHEpsilons() {
    for(int i=0; i<nHumans; i++) {
        if(patchID == humans[i]->getHomePatchID()) {
            model->updateHEpsilon(humans[i]);
        }
    }
}

void Patch::setInitialInfection(double percentInf) {
    for (int j = 0; j < humans.size(); j++) {
        if (patchID == humans[j]->getHomePatchID()) {
            double unifRand = (double) rand() / (double) RAND_MAX;
            if (unifRand < percentInf) {
                //cout<<"\npatch:"<<patchID<<" human:"<<humans[j]->getHumanID()<<" unifRand:"<<unifRand<<" percentInf:"<<percentInf;
                //cout<<"\n+++++++++++setting human is Inf in patch: "<<patchID;
                humans[j]->setInitialInf();
            }
        }
    }
}

void Patch::massVaccinate(double vaccineCoverage, double peUpTake, double tbUpTake) {
    for (int j = 0; j < humans.size(); j++) {
        if (patchID == humans[j]->getHomePatchID()) {
            double unifRand = (double) rand() / (double) RAND_MAX;
            if (unifRand < vaccineCoverage) {
                humans[j]->preErythrocytic(peUpTake);
                humans[j]->transBlocking(tbUpTake);
            }
        }
    }
}

void Patch::massDrugAdministrate(int currentDay, double mdaCoverage) {
    for (int j = 0; j < humans.size(); j++) {
        if (patchID == humans[j]->getHomePatchID()) {
            double unifRand = (double) rand() / (double) RAND_MAX;
            if (unifRand < mdaCoverage) {
                humans[j]->drugTreatment(currentDay);
            }
        }
    }
}

void Patch::massSpray(int currentDay, double sprayCoverage) {
    for (int j = 0; j < humans.size(); j++) {
        if (patchID == humans[j]->getHomePatchID()) {
            double unifRand = (double) rand() / (double) RAND_MAX;
            if (unifRand < sprayCoverage) {
                humans[j]->getSpray(currentDay);
            }
        }
    }
}

void Patch::distributeNets(int currentDay, double itnCoverage) {
    //int sum = 0;
    for (int j = 0; j < humans.size(); j++) {
        if (patchID == humans[j]->getHomePatchID()) {
            //cout<<"\nHumans size: "<<humans.size();
            double unifRand = (double) rand() / (double) RAND_MAX;
            if (unifRand < itnCoverage) {
                humans[j]->getNet(currentDay);
                //cout<<"\npatch: "<<patchID<<" got net: "<<++sum<<" "<<humans[j]->getHumanID()<<" "<<humans[j]->getHasItn()<<" "<<humans[j]->getItnAge(currentDay);
            }
        }
    }
}

void Patch::outputToFile(int day) {
    //	*pOutFile <<day+1<<" "<<nHumans<<" "<<this->getU()<<" ";
    //	for(int i=0; i<this->tau; i++) {
    //		*pOutFile <<this->getY(i)<<" ";
    //	}
    //	*pOutFile <<this->getZ()<<" "<<this->getZ()/nHumans<<"\n";
}

void Patch::finishWriting() {
    *pOutFile << endl;
}

void Patch::addHuman(Human* h) {
    humans.push_back(h);
    nHumans++;
}

int Patch::getNumInfHumans() {
    int infH = 0;
    for (int i = 0; i < humans.size(); i++) {
        if (this == humans[i]->getHomePatch() && humans[i]->getIsInfected()) {
            infH++;
        }
    }
    return infH;
}

void Patch::addNeighborIn(int nID, Patch* nei, double diff) {
    neighborInID->push_back(nID);
    neighborsIn->push_back(nei);
    dIn->push_back(diff); //diffusion to neighbors
}

void Patch::addNeighborOut(int nID, Patch* nei, double diff) {
    neighborOutID.push_back(nID);
    neighborsOut.push_back(nei);
    dOut.push_back(diff); //diffusion to neighbors
}

int Patch::getNumNeighborsOut() {
    return numNeighborsOut;
}

Patch* Patch::getNeighborOut(int i) {
    return neighborsOut[i];
}

void Patch::setNeighborOut(Patch * pt, int i) {
    neighborsOut[i] = pt;
}

int Patch::getNeighborOutID(int i) {
    return neighborOutID[i];
}

double Patch::getDOut(int i) {
    return dOut[i];
}

void Patch::printState() {
    cout << "\n" << setw(10) << patchID;
    model->printState(); 
    cout  << setw(10) << nHumans << setw(10) << numNeighborsOut << "\t";
    for (int i = 0; i < numNeighborsOut; i++) {
        cout << " [" << neighborOutID[i] << ", " << dOut[i] << "]";
    }
    cout << "\t\t\t\t" << neighborInID->size() << "\t";
    for (int i = 0; i < neighborInID->size(); i++) {
        cout << " [" << (*neighborInID)[i] << ", " << (*dIn)[i] << "]";
    }
}

//void Patch::printHeaderToFile() {
//    *pOutFile << "day nHumans U ";
//    for (int i = 0; i < tau; i++) {
//        *pOutFile << "Y" << i + 1 << " ";
//    }
//    *pOutFile << "Z ZperHuman" << "\n";
//}

Patch::Patch(int pid, double lmd, double pp, double ppPerIt, double aa, double ps, int ta, double llPerIt, int nN, int modelType) {
    patchID = pid;
    if(modelType == 1) {
        model = new SimpleRossMacdonaldModel(this, lmd, pp, aa, ps, ta);
    }
    else if(modelType == 2) {
        model = new DistHazardsModel(this, lmd, pp, ppPerIt, aa, ps, ta, llPerIt, nN);
    }
    else {
        cout<<"\n\nFatal error: Unknown model type: "<<modelType;
        exit(1);
    }
    
    numNeighborsOut = nN;
    nHumans = 0;
    neighborInID = new vector<int>;
    //neighborInID->push_back(5);
    //cout<<"\nfirst element: "<<(*neighborInID)[0]<<endl;
    neighborsIn = new vector<Patch*>;
    dIn = new vector<double>;
    
    //ostringstream filename;
    //filename << "patchOutFile" << patchID << ".txt";
    //cout<<filename.str()<<endl;
    //string fname = filename.str();
    //pOutFile = new ofstream();
    //pOutFile->open(fname.c_str());
    //printHeaderToFile();
}

int Patch::getPatchID() {
    return patchID;
}

Patch::Patch() {
}

Patch::Patch(const Patch & orig) {
}

Patch::~Patch() {
    //pOutFile->close();
}

