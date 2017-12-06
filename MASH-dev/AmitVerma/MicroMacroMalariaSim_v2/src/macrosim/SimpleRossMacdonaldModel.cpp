/* 
 * File:   SimpleRossMacdonaldModel.cpp
 * Author: amit
 * 
 * Created on April 25, 2013, 12:02 PM
 */

#include <vector>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "SimpleRossMacdonaldModel.h"
#include "Human.h"

using namespace std;

void SimpleRossMacdonaldModel::calculateParameters(){
    calculateU();
    calculateY();
    calculateZ();
}
		
void SimpleRossMacdonaldModel::updateParameters(){
    updateU();
	updateY();
	updateZ();
}

double SimpleRossMacdonaldModel::getInfectedButNotInfectiousMosquitoes(){
    double sum = 0.0;
    for(int i=0; i<tau; i++){
        sum = sum + Y[i];
    }
    return sum;
}

double SimpleRossMacdonaldModel::getInfectiousMosquitoes(){
    return Z;
}

double SimpleRossMacdonaldModel::getTotMosquitoes(){
    return getInfectedButNotInfectiousMosquitoes() + getInfectiousMosquitoes() + getU();
}

double SimpleRossMacdonaldModel::getU() {
    return U;
}

double SimpleRossMacdonaldModel::getY(int i) {
    return Y[i];
}

double SimpleRossMacdonaldModel::getYSum() {
	double ySum = 0;
	for (int i=0; i<Y.size(); i++) {
		ySum = ySum + Y[i];
	}
	return ySum;
}

double SimpleRossMacdonaldModel::getZ() {
    return Z;
}

double SimpleRossMacdonaldModel::getA() {
    return a;
}

double SimpleRossMacdonaldModel::getP() {
    return p;
}

double SimpleRossMacdonaldModel::getKappa() {
    return kappa;
}

double SimpleRossMacdonaldModel::getPsi() {
    return psi;
}

void SimpleRossMacdonaldModel::dailyUpdate(double dl) {
    dailyLambda = dl;
    patch->calculateEffNHumans();
    patch->calculateDtilda();
    patch->calculateSumHExposure();
    calculateKappa();
    calculateP();
    calculateA();
}

void SimpleRossMacdonaldModel::calculateU() {
    double diffUIn = 0.0;
    SimpleRossMacdonaldModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (SimpleRossMacdonaldModel*)(*patch->neighborsIn)[i]->getModel();
        diffUIn = diffUIn + (*patch->dIn)[i] * neighM->getP() * (1 - neighM->getA() * neighM->getKappa()) * neighM->getU();
    }
    Unew = (lambda * dailyLambda * patch->humans.size()) + p * (1 - a * kappa) * patch->dTilda * U + diffUIn;
	//cout<<"UUU: "<<Unew<<"\n";
}

void SimpleRossMacdonaldModel::updateU() {
    U = Unew;
}

void SimpleRossMacdonaldModel::calculateY() {
    double diffUIn = 0.0;
    SimpleRossMacdonaldModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (SimpleRossMacdonaldModel*)(*patch->neighborsIn)[i]->getModel();
        diffUIn = diffUIn + neighM->getP() * neighM->getA() * neighM->getKappa() * (*patch->dIn)[i] * neighM->getU(); //din?
    }
    Ynew[0] = p * kappa * a * patch->dTilda * U + diffUIn;
    double diffYIn;
    for (int i = 1; i < tau; i++) {
        diffYIn = 0.0;
        SimpleRossMacdonaldModel* neighM;
        for (int j = 0; j < patch->neighborsIn->size(); j++) {
            neighM = (SimpleRossMacdonaldModel*)(*patch->neighborsIn)[j]->getModel();
            diffYIn = diffYIn + neighM->getP() * (*patch->dIn)[j] * neighM->getY(i - 1);
        }
        Ynew[i] = p * patch->dTilda * Y[i - 1] + diffYIn;
		if(Ynew[i]<0) cout<<"Ynew["<<i<<"]: "<<Ynew[i]<<"\n";
    }
}

void SimpleRossMacdonaldModel::updateY() {
    for (int i = 0; i < tau; i++) {
        Y[i] = Ynew[i];
    }
}

void SimpleRossMacdonaldModel::calculateZ() {
    double diffZIn = 0.0;
    SimpleRossMacdonaldModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (SimpleRossMacdonaldModel*)(*patch->neighborsIn)[i]->getModel();
        diffZIn = diffZIn + neighM->getP() * (*patch->dIn)[i]*(neighM->getY(tau - 1) + neighM->getZ());
    }
    Znew = p * patch->dTilda * (Z + Y[tau - 1]) + diffZIn;
}

void SimpleRossMacdonaldModel::updateZ() {
    Z = Znew;
}

void SimpleRossMacdonaldModel::calculateP() {
    double num = 0.0;
    double deno = 0.0;
    double itnE = 0.0;
    double irsE = 0.0;
    double tExp = 0.0;
    for (int i = 0; i < patch->humans.size(); i++) {
        tExp = patch->humans[i]->getBiteWeight() * patch->humans[i]->getTimeSpentInP(patch);
        itnE = 1 - patch->humans[i]->getItnMortality() * patch->humans[i]->getHasItn();
        irsE = 1 - patch->humans[i]->getIrsMortality() * patch->humans[i]->getHasSprayed();
        num = num + itnE * irsE * tExp;
        deno = deno + tExp;
    }
    if (num == 0.0 && deno == 0.0)
        p = 0.0;
    else p = (num / (psi + deno)) * p0;
}

void SimpleRossMacdonaldModel::calculateA() {
    double deno = 0.0;
    for (int i = 0; i < patch->humans.size(); i++) {
        deno = deno + patch->humans[i]->getBiteWeight() * patch->humans[i]->getTimeSpentInP(patch);
    }
    if (patch->sumHExposure == 0 && deno == 0)
        a = 0.0;
    else a = (patch->sumHExposure / (psi + deno)) * a0;
}

void SimpleRossMacdonaldModel::updateHEpsilon(Human* hu) {
        double epsl = 0.0;
        //cout<<"\n-----------------------"<<routinePatches.size()<<"-----------------------------";
        SimpleRossMacdonaldModel * rpM;
        for (int i = 0; i < hu->getNumRoutinePatches(); i++) {
            //cout<<"Patch i="<<i<<" Z:"<<routinePatches[i]->getZ()<<" A:"<<routinePatches[i]->getA()<<" biteWeight:"<<biteWeight<<" time:"<<timeSpentRP[i]<<" psi:"<<routinePatches[i]->getPsi()<<" exposure:"<<huExposure<<"\n";
            rpM = (SimpleRossMacdonaldModel*)hu->getRoutinePatch(i)->getModel();
            double e1 = rpM->getZ();
            double e2 = rpM->getA() * hu->getBiteWeight() * hu->getTimeSpentInP(hu->getRoutinePatch(i));
            double e3 = (1-(1-hu->getItnRepelling())*hu->getHasItn());
            double e4 = rpM->getPsi() + hu->getRoutinePatch(i)->getSumHExposure();
            epsl = epsl + (e1 * e2 * e3) / (e4);
            ///cout <<"\ne1:"<<e1<<" e2:"<<e2<<" e3:"<<e3<<" e4:"<<e4;
        }
        hu->setEpsilon(epsl);
        //cout<<"\nepsilon:"<<epsilon;
}

void SimpleRossMacdonaldModel::calculateKappa() {
    double sum = 0.0;
    for (int i = 0; i < patch->humans.size(); i++) {
        if (patch->humans[i]->getIsInfected()) {
            double timeS = patch->humans[i]->getTimeSpentInP(patch);
            if (timeS < 0) {
                cout << "\nError: Time spent in this patch (patchID: " << patch->patchID << ") is negative";
                exit(0);
            }
            double itnE = (1 - (1 - patch->humans[i]->getItnRepelling()) * patch->humans[i]->getHasItn() * patch->humans[i]->getItnUse());
            //if (itnE<0)
            //	cout<<"\n----------------------------------"<<itnE<<"---------------------------\n";
            sum = sum + itnE * patch->humans[i]->getInfectivity() * patch->humans[i]->getBiteWeight() * timeS;
        }
    }
    //patch->calculateSumHExposure(); //could be done just once for now. But it might change if travel is incorporated in the system.
    if (sum == 0 && (patch->sumHExposure + psi) == 0)
        kappa = 0;
    else kappa = sum / (psi + patch->sumHExposure);
    if (kappa > 1)
        cout << "\n\nsum:" << sum << " sumExp:" << patch->sumHExposure << " psi:" << psi << "\n\n";
}

void SimpleRossMacdonaldModel::printState() {
    cout << setw(10) << lambda << setw(10) << p0 << setw(10) << a0 << setw(10) << psi << setw(10) << tau;
}

SimpleRossMacdonaldModel::SimpleRossMacdonaldModel(Patch* pat, double lmd, double pp, double aa, double ps, int ta) {
    patch = pat;
    lambda = lmd;
    p0 = pp;
    a0 = aa;
    psi = ps;
    tau = ta;
    for (int i = 0; i < tau; i++) {
        Y.push_back(0.0);
        Ynew.push_back(0.0);
    }
    Z = 0.0;
    U = 0.0;
}
SimpleRossMacdonaldModel::SimpleRossMacdonaldModel(const SimpleRossMacdonaldModel& orig) {
}

SimpleRossMacdonaldModel::~SimpleRossMacdonaldModel() {
}

