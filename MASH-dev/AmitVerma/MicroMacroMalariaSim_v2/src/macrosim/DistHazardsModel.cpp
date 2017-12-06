/* 
 * File:   DistHazardsModel.cpp
 * Author: amit
 * 
 * Created on April 22, 2013, 1:50 PM
 */

#include <vector>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "DistHazardsModel.h"
#include "Human.h"
using namespace std;

void DistHazardsModel::calculateParameters(){
    calculateUF();
	calculateUR();
	calculateUL();
	calculateYF();
	calculateYR();
	calculateYL();
	calculateZF();
	calculateZR();
	calculateZL();
}
		
void DistHazardsModel::updateParameters(){
    updateUF();
	updateUR();
	updateUL();
	updateYF();
	updateYR();
	updateYL();
	updateZF();
	updateZR();
	updateZL();
}

double DistHazardsModel::getInfectedButNotInfectiousMosquitoes(){
    double sum = 0.0;
    for(int i=0; i<tau; i++){
        sum = sum + YF[i]+YR[i]+YL[i];
    }
    return sum;
}

double DistHazardsModel::getInfectiousMosquitoes(){
    return ZF+ZR+ZL;
}

double DistHazardsModel::getTotMosquitoes(){
    return getInfectedButNotInfectiousMosquitoes() + getInfectiousMosquitoes() + UF+UR+UL;
}

double DistHazardsModel::getUF() {
    return UF;
}

double DistHazardsModel::getUR() {
    return UR;
}

double DistHazardsModel::getUL() {
    return UL;
}

void DistHazardsModel::updateUF() {
    UF = UFnew;
}

void DistHazardsModel::updateUR() {
    UR = URnew;
}

void DistHazardsModel::updateUL() {
    UL = ULnew;
}

void DistHazardsModel::calculateUF() {
    double diffUIn = 0.0;
    DistHazardsModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
        diffUIn = diffUIn + (*patch->dIn)[i] * neighM->getpF() * (1 - neighM->getF()) * neighM->getUF();
        diffUIn = diffUIn + (*patch->dIn)[i] * neighM->getpL() * neighM->getL() * neighM->getUL();
    }
    UFnew = pF * (1 - f) * patch->dTilda * UF + diffUIn + UL * pL * l * patch->dTilda;
    //cout<<" UUU: "<<UFnew<<" dTilda"<<dTilda<<" diffUIn:"<<diffUIn<<" UL:"<<UL<<" pF:"<<pF<<" (1-f):"<<1-f<<" UF:"<<UF<<" pL:"<<pL<<" l:"<<l;
}

void DistHazardsModel::calculateUR() {
    double diffUIn = 0.0;
    DistHazardsModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
        diffUIn = diffUIn + (*patch->dIn)[i] * neighM->getpF() * neighM->getF() * (1 - neighM->getKappa()) * neighM->getUF();
    }
    URnew = UR + pF * f * (1 - kappa) * patch->dTilda * UF + diffUIn;
    //cout<<" UR: "<<UFnew<<" dTilda"<<dTilda<<" diffUIn:"<<diffUIn<<" pF:"<<pF<<" f:"<<f<<" UF:"<<UF<<" (1-ka):"<<1-kappa;
}

void DistHazardsModel::calculateUL() {
    double diffUIn = 0.0;
    DistHazardsModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
        diffUIn = diffUIn + (*patch->dIn)[i] * neighM->getpL()*(1 - neighM->getL())*neighM->getUL();
    }
    //cout<<"\np:"<<patchID<<" diffUIn:"<<diffUIn;
    ULnew = pL * (1 - l) * patch->dTilda * UL + diffUIn;
}

double DistHazardsModel::getYF(int i) {
    return YF[i];
}

double DistHazardsModel::getYR(int i) {
    return YR[i];
}

double DistHazardsModel::getYL(int i) {
    return YL[i];
}

void DistHazardsModel::updateYF() {
    for (int i = 0; i < tau; i++) {
        YF[i] = YFnew[i];
    }
}

void DistHazardsModel::updateYR() {
    for (int i = 0; i < tau; i++) {
        YR[i] = YRnew[i];
    }
}

void DistHazardsModel::updateYL() {
    for (int i = 0; i < tau; i++) {
        YL[i] = YLnew[i];
    }
}

void DistHazardsModel::calculateYF() {
    double diffYIn;
    DistHazardsModel* neighM;
    for (int j = 0; j < tau; j++) {
        diffYIn = 0.0;
        for (int i = 0; i < patch->neighborsIn->size(); i++) {
            neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
            diffYIn = diffYIn + (*patch->dIn)[i] * neighM->getpF()*(1 - neighM->getF())*neighM->getYF(j);
            diffYIn = diffYIn + (*patch->dIn)[i] * neighM->getpL()*neighM->getL()*neighM->getYL(j);
        }
        YFnew[j] = pF * (1 - f) * patch->dTilda * YF[j] + diffYIn + pL * l * patch->dTilda * YL[j];
    }
}

void DistHazardsModel::calculateYR() {
    double diffUIn = 0.0;
    DistHazardsModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
        diffUIn = diffUIn + (*patch->dIn)[i] * neighM->getpF()*neighM->getF()*neighM->getKappa()*neighM->getUF();
    }
    YRnew[0] = YR[0] + pF * f * patch->dTilda * kappa * UF + diffUIn;

    double diffYIn;
    for (int j = 1; j < tau; j++) {
        diffYIn = 0.0;
        for (int i = 0; i < patch->neighborsIn->size(); i++) {
            neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
            diffYIn = diffYIn + (*patch->dIn)[i] * neighM->getpF()*neighM->getF()*neighM->getYF(j);
        }
        YRnew[j] = YR[j] + pF * f * patch->dTilda * YF[j] + diffYIn;
    }
}

void DistHazardsModel::calculateYL() {
    double diffYIn;
    DistHazardsModel* neighM;
    for (int j = 0; j < tau; j++) {
        diffYIn = 0.0;
        for (int i = 0; i < patch->neighborsIn->size(); i++) {
            neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
            diffYIn = diffYIn + (*patch->dIn)[i] * neighM->getpL()*(1 - neighM->getL())*neighM->getYL(j);
        }
        YLnew[j] = pL * (1 - l) * patch->dTilda * YL[j] + diffYIn;
    }
}

double DistHazardsModel::getYSum() {
    double ySum = 0;
    for (int i = 0; i < YF.size(); i++) {
        ySum = ySum + YF[i] + YR[i] + YL[i];
    }
    return ySum;
}

double DistHazardsModel::getYFSum() {
    double ySum = 0;
    for (int i = 0; i < YF.size(); i++) {
        ySum = ySum + YF[i];
    }
    return ySum;
}

double DistHazardsModel::getYRSum() {
    double ySum = 0;
    for (int i = 0; i < YR.size(); i++) {
        ySum = ySum + YR[i];
    }
    return ySum;
}

double DistHazardsModel::getYLSum() {
    double ySum = 0;
    for (int i = 0; i < YL.size(); i++) {
        ySum = ySum + YL[i];
    }
    return ySum;
}

double DistHazardsModel::getZF() {
    //cout<<"\nthis is ZF:"<<ZF;
    return ZF;
}

double DistHazardsModel::getSumZF() {
    //cout<<"\nthis is ZF:"<<ZF;
    return sumZF;
}

double DistHazardsModel::getZR() {
    return ZR;
}

double DistHazardsModel::getZL() {
    return ZL;
}

void DistHazardsModel::updateZF() {
    ZF = ZFnew;
    sumZF = sumZF + ZF;
}

void DistHazardsModel::updateZR() {
    ZR = ZRnew;
}

void DistHazardsModel::updateZL() {
    ZL = ZLnew;
}

void DistHazardsModel::calculateZF() {
    double diffZIn = 0.0;
    DistHazardsModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
        diffZIn = diffZIn + (*patch->dIn)[i] * neighM->getpF()*(1 - neighM->getF())*neighM->getZF();
        diffZIn = diffZIn + (*patch->dIn)[i] * neighM->getpL()*neighM->getL()*neighM->getZL();
    }
    ZFnew = pF * (1 - f) * patch->dTilda * ZF + diffZIn + pL * l * patch->dTilda * ZL;
}

void DistHazardsModel::calculateZR() {
    double diffZIn = 0.0;
    DistHazardsModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
        diffZIn = diffZIn + (*patch->dIn)[i] * neighM->getpF()*neighM->getF()*neighM->getZF();
    }
    ZRnew = ZR + pF * f * patch->dTilda * ZF + diffZIn;
}

void DistHazardsModel::calculateZL() {
    double diffZIn = 0.0;
    DistHazardsModel* neighM;
    for (int i = 0; i < patch->neighborsIn->size(); i++) {
        neighM = (DistHazardsModel*)(*patch->neighborsIn)[i]->getModel();
        diffZIn = diffZIn + (*patch->dIn)[i] * neighM->getpL()*(1 - neighM->getL())*neighM->getZL();
    }
    ZLnew = pL * (1 - l) * patch->dTilda * ZL + diffZIn;
}

void DistHazardsModel::dailyUpdate(double dayLambda) {
    patch->calculateDtilda();
    patch->calculateEffNHumans();
    patch->calculateSumHExposure();
    calculateKappa();
    calculatepF();
    calculatepR();
    calculateF();

    sumZF = 0;
    //order of these calculations if very important for correctness.
    UF = lambda * dayLambda * patch->effectiveNHumans + UF;
    UL = pR * UR + UL;
    UR = 0;
    //printMNums();
    //cout<<" nH:"<<nHumans<<" lam:"<<lambda<<" dLam:"<<dayLambda;
    ZF = ZF + YF[tau - 1];
    ZL = pR * YR[tau - 1] + ZL + pR * ZR + YL[tau - 1];
    ZR = 0;
    //cout<<"\ncheck: ";
    for (int i = tau - 1; i > 0; i--) {
        YF[i] = YF[i - 1];
        YL[i] = pR * YR[i - 1] + YL[i - 1];
        //cout<<"YR:"<<YR[i-1]<<" YL:"<<YL[i];
        YR[i] = 0;
    }
    YF[0] = 0;
    YR[0] = 0;
    YL[0] = 0;
}

void DistHazardsModel::calculatepF() {
    double num = 0.0;
    double deno = 0.0;
    for (int i = 0; i < patch->humans.size(); i++) {
        double tExp = patch->humans[i]->getBiteWeight() * patch->humans[i]->getTimeSpentInP(patch);
        num = num + (1 - patch->humans[i]->getItnMortality() * patch->humans[i]->getHasItn()) * tExp;
        deno = deno + tExp;
    }
    if (num == 0.0 && deno == 0.0)
        pF = 0.0;
    else pF = num / deno * p0;
}

void DistHazardsModel::calculatepR() {
    double num = 0.0;
    double deno = 0.0;
    for (int i = 0; i < patch->humans.size(); i++) {
        double tExp = patch->humans[i]->getBiteWeight() * patch->humans[i]->getTimeSpentInP(patch);
        num = num + (1 - patch->humans[i]->getIrsMortality() * patch->humans[i]->getHasSprayed()) * tExp;
        deno = deno + tExp;
    }
    if (num == 0.0 && deno == 0.0)
        pR = 0.0;
    else pR = num / deno * p0;
}

void DistHazardsModel::calculateF() {
    double deno = 0.0;
    for (int i = 0; i < patch->humans.size(); i++) {
        deno = deno + patch->humans[i]->getBiteWeight() * patch->humans[i]->getTimeSpentInP(patch);
    }
    if (patch->sumHExposure == 0 && deno == 0)
        f = 0.0;
    else f = patch->sumHExposure / deno * f0;
}

double DistHazardsModel::getPsi() {
    return psi;
}

double DistHazardsModel::getpF() {
    return pF;
}

double DistHazardsModel::getpR() {
    return pR;
}

double DistHazardsModel::getpL() {
    return pL;
}

double DistHazardsModel::getF() {
    return f;
}

double DistHazardsModel::getL() {
    return l;
}

double DistHazardsModel::getKappa() {
    return kappa;
}

void DistHazardsModel::calculateKappa() {
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

void DistHazardsModel::updateHEpsilon(Human* hu) {
        double epsl = 0.0;
        //cout<<"\n-----------------------"<<routinePatches.size()<<"-----------------------------";
        DistHazardsModel* rpM;
        for (int i = 0; i < hu->getNumRoutinePatches(); i++) {
            //cout<<"Patch i="<<i<<" Z:"<<routinePatches[i]->getZ()<<" A:"<<routinePatches[i]->getA()<<" biteWeight:"<<biteWeight<<" time:"<<timeSpentRP[i]<<" psi:"<<routinePatches[i]->getPsi()<<" exposure:"<<huExposure<<"\n";
            rpM = (DistHazardsModel*)hu->getRoutinePatch(i)->getModel();
            double e1 = rpM->getSumZF();
            double e2 = rpM->getF() * hu->getBiteWeight() * hu->getTimeSpentInP(hu->getRoutinePatch(i));
            double e3 = (1-(1-hu->getItnRepelling())*hu->getHasItn());
            double e4 = rpM->getPsi() + hu->getRoutinePatch(i)->getSumHExposure();
            epsl = epsl + (e1 * e2 * e3) / (e4);
            ///cout <<"\ne1:"<<e1<<" e2:"<<e2<<" e3:"<<e3<<" e4:"<<e4;
        }
        hu->setEpsilon(epsl);
        //cout<<"\nepsilon:"<<epsilon;
}

void DistHazardsModel::printState() {
    cout << setw(10) << lambda << setw(10) << p0 << setw(10) << f0 << setw(10) << psi << setw(10) << tau << setw(10) << l;
}

void DistHazardsModel::printMNums(int currDay, int iter) {
    double yf = 0, yr = 0, yl = 0;
    for (int k = 0; k < tau; k++) {
        yf = yf + YF[k];
        yr = yr + YR[k];
        yl = yl + YL[k];
    }
    cout << "\nd" << currDay << " iter:" << iter << " p" << patch->patchID << " uf:" << UF << " ur:" << UR << " ul:" << UL;
    /*
    for(int i=0;i<tau;i++) {
            cout<<" yf["<<i<<"]:"<<YF[i];
    }
    cout<<" yf:"<<yf;
    for(int i=0;i<tau;i++) {
            cout<<" yr["<<i<<"]:"<<YR[i];
    }
    cout<<" yr:"<<yr;
    for(int i=0;i<tau;i++) {
            cout<<" yl["<<i<<"]:"<<YL[i];
    }
    cout<<" yl:"<<yl<<" zf:"<<ZF<<" zr:"<<ZR<<" zl:"<<ZL;
     */
    cout << " kappa:" << kappa << " pF:" << pF << " pR" << pR << " pL:" << pL << " f:" << f << " l:" << l << " ";
}

DistHazardsModel::DistHazardsModel(Patch* pat, double lmd, double pp, double ppPerIt, double aaPerIt, double ps, int ta, double llPerIt, int nN) {
    patch = pat;
    lambda = lmd;
    p0 = ppPerIt;
    f0 = aaPerIt;
    pL = pp;
    psi = ps;
    tau = ta;
    l = llPerIt;
    for (int i = 0; i < tau; i++) {
        YF.push_back(0.0);
        YFnew.push_back(0.0);
        YR.push_back(0.0);
        YRnew.push_back(0.0);
        YL.push_back(0.0);
        YLnew.push_back(0.0);
    }
    ZF = 0.0;
    ZR = 0.0;
    ZL = 0.0;
    UF = 0.0;
    UR = 0.0;
    UL = 0.0;
}

DistHazardsModel::DistHazardsModel(const DistHazardsModel& orig) {
}

DistHazardsModel::~DistHazardsModel() {
}

