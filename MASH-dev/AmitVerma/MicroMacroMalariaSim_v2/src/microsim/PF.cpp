/* 
 * File:   PF.cpp
 * Author: amit
 * 
 */

#include "PF.h"
#include "GlobalParameters.h"
using namespace std;

PF::PF(int d) {
    std::random_device rd;
    std::mt19937 gen(rd());
    startD = d + 6;
    std::exponential_distribution<double> expd(1. / 190.);
    endD = startD + expd(gen);
    merz = 4.4;
    std::normal_distribution<double> gr(10, 2);
    grow = gr(gen) / 2.0;
    std::normal_distribution<double> md(11.5, 1);
    maxD = md(gen);
    std::normal_distribution<double> sld(20, 3);
    shld = sld(gen);
    delta = 0.2;
    P = 0;
    G = 0;
}

unsigned PF::updateP(double xPKill) {
    P = P + outOfLiver();   // P=0 until parasites come out of the liver when they are seeded by merz
    if (P > 0) {
        P = P * getGrowthRate(xPKill);    // If P>0, compute the growth rate 
    }
    if (P < 1) {
        P = 0; // If P<1, set it to zero 
    }
    return P;
}

unsigned PF::outOfLiver() {
    if (currentDay == startD) {
        return pow(10, merz);
    }
    else return 0;    
}

double PF::medPFDens(int dd, double pKill) {
    int age = dd - startD;
    int merlin = min(dend, endD) - dd;
    if (age < 0 || merlin < 0) return 0; //nan?
    else {
        if (age < shld) {
            return min(maxD, merz +  grow * age) + pKill + (pDdef - pKill) * exp((pDlt - dd)/delta); // test for double conversion
        }
        else {
            return maxD * pow((endD - dd) / (endD - startD - shld), sigma) + pKill + (pDdef - pKill) * exp((pDlt - dd)/delta);
        }
    }
}

double PF::getGrowthRate(double xPKill) {
    double xP1, xP2;
    xP1 = medPFDens(currentDay, xPKill);
    xP2 = medPFDens(currentDay + 1, xPKill);
    return pow(10,xP1) * pow(10,xP2) * xPKill;
}


unsigned PF::updateG(double gPD, double iG_PD[], double surv) {
    //Mature Gametocyte Survival PARAMETER
    G = (G * surv + iG[9]) * gPD;
    if (G < 1) G = 0;
    
    //Maturation of iGetocytes 
    for(int i=9; i>0; i--) {
        iG[i] = iG[i-1];
    }
    
    //iGetocytogenesis
    double convert = 0.001;
    iG[0] = pow(10, P) * convert;
    
    //The Effects of Drugs
    for(int i=0; i<10; i++) {
        iG[i] *= iG_PD[i];
        if (iG[i] < 1){ // not required if iG is int
            iG[i] = 0;
        }
    }
    return G;
}

bool PF::isActive() {
    if(currentDay>=startD && currentDay<=endD) 
        return true;
    else return false;
}

unsigned PF::getP() const {
    return P;
}

PF::PF(const PF& orig) {
}

PF::~PF() {
}

