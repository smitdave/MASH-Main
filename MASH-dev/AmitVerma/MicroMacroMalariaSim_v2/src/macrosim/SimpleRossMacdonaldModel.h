/* 
 * File:   SimpleRossMacdonaldModel.h
 * Author: amit
 *
 * Created on April 25, 2013, 12:02 PM
 */

#ifndef SIMPLEROSSMACDONALDMODEL_H
#define	SIMPLEROSSMACDONALDMODEL_H

#include "Patch.h"

class SimpleRossMacdonaldModel: public MacroModel {
private:
    Patch* patch;
    // Mosquitos
    double kappa;
    double p0;                     ///< Initial p
    double p;
    double a0; //initial f, read a from patches file in f0
    long double a;
    double psi;
    double lambda;                 ///< New mosquito generation rate
    double dailyLambda;            ///< Seasonal factor of new mosquito generation rate
    int tau;                       ///< Number of days required to complete sporogony
    double U;                      ///< Number of uninfected mosquitoes
    double Unew;                   ///< Updated number of uninfected mosquitoes
    std::vector<double> Y;         ///< Number of infected but not infectious mosquitoes
    std::vector<double> Ynew;      ///< Updated number of infected but not infectious mosquitoes
    double Z;                      ///< Number of infectious mosquitoes
    double Znew;                   ///< Updated number of infectious mosquitoes
    double sumZ;                   ///< Total number of infectious mosquitoes in a day (sum accross the iterations or feeding bouts)

public:
    SimpleRossMacdonaldModel(Patch*, double, double, double, double, int);
    SimpleRossMacdonaldModel(const SimpleRossMacdonaldModel& orig);
    virtual ~SimpleRossMacdonaldModel();
    void calculateParameters();
    void updateParameters();
    double getTotMosquitoes();
    double getInfectedButNotInfectiousMosquitoes();
    double getInfectiousMosquitoes();
    double getU();
    double getZ();
    double getY(int);
    double getYSum();
    double getA();
    double getPsi();
    double getP();  
    double getKappa();
    void calculateKappa();
    void calculateP();
    void calculateA();
    void calculateU();
    void calculateY();
    void calculateZ();
    void dailyUpdate(double);
    void updateU();
    void updateY();
    void updateZ();
    void updateHEpsilon(Human*);
    void printState();
};

#endif	/* SIMPLEROSSMACDONALDMODEL_H */

