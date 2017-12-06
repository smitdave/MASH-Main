/* 
 * File:   DistHazardsModel.h
 * Author: amit
 *
 * Created on April 22, 2013, 1:50 PM
 */

#ifndef DISTHAZARDSMODEL_H
#define	DISTHAZARDSMODEL_H

#include "Patch.h"

class DistHazardsModel: public MacroModel {
private:
    Patch* patch;
    // Mosquitos
    double kappa;
    double p0;                      ///< Initial p
    double f0; //initial f, read a from patches file in f0
    long double f;
    double psi;
    long double l;
    double lambda;                  ///< New mosquito generation rate
    int tau;                        ///< Number of days required to complete sporogony
    double UF;                      ///< Number of feeding uninfected mosquitoes
    double UFnew;                   ///< Updated number of feeding uninfected mosquitoes
    double UR;                      ///< Number of resting uninfected mosquitoes
    double URnew;                   ///< Updated number of resting uninfected mosquitoes
    double UL;                      ///< Number of ovipositing uninfected mosquitoes
    double ULnew;                   ///< Number of ovipositing uninfected mosquitoes
    std::vector<double> YF;         ///< Number of feeding infected but not infectious mosquitoes
    std::vector<double> YFnew;      ///< Updated number of feeding infected but not infectious mosquitoes
    std::vector<double> YR;         ///< Number of resting infected but not infectious mosquitoes
    std::vector<double> YRnew;      ///< Updated number of resting infected but not infectious mosquitoes
    std::vector<double> YL;         ///< Number of ovipositing infected but not infectious mosquitoes
    std::vector<double> YLnew;      ///< Updates number of ovipositing infected but not infectious mosquitoes
    double ZF;                      ///< Number of feeding infectious mosquitoes
    double ZFnew;                   ///< Updated number of feeding infectious mosquitoes
    double ZR;                      ///< Number of resting infectious mosquitoes
    double ZRnew;                   ///< Updated Number of resting infectious mosquitoes
    double ZL;                      ///< Number of ovipositing infectious mosquitoes
    double ZLnew;                   ///< Updated number of ovipositing infectious mosquitoes
    double sumZF;                   ///< Total number of infectious mosquitoes in a day (sum accross the iterations or feeding bouts)
    double pF;
    double pR;
    double pL;
public:
    DistHazardsModel(Patch*, double, double, double, double, double, int, double, int);
    DistHazardsModel(const DistHazardsModel& orig);
    virtual ~DistHazardsModel();
    void calculateParameters();
    void updateParameters();
    double getTotMosquitoes();
    double getInfectedButNotInfectiousMosquitoes();
    double getInfectiousMosquitoes();
    void dailyUpdate(double);
    void calculateKappa();
    double getUF();
    double getUR();
    double getUL();
    double getYF(int);
    double getYR(int);
    double getYL(int);
    double getYSum();
    double getYFSum();
    double getYRSum();
    double getYLSum();
    double getZF();
    double getSumZF();
    double getZR();
    double getZL();
    double getPsi();
    double getpF();
    double getpR();
    double getpL();
    double getF();
    double getL();
    double getKappa();
    void calculateUF();
    void calculateUR();
    void calculateUL();
    void calculateYF();
    void calculateYR();
    void calculateYL();
    void calculateZF();
    void calculateZR();
    void calculateZL();
    void calculatepF();
    void calculatepR();
    void calculateF();
    void updateUF();
    void updateUR();
    void updateUL();
    void updateYF();
    void updateYR();
    void updateYL();
    void updateZF();
    void updateZR();
    void updateZL();
    void updatepF();
    void updatepR();
    void updateF();
    void updateHEpsilon(Human *);
    void printState();
    void printMNums(int, int);
};

#endif	/* DISTHAZARDSMODEL_H */

