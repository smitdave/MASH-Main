/* 
 * File:   Patch.h
 * Author: amit
 *
 * Created on September 21, 2011, 2:44 PM
 */

#ifndef PATCH_H
#define	PATCH_H

#include<vector>
#include "MacroModel.h"

class Human;
class DistHazardsModel;
class SimpleRossMacdonaldModel;
/**
 * \brief A class to hold information about patches.
 *
 * Patches contain objects of Human class
 */

class Patch {
private:
    int patchID;                    ///< Patch ID
    std::vector<Human*> humans;	    ///< List of Humans in the Patch
    int nHumans; //needs set        ///< Number of Humans in the Patch
    double effectiveNHumans;        ///< Effective number of Humans in the Patch, given by weighted presence of each Human
    double dTilda; // 1 - \Sum_j{d_j}    
    double sumHExposure; // \psi + \Sum_h{w_h H_{h}}
    int numNeighborsOut; //number of neighbors
    std::vector<int> neighborOutID;
    std::vector<Patch*> neighborsOut;
    std::vector<double> dOut; //diffusion to neighbors
    std::vector<int> * neighborInID;
    std::vector<Patch*> * neighborsIn;
    std::vector<double> * dIn; //diffusion from neighbors
    std::ofstream *pOutFile;
    MacroModel* model;    
    friend class DistHazardsModel;
    friend class SimpleRossMacdonaldModel;
public:
    MacroModel* getModel();
    double getSumHExposure();
    void dailyUpdate(double);
    void calculateModelParams();
    void updateModelParams();
    void updateHEpsilons();
    void calculateDtilda();
    void calculateSumHExposure();
    void calculateEffNHumans();
    void massVaccinate(double, double, double);
    void massDrugAdministrate(int, double);
    void massSpray(int, double);
    void distributeNets(int, double);
    void addNeighborIn(int, Patch*, double);
    void addNeighborOut(int, Patch*, double);
    void addHuman(Human*);
    int getNumInfHumans();
    int getNumNeighborsOut();
    Patch* getNeighborOut(int);
    int getNeighborOutID(int);
    void setNeighborOut(Patch*, int);
    double getDOut(int);
    int getPatchID();
    void printState();
    void printMNums(int, int);
    void outputToFile(int);
    void printHeaderToFile();
    void finishWriting();
    void setInitialInfection(double);
    Patch();
    Patch(int, double, double, double, double, double, int, double, int, int);
    Patch(const Patch& orig);
    virtual ~Patch();
};

#endif	/* PATCH_H */

