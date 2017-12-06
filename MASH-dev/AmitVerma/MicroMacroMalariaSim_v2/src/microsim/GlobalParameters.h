/*
 * GlobalParameters.h
 *
 *  Created on: May 18, 2010
 *      Author: itamar
 */

/**
 * \file GlobalParameters.h
 * \brief This file contains the global parameters
 */


#ifndef GLOBALPARAMETERS_H_
#define GLOBALPARAMETERS_H_

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
#include<cmath>
#include<sstream>
#include <stdexcept>
#include <random>

using namespace std;

class Patch;
class Drug;

// Global functions
extern Patch * findPatch(int pID);

// Global variables
extern int currentDay;  ///< this variable defines the day in the simulation
extern bool PF_MODEL;
extern Drug useDrug;


extern int nBites;
extern int muhu, mihu, mihu_it, muhi, muhi_it, mihi, mihi_it;
extern int windForecast;

extern int nSearchHouse;
extern int nSearchPond;
extern ofstream biteLog;
extern ofstream clinicLog;

#endif /* GLOBALPARAMETERS_H_ */
