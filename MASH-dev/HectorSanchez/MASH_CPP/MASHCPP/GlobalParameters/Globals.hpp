//  ***************************************************************************
//  Globals.hpp
//  MASH
//  Contains various definitions for constants used throughout the simulation
//  ***************************************************************************

#ifndef Globals_hpp
#define Globals_hpp

#include <stdio.h>
//Set this following line to false if no tests are required
const bool PERFORM_UNIT_TESTS = true;
// ***** Defining Mosquito States as Chars ********//
const char M = 'M';     //@@ Defining mating state as M for code clarity
const char F = 'F';     //@@ Defining blood-search state as F for code clarity
const char B = 'B';     //@@ Defining blood-feed state as B for code clarity
const char R = 'R';     //@@ Defining resting state as R for code clarity
const char L = 'L';     //@@ Defining egg-laying search state as L for code clarity
const char O = 'O';     //@@ Defining oviposition state as O for code clarity
const char S = 'S';     //@@ Defining sugar-feeding state as S for code clarity
const char D = 'D';     //@@ Defining dead state as D for code clarity
const char E = 'E';     //@@ Defining estivation state as E for code clarity
// ***** Defining coordinates structure ***********//
typedef struct {
    double x,y,z;
} coordinate;

#endif
