//  ***************************************************************************
//  AuxiliaryFunctions.h
//  MASHCPP
//  Generic functions that are used for various purposes throughout different
//  files and routines of the simulation.
//  ***************************************************************************

#ifndef AuxiliaryFunctions_hpp
#define AuxiliaryFunctions_hpp

#include <iostream>
#include <stdio.h>
#include "../GlobalParameters/Globals.hpp"

float sigmoidA(float variable, float shapeA, float shapeB);
float sigmoidB(float variable, float shapeA, float shapeB);
double calculateEuclideanDistance(coordinate positionA, coordinate positionB);

#endif
