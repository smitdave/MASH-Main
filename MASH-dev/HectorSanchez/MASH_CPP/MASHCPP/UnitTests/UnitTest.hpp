//  ***************************************************************************
//  UnitTest.h
//  MASHCPP
//  Routines that should determine if functions are working correctly. The full
//  test (runTests) should be called from the main. All other tests should be
//  called from runTests. The variable totalErrorCount should return the number
//  of found errors.
//  ***************************************************************************

#ifndef UnitTest_hpp
#define UnitTest_hpp

#include <stdio.h>
#import "../Imported.hpp"

int runTests();
int runTestsAuxiliaryFunctions();
int runTestsMosquito();
int runTestsSite();
int runTestsVectorControl();
int runTestsCattle();
int runTestsHuman();
int runtTestsMosquitoPopulation();

#endif
