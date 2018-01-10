//
//  StatisticalFunctions.hpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 12/28/16.
//  Copyright © 2016 MASH. All rights reserved.
//

#ifndef StatisticalFunctions_hpp
#define StatisticalFunctions_hpp

#include <stdio.h>
#include "boost/random.hpp"
#include "./StatisticalConstants.hpp"

void runBoostExamples();
//Boost Random Functions
int randomUniformInt(mt_generator generator, int min, int max);
float randomFloatZeroToOne(mt_generator generator);
bool randomBinomial(float probability);
float randomBeta(mt_generator generator, float alpha, float beta);
float randomNormal(mt_generator generator,float mean,float sd);

#endif /* StatisticalFunctions_hpp */
