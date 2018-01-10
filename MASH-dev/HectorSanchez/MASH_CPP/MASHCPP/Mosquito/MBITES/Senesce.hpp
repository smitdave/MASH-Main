//
//  Senesce.hpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 1/18/17.
//  Copyright © 2017 MASH. All rights reserved.
//

#ifndef Senesce_hpp
#define Senesce_hpp

#include <stdio.h>
#include "boost/random.hpp"
#include "../../AuxiliaryFunctions/AuxiliaryFunctions.hpp"
#include "../../AuxiliaryFunctions/StatisticalFunctions.hpp"

float senesceProbability(float A, float senesceA, float senesceB);
bool senesceEvent(float A, float senesceA, float senesceB);
float senesce(float tMNow,float tMBirthday);

#endif /* Senesce_hpp */
