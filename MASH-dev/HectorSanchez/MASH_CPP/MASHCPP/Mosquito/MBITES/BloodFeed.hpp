//
//  BloodFeed.hpp
//  MASHCPP
//


#ifndef BloodFeed_hpp
#define BloodFeed_hpp

#include <stdio.h>
#include <math.h>
#include "../../AuxiliaryFunctions/AuxiliaryFunctions.hpp"
#include "../../AuxiliaryFunctions/StatisticalFunctions.hpp"

float randomBloodMealSize(float alpha, float beta);
float randomReFeedProbability(float variable, float shapeA, float shapeB);
float bloodOverFeedProbability(float variable,float overFeedA,float overFeedB);
bool bloodOverFeedEvent(float variable,float overFeedA,float overFeedB);

#endif /* BloodFeed_hpp */
