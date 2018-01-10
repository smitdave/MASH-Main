//
//  SugarFeed.cpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 11/28/16.
//  Copyright © 2016 MASH. All rights reserved.
//

#include "SugarFeed.hpp"

float sugarFeedProbability(float w, float s, float SSb, float SSa, float SSz){
    //@@ Returns the sugar feed probability according to...
    float probability = (2+SSb*(1+w))/(1+SSb*(1+w)) - exp(SSa*s/(1+SSz*w))/(SSb*(1+w)+exp(SSa*s/(1+SSz*w)));
    return probability;
}

