//
//  Senesce.cpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 1/18/17.
//  Copyright © 2017 MASH. All rights reserved.
//

#include "Senesce.hpp"

float senesceProbability(float A, float senesceA, float senesceB){
    //@ Returns the senesce probability of a mosquito (equivalent to pSenesce in MASHR)
    float probability=sigmoidB(A, senesceA, senesceB);
    return probability;
}
bool senesceEvent(float A, float senesceA, float senesceB){
    //@ (rSenesce)
    float senesceP=senesceProbability(A,senesceA,senesceB);
    bool randomBinomialBasic(senesceP);
    return randomBinomialBasic;
}
float senesce(float tMNow,float tMBirthday, float senesceA, float senesceB){
    //@ (Senesce)
    float probability=senesceProbability(tMNow-tMBirthday,senesceA,senesceB);
    return probability;
}
