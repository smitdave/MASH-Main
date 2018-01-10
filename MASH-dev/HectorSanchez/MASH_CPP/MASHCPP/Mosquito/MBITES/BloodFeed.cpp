//
//  BloodMeal.cpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 11/28/16.
//  Copyright © 2016 MASH. All rights reserved.
//

#include "BloodFeed.hpp"

float randomBloodMealSize(float alpha, float beta){
    //@ Beta-distributed blood meal size (rBloodMealSize)
    return 1;
}

float randomReFeedProbability(float variable, float shapeA, float shapeB){
    //@ Sigmoid-distributed blood meal size (rReFeed.Pr)
    return 1;
}
float bloodOverFeedProbability(float variable,float overFeedA,float overFeedB){
    //@ (pOverFeed)
    float probability=exp(overFeedA*variable)/(overFeedB + exp(overFeedA*variable));
    return probability;
}
bool bloodOverFeedEvent(float variable,float overFeedA,float overFeedB){
    //@ (rOverFeed)
    float probability=bloodOverFeedProbability(variable, overFeedA, overFeedB);
    bool event=randomBinomial(probability);
    return event;
}


//dBloodMealSize = function(B, bm.a=7.5, bm.b=2.5){
//    X = dbeta(B,bm.a,bm.b)
//}
//
//rBloodMealSize = function(bm.a=7.5, bm.b=2.5, N=1){
//    rbeta(N,bm.a,bm.b)
//}
//
//eggMaturationTime.rnorm = function(emt.m = 3, emt.V=.1){rnorm(1, emt.m, emt.V)}
//eggMaturationTime.off = function(){0}
//
//
//######################################
//#  Egg Batch Size
//######################################
//BatchSize.pdf = function(M,bs.m=30, bs.v=5){
//    round(rnorm(1, bs.m, bs.v))
//}
//
//BatchSize.bms = function(M){
//    M$bmSize*dhmP$maxBatch
//}
//
//
//##############################
//#  REFEED Functions
//##############################
//reFeed = function(M){
//    if(dhmP$REFEED & isAlive(M))
//        M$bStateNew = reFeedF.Pr(M)
//        M}
//
//##############################
//#  reFeedF.Pr
//##############################
//pReFeed = function(B, rf.a=18, rf.b=10000){
//    (2+rf.b)/(1+rf.b) - exp(rf.a*B)/(rf.b + exp(rf.a*B))
//}
//
//rReFeed.Pr = function(B, rf.a=18, rf.b=10000){
//# 1 = refeed :: 0 = done
//    rbinom(1,1,pReFeed(B, rf.a, rf.b))
//}
//
//reFeedF.Pr = function(M){
//    ifelse(rbinom(1,1,rReFeed.Pr(M$bmSize)),"B", "L")
//}
//
//##############################
//#  reFeedF.Egg
//##############################
//reFeedF.Egg = function(M){
//    if(M$tnow > M$eggT & M$eggP > dhmP$eggP.min){
//        M$bStateNew = "L"
//    } else {
//        M$bStateNew = "B"
//    }
//}
//
//##############################
//#  OVERFEED Functions
//##############################
//

//
//overFeed = function(M){
//    if(rOverFeed(M$bmSize, dhmP$of.a, dhmP$of.b))
//        M$bStateNew = "D"
//        M}
//
//##############################
//#  The Blood Meal
//##############################
//
//BloodMeal = function(M){with(dhmP,{
//    M$bmSize = rBloodMealSize(bm.a,bm.b)
//    M = bloodEnergetics(M) # defined in DHM-Sugar.R
//    if(OVERFEED) M=overFeed(M)
//        if(M$batch == 0){
//            M$batch  = BatchSize(M)
//            M$eggT  = M$tnow + eggMaturationTime()
//        }
//    M$eggP = M$eggP + M$bmSize
//    M})}
