//
//  AuxiliaryFunctions.c
//  MASHCPP
//

#include "AuxiliaryFunctions.hpp"
#include "math.h"

float sigmoidA(float variable, float shapeA, float shapeB){
    //@ Generates a flexible sigmoid shape (pOverFeed)
    float num = exp(shapeA * variable);
    float den = shapeB + exp(shapeA * variable);
    return (num/den);
}
float sigmoidB(float variable, float shapeA, float shapeB){
    //@ Generates a more flexible sigmoid shape (pReFeed)
    float partA = (2+shapeB)/(1+shapeB);
    float partB = exp(shapeA * variable)/(shapeB + exp(shapeA * variable));
    return (partA - partB);
}
double calculateEuclideanDistance(coordinate positionA, coordinate positionB){
    //@ Calculates the euclidean distance between two three-dimensional coordinates
    double partialX = pow(positionA.x-positionB.x,2);
    double partialY = pow(positionA.y-positionB.y,2);
    double partialZ = pow(positionA.z-positionB.z,2);
    double distance = sqrt(partialX+partialY+partialZ);
    return distance;
}
