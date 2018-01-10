//
//  Flight.cpp
//  MASHCPP
//


#include "Flight.hpp"

float getFlightSurvivalProbability(char mosquitoState){
    float survivalProbability=0;
    switch(mosquitoState){
        case(F):
            survivalProbability=MBITES_FP;
            break;
        case(B):
            survivalProbability=MBITES_BP;
            break;
        case(R):
            survivalProbability=MBITES_RP;
            break;
        case(L):
            survivalProbability=MBITES_LP;
            break;
        case(O):
            survivalProbability=MBITES_OP;
            break;
        case(M):
            survivalProbability=MBITES_MP;
            break;
        case(S):
            survivalProbability=MBITES_SP;
            break;
        case(E):
            survivalProbability=MBITES_EP;
            break;
        defaut:
            break;
    }
    return survivalProbability;
}
