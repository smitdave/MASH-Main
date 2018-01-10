//
//  SiteSugar.cpp
//  MASH
//

#include "SiteSugar.hpp"

SiteSugar::SiteSugar(){
    //@ Default constructor
    position={0,0,0};
    sugarQuantity=100;
    atsb=ATSB();
    areaRepellant=AreaRepellant();
    areaSpray=AreaSpray();
}
void SiteSugar::setSugar(float sugarQuantityIn){
    //@ Sets sugar level to a given value
    sugarQuantity = sugarQuantityIn;
}
void SiteSugar::increaseSugar(float sugarIncrease){
    //@ Increases sugar quantity by a given amount
    sugarQuantity = sugarQuantity + sugarIncrease;
}
void SiteSugar::decreaseSugar(float sugarDecrease){
    //@ Decreases sugar quantity by a given amount
    sugarQuantity = sugarQuantity - sugarDecrease;
}
float SiteSugar::getSugar(){
    //@ Gets sugar quantity
    return sugarQuantity;
}
ATSB SiteSugar::getATSB(){
    //@ Returns ATSB
    return atsb;
}
