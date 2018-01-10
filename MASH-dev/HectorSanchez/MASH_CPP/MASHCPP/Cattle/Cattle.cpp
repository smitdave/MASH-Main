//
//  Cattle.cpp
//  MASHCPP
//

#include "Cattle.hpp"

Cattle::Cattle(){
    //@ Cattle void constructor
    quantity=0;
    updateAttractiveness();
    ivermectin=Ivermectin();
    zoospray=Zoospray();
}
Cattle::Cattle(float quantityIn){
    //@ Cattle quantity constructor
    quantity=quantityIn;
    updateAttractiveness();
}
void Cattle::updateAttractiveness(){
    //@ Should calculate the attractiveness of the cattle as a function of the cattle quantity (should probably also depend on a parameter Q that represents antropophilia on mosquito part
    attractiveness=quantity;
}
float Cattle::getQuantity(){
    //@ Returns the cattle quantity
    return quantity;
}
float Cattle::getAttractiveness(){
    //@ Returns cattle attractiveness as a group
    return attractiveness;
}
Ivermectin Cattle::getIvermectin(){
    //@ Returns cattle Ivermectin object
    return ivermectin;
}
Zoospray Cattle::getZoospray(){
    //@ Returns cattle Zoosprau object
    return zoospray;
}
