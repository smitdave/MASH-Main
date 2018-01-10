//
//  Mosquito.cpp
//  MASH
//

#include "Mosquito.hpp"

//******* Constructors ******************//
//Mosquito::Mosquito(){
//    //@ Basic constructor of mosquito class that returns a dead individual with no characteristics
//    state=D;
//    age=0;
//    damage=0;
//    timeBorn=0;
//    anthropophilia=1;
//    gm=GM();
//}
//Mosquito::Mosquito(char stateIn){
//    //@ Basic constructor of mosquito class that returns a dead individual with no characteristics
//    state=stateIn;
//    age=0;
//    damage=0;
//    timeBorn=0;
//    anthropophilia=1;
//    gm=GM();
//}
//******* Accessors *********************//
char Mosquito::getState(){
    //@ Returns mosquito's current state
    return state;
}
GM Mosquito::getGM(){
    //@ Returns GM
    return gm;
}
//******* Mutators **********************//
void Mosquito::setState(char stateIn){
    //@ Mutator for mosquito's current state
    state=stateIn;
}
//******* Main Actions ******************//
void Mosquito::boutD(){
    //@ Kills mosquito (should clean from memory)
    setState(D);
}
void Mosquito::boutR(){
    //@ Performs the activities that regard the resting behaviour of mosquitoes
    if(true){//randomBinomial(DHMP_RD)){
        //Mosquito can die from random events while resting
        setState(D);
    }else{
        //ReFeed Event
    }
}
void Mosquito::boutE(){
    //@ Performs the estivation routines of mosquitoes
    setState(F);
}
void Mosquito::boutS(){
    //@ Performs sugar feeding mosquito cycle
    
}
//******* Auxiliary Actions *************//
