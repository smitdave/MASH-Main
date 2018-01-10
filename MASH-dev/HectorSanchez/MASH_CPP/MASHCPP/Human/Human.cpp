//
//  Human.cpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 12/9/16.
//  Copyright © 2016 MASH. All rights reserved.
//

#include "Human.hpp"

Human::Human(){
    id = 0;
    itn = new InsecticideTreatedNet();
    personalRepellant = new PersonalRepellant();
    personalProtection = new PersonalProtection();
}
int Human::getID(){
    return id;
}
InsecticideTreatedNet Human::getInsecticideTreatedNet(){
    return *itn;
}
PersonalProtection Human::getPersonalProtection(){
    return *personalProtection;
}
PersonalRepellant Human::getPersonalRepellant(){
    return *personalRepellant;
}
Human::~Human(){
    delete itn;
    delete personalRepellant;
    delete personalProtection;
}
