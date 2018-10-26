//
//  Human.cpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/25/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#include "Human.hpp"
#include "Event.hpp"

/* move operators */
human::human(human&&) = default;
human& human::operator=(human&&) = default;

/* event queue related functions */
void human::addEvent2Q(const event& e){
    eventQ.emplace_back(e);
    std::sort(eventQ.begin(),eventQ.end());
};

void human::rmTagFromQ(const std::string &tag){
    
};

void human::fireEvent(){
    
};

void human::printEventQ(){
    
};
