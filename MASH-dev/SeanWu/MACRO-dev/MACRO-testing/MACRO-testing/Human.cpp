//
//  Human.cpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/25/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#include "Human.hpp"
#include "Event.hpp"

/* define virtual destructor is ok */
human::~human(){
    std::cout << "human " << id << ", name: " << name << " dying at " << this << std::endl;
};

/* move operators */
human::human(human&&) = default;
human& human::operator=(human&&) = default;

/* event queue related functions */
void human::addEvent2Q(event&& e){
    eventQ.emplace_back(std::make_unique<event>(e));
    std::sort(eventQ.begin(),eventQ.end());
};

void human::rmTagFromQ(const std::string &tag){
    eventQ.erase(std::remove_if(eventQ.begin(),eventQ.end(),
                                [tag](eventP& e){
                                    return(tag.compare(e->tag)==0);
                                }));
};

void human::fireEvent(){
    if(eventQ.size() > 0){
        eventQ.front()->eventF(eventQ.front()->eventD);
        eventQ.erase(eventQ.begin());
    }
};

void human::printEventQ(){
    std::cout << "printing human " << id << ", name: " << name << ", event queue: " << std::endl;
    for(auto it = eventQ.begin(); it != eventQ.end(); it++){
        (*it)->print();
    }
};