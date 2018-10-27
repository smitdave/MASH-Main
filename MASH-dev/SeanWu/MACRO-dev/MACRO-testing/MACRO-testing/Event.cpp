//
//  Event.cpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/25/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#include "Event.hpp"

/* constructor */
event::event(std::string tag_, double tEvent_, std::function<void()> eventF_):
tag(tag_),tEvent(tEvent_),eventF(eventF_) {
    std::cout << "event constructor being called at " << this << std::endl;
};

/* destructor */
event::~event(){
    std::cout << "event destructor being called at " << this << std::endl;
};



