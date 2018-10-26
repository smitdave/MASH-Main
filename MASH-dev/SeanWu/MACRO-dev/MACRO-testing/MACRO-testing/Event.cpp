//
//  Event.cpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/25/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#include "Event.hpp"

/* constructor */
event::event(std::string _tag, double _tEvent, std::function<void(const void*)> _eventF, void* _eventD):
tag(_tag),tEvent(_tEvent),eventF(_eventF),eventD(_eventD) {
    std::cout << "event constructor being called at " << this << std::endl;
};

/* destructor */
event::~event(){
    std::cout << "event destructor being called at " << this << std::endl;
};



