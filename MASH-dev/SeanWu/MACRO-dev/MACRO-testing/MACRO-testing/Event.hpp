//
//  Event.hpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/25/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#ifndef Event_hpp
#define Event_hpp

#include <stdio.h>
#include <iostream>

#include <string>
#include <functional>

class event {
public:
    
    /* constructor */
    event(std::string _tag, double _tEvent, std::function<void()> _eventF, void* _eventD):
    tag(_tag),tEvent(_tEvent),eventF(_eventF),eventD(_eventD) {
        std::cout << "event constructor being called at " << this << std::endl;
    };
    
    /* destructor */
    ~event(){
        std::cout << "event destructor being called at " << this << std::endl;
    };
    
    /* move operators */
    event(event&&) = delete;
    event& operator=(event&&) = delete;
    
    /* copy operators */
    event(event&) = delete;
    event& operator=(event&) = delete;
    
    /* information for event */
    std::string             tag;
    double                  tEvent;
    std::function<void()>   eventF;
    void*                   eventD;
    
};

#endif /* Event_hpp */
