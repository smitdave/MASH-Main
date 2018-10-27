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
    event(std::string tag_, double tEvent_, std::function<void()> eventF_);
    
    /* destructor */
    ~event();
    
    /* move operators */
    event(event&&) = default;
    event& operator=(event&&) = default;
    
    /* copy operators */
    event(event&) = default;
    event& operator=(event&) = default;
    
    /* print (debugging) */
    void print(){
        std::cout << "event -- tag: " << tag << ", tEvent: " << tEvent << std::endl;
    };
    
    /* comparison for sorting */
    bool operator<(event e) const {
        return tEvent < e.tEvent;
    };
    
    /* information for event */
    std::string                        tag;
    double                             tEvent;
    std::function<void()>            eventF;
    
};




#endif /* Event_hpp */
