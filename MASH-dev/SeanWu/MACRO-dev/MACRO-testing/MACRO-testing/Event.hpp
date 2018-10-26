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
    event(std::string _tag, double _tEvent, std::function<void(const void*)> _eventF, void* _eventD);
    
    /* destructor */
    ~event();
    
//    /* move operators */
//    event(event&&);
//    event& operator=(event&&);
//    
//    /* copy operators */
//    event(event&);
//    event& operator=(event&);
    
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
    std::function<void(const void*)>   eventF;
    void*                              eventD; /* pack of data (optional) for eventF function */
    
};




#endif /* Event_hpp */
