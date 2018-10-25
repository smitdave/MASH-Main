//
//  Human.hpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/25/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#ifndef Human_hpp
#define Human_hpp

#include <stdio.h>
#include <iostream>

#include <vector>

/* forward declaration */
class event;

/* human class */
class human {
public:
    
    human(const int id_) : id(id_), alive(true) {};
    ~human(){};
    
    /* move operators */
    human(human&&);
    human& operator=(human&&);
    
    /* copy operators */
    human(human&) = delete;
    human& operator=(human&) = delete;
    
private:
    
    u_int               id; /* my id */
    bool                alive; /* alive? */
    
    std::vector<event>  eventQ;
};

#endif /* Human_hpp */
