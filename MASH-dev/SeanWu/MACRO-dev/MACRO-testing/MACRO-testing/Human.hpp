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

#include <string>
#include <vector>

#include <memory>

/* forward declaration */
class event;

/* human class */
class human {
public:
    
    human(const int id_, const std::string name_) : id(id_), name(name_), alive(true) {
        std::cout << "human " << id << ", name: " << name << " born at " << this << std::endl;
    };
    ~human(){
        std::cout << "human " << id << ", name: " << name << " dying at " << this << std::endl;
    };
    
    /* move operators */
    human(human&&);
    human& operator=(human&&);
    
    /* copy operators */
    human(human&) = delete;
    human& operator=(human&) = delete;
    
    /* event queue related functions */
    void addEvent2Q(const event& e);
    void rmTagFromQ(const std::string &tag);
    void fireEvent();
    void printEventQ();
    
private:
    
    u_int               id; /* my id */
    std::string         name;
    bool                alive; /* alive? */
    
    std::vector<event>  eventQ;
};

#endif /* Human_hpp */
