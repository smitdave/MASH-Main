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
using eventP = std::unique_ptr<event>;

/* human class (abstract base) */
class human {
public:
    
    human(const int id_, const std::string name_) : id(id_), name(name_), alive(true), tnow(0.0) {
        std::cout << "human " << id << ", name: " << name << " born at " << this << std::endl;
    };
    virtual ~human() = 0;
    
    /* move operators */
    human(human&&);
    human& operator=(human&&);
    
    /* copy operators */
    human(human&) = delete;
    human& operator=(human&) = delete;
    
    /* print */
    void print(){
        std::cout << "human " << id << ", name: " << name << " saying hi!" << std::endl;
    }
    
    /* event queue related functions */
    void addEvent2Q(event&& e);
    void rmTagFromQ(const std::string &tag);
    void fireEvent();
    void printEventQ();
    
    /* accessors */
    double              get_tnow(){return tnow;}
    void                set_tnow(const double t){tnow = t;}
    
protected:
    
    u_int               id; /* my id */
    std::string         name;
    bool                alive; /* alive? */
    double              tnow;
    
    std::vector<eventP>  eventQ;
};

#endif /* Human_hpp */
