//
//  Event-PfSI.hpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/26/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#ifndef Event_PfSI_hpp
#define Event_PfSI_hpp

#include <stdio.h>
#include <functional>

using namespace std::placeholders;

#include "Event.hpp"

class human_pfsi;

/* infect a human */
class e_pfsi_infect : public event {
public:
    /* constructor */
    e_pfsi_infect(double tEvent_, human_pfsi* h);
    
    /* destructor */
    ~e_pfsi_infect();
};

/* human recovers */
class e_pfsi_recover : public event {
public:
    /* constructor */
    e_pfsi_recover(double tEvent_, human_pfsi* h);
    
    /* destructor */
    ~e_pfsi_recover();
};

/* fever event */
class e_pfsi_fever : public event {
public:
    /* constructor */
    e_pfsi_fever(double tEvent_, double fever_,human_pfsi* h);
    
    /* destructor */
    ~e_pfsi_fever();
};


#endif /* Event_PfSI_hpp */