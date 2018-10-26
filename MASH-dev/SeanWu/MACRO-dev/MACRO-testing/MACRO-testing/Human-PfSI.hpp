//
//  Human-PfSI.hpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/26/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#ifndef Human_PfSI_hpp
#define Human_PfSI_hpp

#include <stdio.h>
#include <iostream>
#include <string>

#include "Human.hpp"

class human_pfsi : public human {
    
    /* events are friends */
    friend class e_pfsi_infect;
    
public:
    human_pfsi(const int id_, const std::string name_);
    ~human_pfsi();
    
    /* move operators */
    human_pfsi(human_pfsi&&);
    human_pfsi& operator=(human_pfsi&&);
    
    /* copy operators */
    human_pfsi(human_pfsi&) = delete;
    human_pfsi& operator=(human_pfsi&) = delete;
    
    /* print */
    void print(){
        human::print();
        std::cout << "my state is: " << state << std::endl;
    }
    
    /* accessors */
    void        set_state(const std::string& stateN){ state = stateN; }
    std::string get_state(){ return state; }
    
private:
    std::string state;
};

#endif /* Human_PfSI_hpp */
