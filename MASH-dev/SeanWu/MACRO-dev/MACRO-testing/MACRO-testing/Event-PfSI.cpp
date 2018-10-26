//
//  Event-PfSI.cpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/26/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#include "Event-PfSI.hpp"
#include "Human-PfSI.hpp"

e_pfsi_infect::e_pfsi_infect(double tEvent_, human_pfsi* h):
    event("PfSI_infection",tEvent_,std::bind(&human_pfsi::set_state,h,"I"),nullptr)
{
    std::cout << "e_pfsi_infect constructor being called at " << this << std::endl;
};

///* constructor */
//e_pfsi_infect::e_pfsi_infect(std::string tag_, double tEvent_, std::function<void(const void*)> eventF_):
//    event(tag_,tEvent_,eventF_,nullptr)
//{
//    std::cout << "e_pfsi_infect constructor being called at " << this << std::endl;
//};

/* destructor */
e_pfsi_infect::~e_pfsi_infect(){
    std::cout << "e_pfsi_infect destructor being called at " << this << std::endl;
};
