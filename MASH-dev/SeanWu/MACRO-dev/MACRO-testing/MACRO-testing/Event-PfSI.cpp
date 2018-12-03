//
//  Event-PfSI.cpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/26/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#include "Event-PfSI.hpp"
#include "Human-PfSI.hpp"

/* infection event */

/* constructor */
e_pfsi_infect::e_pfsi_infect(double tEvent_, human_pfsi* h):
    event("PfSI_infection",tEvent_,[tEvent_,h](){
        h->set_state("I");
        h->set_tnow(tEvent_);
    })
{
    std::cout << "e_pfsi_infect constructor being called at " << this << std::endl;
};

/* destructor */
e_pfsi_infect::~e_pfsi_infect(){
    std::cout << "e_pfsi_infect destructor being called at " << this << std::endl;
};


/* recovery event */

/* constructor */
e_pfsi_recover::e_pfsi_recover(double tEvent_, human_pfsi* h):
    event("PfSI_recovery",tEvent_,[tEvent_,h](){
        h->set_state("S");
        h->set_tnow(tEvent_);
    })
{
    std::cout << "e_pfsi_recover constructor being called at " << this << std::endl;
};

/* destructor */
e_pfsi_recover::~e_pfsi_recover(){
    std::cout << "e_pfsi_recover destructor being called at " << this << std::endl;
};

/* fever event */

/* constructor */
e_pfsi_fever::e_pfsi_fever(double tEvent_, double fever_, human_pfsi* h):
    event("PFSI_fever",tEvent_,[h,fever_](){
        h->set_fever(fever_);
    })
{ 
    std::cout << "e_pfsi_fever constructor being called at " << this << std::endl;
};

/* destructor */
e_pfsi_fever::~e_pfsi_fever(){
    std::cout << "e_pfsi_fever destructor being called at " << this << std::endl;
};
