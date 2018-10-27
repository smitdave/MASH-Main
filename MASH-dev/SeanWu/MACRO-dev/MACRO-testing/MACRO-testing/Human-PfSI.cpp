//
//  Human-PfSI.cpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/26/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#include "Human-PfSI.hpp"
#include "Event.hpp"
#include "Event-PfSI.hpp"

human_pfsi::human_pfsi(const int id_, const std::string name_) :
    human(id_,name_), state("S"), fever(0)
{
    std::cout << "human_pfsi birthed at " << this << std::endl;
};

human_pfsi::~human_pfsi(){
    std::cout << "human_pfsi dying at " << this << std::endl;
};

/* move operators */
human_pfsi::human_pfsi(human_pfsi&&) = default;
human_pfsi& human_pfsi::operator=(human_pfsi&&) = default;
