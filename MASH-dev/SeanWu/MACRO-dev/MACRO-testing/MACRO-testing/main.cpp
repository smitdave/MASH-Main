//
//  main.cpp
//  MACRO-testing
//
//  Created by Sean Wu on 10/25/18.
//  Copyright © 2018 Sean Wu. All rights reserved.
//

#include <iostream>
#include <vector>
#include <functional>
#include <memory>

#include "Human-PfSI.hpp"
#include "Event-PfSI.hpp"

int main(){
    
    std::unique_ptr<human_pfsi> bob = std::make_unique<human_pfsi>(1,"bob");
    std::unique_ptr<human_pfsi> alice = std::make_unique<human_pfsi>(2,"alice");
    
    std::cout << std::endl;
    bob->print();
    alice->print();
    
    std::cout << std::endl;
    alice->addEvent2Q(e_pfsi_recover(5.46,alice.get()));
    alice->addEvent2Q(e_pfsi_infect(1.23,alice.get()));
    alice->addEvent2Q(e_pfsi_fever(3.26,10.0,alice.get()));
    std::cout << " START PRINTING ALICE'S EVENT QUEUE " << std::endl;
    alice->printEventQ();
    std::cout << " DONE PRINTING ALICE'S EVENT QUEUE " << std::endl;
    std::cout << std::endl;
    
    std::cout << " FIRING ALICE'S EVENT QUEUE " << std::endl;
    std::cout << "alice fever " << alice->get_fever() << std::endl;
    alice->fireEvent();
    alice->print();
    alice->printEventQ();
    alice->fireEvent();
    alice->print();
    std::cout << "alice fever " << alice->get_fever() << std::endl;
    
    std::cout << " START PRINTING ALICE'S EVENT QUEUE " << std::endl;
    alice->printEventQ();
    std::cout << " DONE PRINTING ALICE'S EVENT QUEUE " << std::endl;
    std::cout << std::endl;
    
    std::cout << std::endl;
    std::cout << " STOP PROGRAM " << std::endl;
    return 0;
}
