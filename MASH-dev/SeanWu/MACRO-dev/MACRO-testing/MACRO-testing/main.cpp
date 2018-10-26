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

using namespace std::placeholders;

#include "Event.hpp"

/* a tiny human class to check on */
class tinyhuman {
public:
    tinyhuman(std::string id_): id(id_), alive(true) {
        std::cout << "tinyhuman " << id << ", born at " << this << std::endl;
    };
    ~tinyhuman(){
        std::cout << "tinyhuman " << id << ", dying at " << this << std::endl;
    }
    
    void set_alive(void* in){
        bool* life = static_cast<bool*>(in);
        alive = *life;
    };
    void kill(void* in){
        alive = false;
    }
    void print_alive(){
        std::cout << "tinyhuman " << id << ", alive: " << alive << std::endl;
    };
    void print_state(){
        std::cout << "tinyhuman " << id << ", state: " << state << std::endl;
    };
    
    
private:
    std::string id;
    bool alive;
    std::string state;
};

/* main */
int main(int argc, const char * argv[]) {
    
    
    tinyhuman* bob = new tinyhuman("bob");
    tinyhuman* alice = new tinyhuman("alice");
    
    
    bob->print_alive();
    
    bool a = false;
    void* aa = static_cast<bool*>(&a);
    bob->set_alive(aa);
    
    bob->print_alive();
    
    a = true;
    bob->set_alive(&a);
    
    bob->print_alive();
    
    bob->kill(nullptr);
    bob->print_alive();
    
    

//    eventQ.push_back();
    
    delete bob;
    delete alice;
    
    return 0;
}
