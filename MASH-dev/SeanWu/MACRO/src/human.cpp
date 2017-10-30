#include "human.hpp"

// properties of human
human::human(const int &id_new){
    id = id_new;
    event_queue.reserve(100);
    #ifdef DEBUG_INFSIM
    std::cout << "human " << id << " being born at memory location: " << this << std::endl;;
    #endif
};

human::~human(){
    event_queue.clear();
    #ifdef DEBUG_INFSIM
    std::cout << "human " << id << " getting killed at memory location: " << this << std::endl;
    #endif
};

int human::get_id(){
    return(id);
};

std::string human::get_state(){
    return(state);
};

void human::set_state(const std::string &state_new){
    state = state_new;
};

bool human::get_inf(){
    return(inf);
};

void human::set_inf(const bool &i){
    inf = i;
};


// address
address human::get_home_address(){
    return(home_address);
};

void human::set_home_address(patch* p, tile* t){
    std::get<0>(home_address) = p;
    std::get<1>(home_address) = t;
};

patch* human::get_home_patch(){
    return std::get<0>(home_address);
};

tile* human::get_home_tile(){
    return std::get<1>(home_address);
};

address human::get_current_address(){
    return(current_address);
};

void human::set_current_address(patch* p, tile* t){
    std::get<0>(current_address) = p;
    std::get<1>(current_address) = t;
};

void human::set_current_patch(patch* p){
  std::get<0>(current_address) = p;
}

void human::set_current_tile(tile* t){
  std::get<1>(current_address) = t;
}

patch* human::get_current_patch(){
    return std::get<0>(current_address);
};

tile* human::get_current_tile(){
    return std::get<1>(current_address);
};

// pathogen
pathogen* human::get_pathogen(){
    return(pathogen_ptr);
}

void human::set_pathogen(pathogen* p){
    // p->set_human_ptr(this);
    pathogen_ptr = p;
}

// event queue
void human::add2Q_set_state(const std::string &state_new){
  
  // event_queue.push_back([] {})
  // q.push_back( [&x] { x.foo1(1); } );
}

//debug
void human::get_memLoc(){
    std::cout << "human " << id << " at memory location: " << this << std::endl;
};
