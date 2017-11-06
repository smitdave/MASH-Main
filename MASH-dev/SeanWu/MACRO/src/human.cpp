/*
 * ################################################################################
 *
 *        __  __
 *       / / / /_  ______ ___  ____ _____
 *      / /_/ / / / / __ `__ \/ __ `/ __ \
 *     / __  / /_/ / / / / / / /_/ / / / /
 *    /_/ /_/\__,_/_/ /_/ /_/\__,_/_/ /_/
 *
 *    Human Class Implementation
 *    MASH Team
 *    October 2017
 *
 * ################################################################################
*/

#include "human.hpp"
#include "humanPop.hpp"

/*
 * ################################################################################
 *    Constructor & Destructor
 * ################################################################################
 */

human::human(const int &id_new){
    id = id_new;
    event_queue.reserve(100); // get rid of later
    eventQ.reserve(100);
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

bool human::get_alive(){
  return alive;
};

void human::set_alive(const bool &a){
  alive = a;
};


/*
 * ################################################################################
 *    Address
 * ################################################################################
*/

humanPop* human::get_pop_ptr(){
  return pop_ptr;
};

void human::set_pop_ptr(humanPop* h){
  pop_ptr = h;
};

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


/*
 * ################################################################################
 *    Pathogen
 * ################################################################################
 */

pathogen* human::get_pathogen(){
    return(pathogen_ptr);
}

void human::set_pathogen(pathogen* p){
    // p->set_human_ptr(this);
    pathogen_ptr = p;
}


/*
 * ################################################################################
 *    Event Queue
 * ################################################################################
*/

// inline definition need to sort the queue
inline bool compare_tEvent(const event& eventA, const event& eventB) { return eventA.tEvent < eventB.tEvent; }

void human::addEvent2Q(const event &e){
    eventQ.push_back(e);
    std::sort(eventQ.begin(),eventQ.end(),compare_tEvent);
};

void human::rmTagFromQ(const std::string &tag){
    eventQ.erase(std::remove_if(
      eventQ.begin(),eventQ.end(),
      [tag](const event& e){
        return(tag.compare(e.tag)==0);
      }
    ));
};

void human::fireEvent(){
  eventQ.front().eventF();
  eventQ.erase(eventQ.begin());
};

void human::printEventQ(){
  std::cout << "printing event queue for human: " << id << std::endl;
  for(auto it = eventQ.begin(); it != eventQ.end(); it++){
    std::cout << "tag: " << (*it).tag << std::endl;
    std::cout << "tEvent: " << (*it).tEvent << std::endl;
  }
};

void human::add2Q_set_state(const double &tEvent, std::string s){
  this->addEvent2Q(
    event("setState",tEvent,std::bind(&human::set_state,this,s))
  );
};


/*
 * ################################################################################
 *    Other
 * ################################################################################
*/

void human::death(){
  pop_ptr->get_pop().erase(id);
};


/*
 * ################################################################################
 *    DEBUG
 * ################################################################################
*/

// DEBUG
void human::get_memLoc(){
    std::cout << "human " << id << " at memory location: " << this << std::endl;
};

void human::add2Q_set_stateTest(const double &tEvent, std::string state_new){

  // std::function<void()> boundF = std::bind(&human::set_state,state_new);
  // std::bind(&human::set_state,state_new);
  event_queue.push_back(std::bind(&human::set_state,this,state_new));
  // event_queue.push_back()

  // deferred.push_back(std::bind(&Class1::action1, this, arg1, arg2));
};

void human::fireEventTest(){
  for(auto f : event_queue){
    f();
  }
};

void human::check_inf(){
    if(pathogen_ptr == nullptr){
        std::cout << "i have no pathogens in me!" << std::endl;
    } else {
        std::cout << "i'm infected with a pathogen!" << std::endl;
    }
}
