/*
 *      __  ______   __________  ____ 
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ / 
 *  /_/  /_/_/  |_\____/_/ |_|\____/  
 * 
 *  Generic Human Class: humans are specialized by model type
 *  
 *  Sean Wu
 *  November 2018
 */

#include "Event.hpp"
#include "Human.hpp"

/* define virtual destructor is ok */
human::~human(){
  #ifdef MACRO_DEBUG
  std::cout << "human " << id << " dying at " << this << std::endl;
  #endif
};

/* move operators */
human::human(human&&) = default;
human& human::operator=(human&&) = default;

/* event queue related functions */

/* add an event to my queue */
void human::addEvent2Q(event&& e){
  eventQ.emplace_back(std::make_unique<event>(e));
  std::sort(eventQ.begin(),eventQ.end(),[](std::unique_ptr<event>& e1, std::unique_ptr<event>& e2){
    return e1->tEvent < e2->tEvent;
  });
};

/* remove future queued events */
void human::rmTagFromQ(const std::string &tag){
  eventQ.erase(std::remove_if(eventQ.begin(),eventQ.end(),
                              [tag](eventP& e){
                                return(tag.compare(e->tag)==0);
                              }));
};

/* fire the first event */
void human::fireEvent(){
  if(eventQ.size() > 0){
    eventQ.front()->eventF();
    eventQ.erase(eventQ.begin());
  }
};

/* print my event queue */
void human::printEventQ(){
  std::cout << "printing human " << id << ", event queue: " << std::endl;
  for(auto it = eventQ.begin(); it != eventQ.end(); it++){
    (*it)->print();
  }
};
