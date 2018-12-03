/*
 *      __  ______   __________  ____ 
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ / 
 *  /_/  /_/_/  |_\____/_/ |_|\____/  
 * 
 *  Generic Event Class: events occur on humans;
 *  and are specialized for each model (eg PfSI, PfMOI, etc.)
 *  
 *  Sean Wu
 *  November 2018
 */

#include "Event.hpp"

/* constructor */
event::event(std::string tag_, double tEvent_, std::function<void()> eventF_):
  tag(tag_),tEvent(tEvent_),eventF(eventF_) {
  #ifdef DEBUG_MACRO
  std::cout << "event constructor being called at " << this << std::endl;
  #endif
};

/* destructor */
event::~event(){
  #ifdef DEBUG_MACRO
  std::cout << "event destructor being called at " << this << std::endl;
  #endif
};
