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

#ifndef Event_hpp
#define Event_hpp

#include "Debug.hpp"

/* standard includes */
#include <stdio.h>
#include <iostream>

/* event-specific includes */
#include <string>
#include <functional>


/* generic event */
class event {
public:

  /* constructor */
  event(std::string tag_, double tEvent_, std::function<void()> eventF_);

  /* destructor */
  virtual ~event();

  /* move operators */
  event(event&&) = default;
  event& operator=(event&&) = default;

  /* copy operators */
  event(event&) = default;
  event& operator=(event&) = default;

  /* print (debugging) */
  void print(){
    std::cout << "event -- tag: " << tag << ", tEvent: " << tEvent << std::endl;
  };

  /* comparison for sorting */
  bool operator<(event e) const {
    return tEvent < e.tEvent;
  };

  /* information for event */
  std::string                        tag;
  double                             tEvent;
  std::function<void()>              eventF;

};

#endif
