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
 *  Sean Wu (slwu89@berkeley.edu)
 *  November 2018
*/

#ifndef EVENT_HPP
#define EVENT_HPP

/* standard includes */
#include <iostream>

/* event-specific includes */
#include <string>
#include <functional>


/* ################################################################################
 * generic event class (abstract base)
################################################################################ */

class event {
public:

  /* constructor */
  event(std::string tag_, double tEvent_, std::function<void()> eventF_);

  /* destructor */
  // NOW default
  virtual ~event();

  /* move operators */
  event(event&&) = default;
  event& operator=(event&&) = default;

  /* copy operators */
  // NOW delete
  event(event&) = delete;
  event& operator=(event&) = delete;

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
