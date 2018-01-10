/*
 * ################################################################################
 *
 *        __  __
 *       / / / /_  ______ ___  ____ _____
 *      / /_/ / / / / __ `__ \/ __ `/ __ \
 *     / __  / /_/ / / / / / / /_/ / / / /
 *    /_/ /_/\__,_/_/ /_/ /_/\__,_/_/ /_/
 *
 *    Human Abstract Base Class Definition
 *    MASH Team
 *    January 2017
 *
 * ################################################################################
*/

#ifndef HUMAN_HPP
#define HUMAN_HPP

#include <iostream>
#include <string>
#include <functional> /* std::function */

/*
 * ################################################################################
 *    Event Queue
 * ################################################################################
*/

/* event is a struct */
typedef struct event {

  /* event data */
  std::string           tag;            /* tag of event */
  double                tEvent;         /* time event will fire */
  std::function<void()> eventF;         /* bound closure */

  /* constructor & destructor */
  event(std::string _tag, double _tEvent, std::function<void()> _eventF):
    tag(_tag),tEvent(_tEvent),eventF(_eventF) {};
  ~event(){};
} event;


/*
 * ################################################################################
 *    Human Class
 * ################################################################################
*/

class human {
public:
  virtual ~human() = 0;

  human(human&&);
protected:

};

human::~human(){};

#endif
