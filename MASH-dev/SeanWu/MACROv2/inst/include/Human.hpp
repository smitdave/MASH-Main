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
#include <vector>
#include <functional> /* std::function */


/*
 * ################################################################################
 *    Declarations
 * ################################################################################
*/

class tile; /* tile declaration */


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
  /* constructor & destructor */
  human(const int& _id, const double& _age, tile* _tileP);
  virtual ~human() = 0;

  /* move operators */
  human(human&&);
  human& operator=(human&&);

  /* copy operators */
  human(human&) = delete;
  human& operator=(human&) = delete;

  /* event queue */
  void                        addEvent2Q(const event& e);
  void                        rmTagFromQ(const std::string& tag);
  void                        fireEvent();

  /* accessors */
  int&                        get_id(){return id;};
  bool&                       get_alive(){return alive;};
  double&                     get_age(){return age;};
  tile*                       get_tileP(){return tileP;};

  /* events */
  void                        event_death();

  /* virtual member functions */
  virtual void                simHuman() = 0;

protected:

  /* general fields */
  int                         id;
  bool                        alive;
  double                      age;

  /* event queue */
  std::vector<event>          EventQ;

  /* tile: raw pointer fine because tile outlives humans */
  tile*                       tileP;
};

#endif
