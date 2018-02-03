/*
 * ################################################################################
 *        ______                 __
 *       / ____/   _____  ____  / /_
 *      / __/ | | / / _ \/ __ \/ __/
 *     / /___ | |/ /  __/ / / / /_
 *    /_____/ |___/\___/_/ /_/\__/
 *
 *    Event template
 *    MASH Team
 *    January 2017
 *
 * ################################################################################
*/

/*
 * in MASH, agents experience 'events' that change their internal state
 * agents can also communicate amongst themselves by sending events to each other.
*/

#ifndef EVENTS_MACRO
#define EVENTS_MACRO

#include <string>

struct Message;

template <class agent>
class Event {
public:

  virtual ~Event(){};

  /* fire an event */
  virtual void Fire(agent*) = 0;

  /* fire events that are messages from other agents */
  virtual void OnMessage(agent*, const Message&) = 0;

  /* accessors */
  double get_tEvent(){return tEvent;};
  std::string& get_tag(){return tag;};

protected:

  double        tEvent; /* time from now the event fires */
  std::string   tag; /* tag of event */

};

/* comparators for priority queue */
template <class agent>
bool operator<(const Event<agent>& e1, const Event<agent>& e2){
  if(e1 == e2){
    return false;
  } else {
    return e1.tEvent < e2.tEvent;
  }
};

#endif
