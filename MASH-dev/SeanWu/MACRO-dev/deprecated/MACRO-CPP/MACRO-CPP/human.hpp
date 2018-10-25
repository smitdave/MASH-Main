/*
 * ################################################################################
 *
 *        __  __
 *       / / / /_  ______ ___  ____ _____
 *      / /_/ / / / / __ `__ \/ __ `/ __ \
 *     / __  / /_/ / / / / / / /_/ / / / /
 *    /_/ /_/\__,_/_/ /_/ /_/\__,_/_/ /_/
 *
 *    Human Class Definition
 *    MASH Team
 *    October 2017
 *
 * ################################################################################
*/

#ifndef HUMAN_HPP
#define HUMAN_HPP

#include <stdio.h>
#include <iostream>
#include <string>
#include <algorithm>
#include <memory>

#include <tuple>        // for address tuple
#include <functional>   // std::invoke
#include <vector>       // for event queue

#include "DEBUG.hpp"

// forward declarations
class immune_base;      // forward declare immune
class humanPop;         // forward declare human population
class patch;            // forward declare patch
class tile;             // forward declare tile
class pathogen;         // forward declare pathogen

// typedefs
typedef std::tuple<patch*,tile*> address;       // address is a tuple of pointers
typedef std::shared_ptr<immune_base> immune_ptr;

/*
 * ################################################################################
 *    Event Queue
 * ################################################################################
*/

// event is a struct
typedef struct event {
  std::string           tag;            // type of the event
  double                tEvent;         // time event will fire
  std::function<void()> eventF;         // bound function with parameters
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
    human(const int &id_new);
    ~human();

    // basic information
    int                         get_id();
    std::string                 get_state();
    void                        set_state(const std::string &state_new);
    bool                        get_inf();
    void                        set_inf(const bool &i);
    bool                        get_alive();
    void                        set_alive(const bool &a);

    // population
    humanPop*                   get_pop_ptr();
    void                        set_pop_ptr(humanPop* h);

    // home address: my patch and tile
    address                     get_home_address();
    void                        set_home_address(patch* p, tile* t);
    patch*                      get_home_patch();
    tile*                       get_home_tile();

    // current address: my patch and tile
    address                     get_current_address();
    void                        set_current_address(patch* p, tile* t);
    void                        set_current_patch(patch* p);
    void                        set_current_tile(tile* t);
    patch*                      get_current_patch();
    tile*                       get_current_tile();

    // pathogen: pathogens infecting me
    pathogen*                   get_pathogen();
    void                        set_pathogen(pathogen* p);

    // event queue
    void                        addEvent2Q(const event &e);
    void                        rmTagFromQ(const std::string &tag);
    void                        fireEvent();
    void                        printEventQ();

    void                        add2Q_set_state(const double &tEvent, std::string s);

    // other
    void                        death();

    // debug
    void                        check_inf();
    void                        get_memLoc();
    void                        fireEventTest();
    void                        add2Q_set_stateTest(const double &tEvent, std::string state_new);

private:

    int                         id;                 // id
    std::string                 state;              // my life state
    bool                        inf;                // my infection status
    bool                        alive;

    humanPop*                   pop_ptr;
    address                     home_address;       // home address
    address                     current_address;    // current address

    pathogen*                   pathogen_ptr;       // pathogen object

    immune_base*                immune;            // my immune system

    std::vector<event>          eventQ;             // event queue

    // debug
    std::vector<std::function<void()>> event_queue;
};




#endif /* HUMAN_HPP */
