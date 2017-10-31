#ifndef human_hpp
#define human_hpp

#include <stdio.h>
#include <iostream>
#include <string>

#include <tuple>        // for address tuple
#include <functional>   // std::invoke
#include <vector>       // for event queue


#include "DEBUG.hpp"

class patch;            // forward declare patch
class tile;             // forward declare tile
class pathogen;         // forward declare pathogen

typedef std::tuple<patch*,tile*> address;       // address is a tuple of pointers
  
// event is a struct
typedef struct event {
  std::string           tag;            // type of the event
  double                tEvent;         // time event will fire
  std::function<void()> eventF;         // bound function with parameters
  event(std::string _tag, double _tEvent, std::function<void()> _eventF):
    tag(_tag),tEvent(_tEvent),eventF(_eventF) {};
  ~event(){};
} event;


class human {
public:
    human(const int &id_new);
    ~human();

    int                         get_id();
    std::string                 get_state();
    void                        set_state(const std::string &state_new);
    bool                        get_inf();
    void                        set_inf(const bool &i);

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
    void                        add2Q_set_state(const double &tEvent, std::string state_new);


    void                        get_memLoc();

private:

    int                         id;                 // id
    std::string                 state;              // my life state
    bool                        inf;                // my infection status

    address                     home_address;       // home address
    address                     current_address;    // current address

    pathogen*                   pathogen_ptr;       // pathogen object
    
    // std::vector<event>          event_queue;
    std::vector<std::function<void()>> event_queue;


};




#endif /* human_hpp */
