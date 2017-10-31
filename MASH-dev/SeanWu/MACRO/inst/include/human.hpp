#ifndef human_hpp
#define human_hpp

#include <stdio.h>
#include <iostream>
#include <string>

#include <tuple>        // for address tuple
#include <functional>   // std::invoke
#include <vector>       // for event queue (maybe change to linked-list later on)


#include "DEBUG.hpp"

class patch;            // forward declare patch
class tile;             // forward declare tile
class pathogen;         // forward declare pathogen

typedef std::tuple<patch*,tile*> address;       // address is a tuple of pointers
  
// // event struct
// typedef struct event {
//   // parameters
//   double tEvent;                  // time the event will fire
//   std::string tag;                // what event it is
//   // std::function<void()> event;           
//   // constructor
//   event(const double &_tEvent, 
//         const std::string &_tag,
//         std::function<void()> _event) : tEvent(_tEvent), tag(_tag), event(_event) {};
//   // destructor
//   ~event(){};
// } event;

typedef struct event {
  
  double tEvent;
  std::string tag;
  std::function<void()> event;
  
  event(double _tEvent, std::string _tag): tEvent(_tEvent), tag(_tag) {}
  ~event(){};
  
} event;

typedef struct Control{
  char key;
  std::function<void()> press;
  std::function<void()> release;
  Control(char _key, std::function<void()> _press, std::function<void()> _release):
    key(_key),press(_press),release(_release){}
} Control;

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
    void                        add2Q_set_state(const double &tEvent, const std::string &state_new);

    void                        get_memLoc();

private:

    int                         id;                 // id
    std::string                 state;              // my life state
    bool                        inf;                // my infection status

    address                     home_address;       // home address
    address                     current_address;    // current address

    pathogen*                   pathogen_ptr;       // pathogen object
    
    std::vector<event>          event_queue;

};




#endif /* human_hpp */
