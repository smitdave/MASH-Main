#ifndef human_hpp
#define human_hpp

#include <stdio.h>
#include <string>
#include <tuple>
#include <iostream>

#include "DEBUG.hpp"

class patch;            // forward declare patch
class tile;             // forward declare tile
class pathogen;         // forward declare pathogen

typedef std::tuple<patch*,tile*> address;

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
    void                        set_home_address(patch* h, tile* b);
    patch*                      get_home_patch();
    tile*                       get_home_tile();

    // current address: my patch and tile
    address                     get_current_address();
    void                        set_current_address(patch* h, tile* b);
    patch*                      get_current_patch();
    tile*                       get_current_tile();

    // pathogen: pathogens infecting me
    pathogen*                   get_pathogen();
    void                        set_pathogen(pathogen* p);

    void                        get_memLoc();

private:

    int                         id;                 // id
    std::string                 state;              // my life state
    bool                        inf;                // my infection status

    address                     home_address;       // home address
    address                     current_address;    // current address

    pathogen*                   pathogen_ptr;

};


#endif /* human_hpp */
