/*
 * ################################################################################
 *
 #        ____        __       __
 #       / __ \____ _/ /______/ /_
 #      / /_/ / __ `/ __/ ___/ __ \
 #     / ____/ /_/ / /_/ /__/ / / /
 #    /_/    \__,_/\__/\___/_/ /_/
 *
 *    Patch Class Definition
 *    MASH Team
 *    November 2017
 *
 * ################################################################################
*/

#ifndef PATCH_HPP
#define PATCH_HPP

#include <stdio.h>
#include <iostream>

#include <RcppGSL.h>

#include "DEBUG.hpp"

// forward declarations
class tile;
class human;
class humanPop;

// typedefs
typedef std::shared_ptr<humanPop> humanPopPtr;


/*
 * ################################################################################
 *    Patch Class
 * ################################################################################
 */

class patch {
public:
    patch(const int &id_new);
    ~patch();

    void                       set_id(const int &i);
    int                        get_id();


private:

    int                         id;                 // id

    humanPopPtr                 humans;             // shared pointer to humanPop

};




#endif /* PATCH_HPP */
