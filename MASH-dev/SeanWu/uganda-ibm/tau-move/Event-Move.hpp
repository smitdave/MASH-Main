/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Events to handle human movement (trips)
 *  Requires logging stream 'human_move'
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef Event_Move_hpp
#define Event_Move_hpp

#include <Rcpp.h>

/* standard includes */
#include <stdio.h>
#include <functional>

#include "Event.hpp"

/* forward declarations */
class human;


/* ################################################################################
 * take a trip to another patch
################################################################################ */

class e_move_takeTrip : public event {
public:
  /* constructor */
  e_move_takeTrip(const double tEvent_, const size_t dest_id, human* h);

  /* destructor */
  ~e_move_takeTrip();
};


/* ################################################################################
 * return to my home patch
################################################################################ */

class e_move_returnHome : public event {
public:
  /* constructor */
  e_move_returnHome(const double tEvent_, human* h);

  /* destructor */
  ~e_move_returnHome();
};


#endif /* Event_Move_hpp */
