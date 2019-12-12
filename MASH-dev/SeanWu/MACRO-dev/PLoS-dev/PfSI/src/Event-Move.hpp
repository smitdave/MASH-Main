/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Events to handle human movement (trips)
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  November 2018
*/

#ifndef EVENT_MOVE_HPP
#define EVENT_MOVE_HPP

/* standard includes */
#include <functional>

/* Rcpp includes */
#include <RcppArmadillo.h>

#include "Event.hpp"

/* forward declarations */
class human;


/* ################################################################################
 * take a trip to another patch
################################################################################ */

class e_move_takeTrip : public event {
public:
  /* constructor */
  e_move_takeTrip(const double tEvent_, const u_int dest_id, human* h);

  /* destructor */
  ~e_move_takeTrip() = default;
};


/* ################################################################################
 * return to my home patch
################################################################################ */

class e_move_returnHome : public event {
public:
  /* constructor */
  e_move_returnHome(const double tEvent_, human* h);

  /* destructor */
  ~e_move_returnHome() = default;
};


#endif
