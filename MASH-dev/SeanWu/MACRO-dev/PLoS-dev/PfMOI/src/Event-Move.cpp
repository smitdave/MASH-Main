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

#include "Event-Move.hpp"

// other MACRO headers
#include "Human-PfMOI.hpp"
#include "Tile.hpp"
#include "Patch.hpp"
#include "Parameters.hpp"

// for categorial rv
#include "RNG.hpp"


/* ################################################################################
 * take a trip to another patch
################################################################################ */

/* constructor */
e_move_takeTrip::e_move_takeTrip(double tEvent_, const u_int dest_id, human* h) :
  event("takeTrip",tEvent_,[tEvent_,dest_id,h](){

    /* pack my bags (my biting weight) and take a trip */
    h->decrement_bweight(); /* decrement the biting weight where I came from */
    h->set_patch_id(dest_id); /* move */
    h->accumulate_bweight(); /* accumulate the biting weight where I go */
    h->set_travel(true); // i'm not at home!

    /* queue the voyage home */
    double home_t = tEvent_ + R::rexp(h->get_trip_duration(dest_id));
    h->addEvent2Q(e_move_returnHome(home_t,h));

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_move_takeTrip constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * return to my home patch
################################################################################ */

e_move_returnHome::e_move_returnHome(const double tEvent_, human* h) :
  event("returnHome",tEvent_,[tEvent_,h](){

    /* i come home */
    h->decrement_bweight(); /* decrement the biting weight where I came from */
    h->set_patch_id(h->get_home_patch_id()); /* move */
    h->accumulate_bweight(); /* accumulate the biting weight where I go */
    h->set_travel(false); // i'm at home again!

    /* queue my next trip */
    int dest_id = rcategorical(h->get_patch()->get_move());
    double trip_t = tEvent_ + R::rexp(1./h->get_trip_frequency());
    h->addEvent2Q(e_move_takeTrip(trip_t,dest_id,h));

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_move_returnHome constructor being called at " << this << std::endl;
  #endif

};
