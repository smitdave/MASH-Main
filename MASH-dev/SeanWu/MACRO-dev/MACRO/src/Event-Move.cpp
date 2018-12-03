/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Events to handle human movement (trips)
 *
 *  Sean Wu
 *  November 2018
 */

#include "Event-Move.hpp"
#include "Human.hpp"
#include "Tile.hpp"
#include "Patch.hpp"
#include "PRNG.hpp"


/* ################################################################################
 * take a trip to another patch
################################################################################ */

/* constructor */
e_move_takeTrip::e_move_takeTrip(double tEvent_, const size_t dest_id, human* h) :
  event("takeTrip",tEvent_,[tEvent_,dest_id,h](){

    h->decrement_bweight(); /* decrement the biting weight where I came from */
    h->set_patch_id(dest_id); /* move */
    h->accumulate_bweight(); /* accumulate the biting weight where I go */

    /* queue the trip back home */
    double home_t = tEvent_ + h->get_tile()->get_prng()->get_rexp(1.0/h->get_trip_duration());

    h->addEvent2Q(e_move_returnHome(home_t,h));

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_move_takeTrip constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_move_takeTrip::~e_move_takeTrip(){

  #ifdef DEBUG_MACRO
  std::cout << "e_move_takeTrip destructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * return to my home patch
################################################################################ */

e_move_returnHome::e_move_returnHome(const double tEvent_, human* h) :
  event("returnHome",tEvent_,[tEvent_,h](){

    h->decrement_bweight(); /* decrement the biting weight where I came from */
    h->set_patch_id(h->get_home_patch_id()); /* move */
    h->accumulate_bweight(); /* accumulate the biting weight where I go */

    /* queue my next trip */
    size_t dest_id = h->get_tile()->get_prng()->get_rcategorical(h->get_patch()->get_move());
    double trip_t = tEvent_ + h->get_tile()->get_prng()->get_rexp(h->get_trip_frequency());

    h->addEvent2Q(e_move_takeTrip(trip_t,dest_id,h));

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_move_returnHome constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_move_returnHome::~e_move_returnHome(){

  #ifdef DEBUG_MACRO
  std::cout << "e_move_returnHome destructor being called at " << this << std::endl;
  #endif

};
