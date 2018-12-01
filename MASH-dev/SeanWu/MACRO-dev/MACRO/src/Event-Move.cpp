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

/* take a trip (leave my home and go somewhere) */
e_move_takeTrip::e_move_takeTrip(double tEvent_, const size_t dest_id, human* h):
  event("takeTrip",tEvent_,[dest_id,h](){

    h->decrement_bweight(); /* decrement the biting weight where I came from */
    h->set_patch_id(dest_id); /* move */
    h->accumulate_bweight(); /* accumulate the biting weight where I go */

    /* queue the trip back home */
  })
{
  std::cout << "e_move_takeTrip constructor being called at " << this << std::endl;
};

/* destructor */
e_move_takeTrip::~e_move_takeTrip(){
  std::cout << "e_move_takeTrip destructor being called at " << this << std::endl;
};
