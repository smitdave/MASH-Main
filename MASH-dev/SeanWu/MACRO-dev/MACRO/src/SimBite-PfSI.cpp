/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Simulated Biting (derive from 'Event') for PfSI module
 *
 *  Sean Wu
 *  November 2018
 */

#include "SimBite-PfSI.hpp"
#include "Human-PfSI.hpp"

/* simulated biting event */

/* constructor */
e_pfsi_bite::e_pfsi_bite(double tEvent_, human_pfsi* h):
  event("SimBitePfSI",tEvent_,[h](){
    
  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_bite constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_pfsi_bite::~e_pfsi_bite(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_bite destructor being called at " << this << std::endl;
  #endif

};
