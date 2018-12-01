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

#ifndef SimBite_PfSI_hpp
#define SimBite_PfSI_hpp

/* standard includes */
#include <stdio.h>
#include <functional>

#include "Event.hpp"

class human_pfsi;

/* simulated bite */
class e_pfsi_bite : public event {
public:
  /* constructor */
  e_pfsi_bite(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_bite();

};


#endif /* Event_PfSI_hpp */
