/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Generic Mosquito Class: mosquitos are specialized by model type
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef Mosquito_hpp
#define Mosquito_hpp

/* standard includes */
#include <stdio.h>
#include <iostream>

/* forward declarations */
class tile;

class mosquito {
public:

  mosquito(tile* tileP_) : tileP(tileP_) {
    #ifdef DEBUG_MACRO
    std::cout << "mosquito born at " << this << std::endl;
    #endif
  };
  virtual ~mosquito() = 0;

  /* interface */
  // virtual void simulate();

protected:

  tile*                 tileP;

};


#endif
