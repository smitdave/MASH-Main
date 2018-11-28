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

#include "Mosquito.hpp"
#include "Tile.hpp"

/* define virtual destructor is ok; ensures base class parts get destroyed */
mosquito::~mosquito(){
  #ifdef MACRO_DEBUG
  std::cout << "mosquito dying at " << this << std::endl;
  #endif
};
