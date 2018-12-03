/*
 * ################################################################################
 *
 *      _______ __
 *     /_  __(_) /__
 *      / / / / / _ \
 *     / / / / /  __/
 *    /_/ /_/_/\___/
 *
 *    Tile Class Definition
 *    MASH Team
 *    January 2017
 *
 * ################################################################################
*/

#include "Tile.hpp"
#include "Human.hpp"

tile::tile(){
  #ifdef DEBUG_MACRO
  std::cout << "tile being born at " << this << std::endl;
  #endif
};

tile::~tile(){
  #ifdef DEBUG_MACRO
  std::cout << "tile being killed at " << this << std::endl;
  #endif
};
