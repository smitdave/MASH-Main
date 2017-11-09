/*
 * ################################################################################
 *
 *        ____
 *       /  _/___ ___  ____ ___  __  ______  ___
 *       / // __ `__ \/ __ `__ \/ / / / __ \/ _ \
 *     _/ // / / / / / / / / / / /_/ / / / /  __/
 *    /___/_/ /_/ /_/_/ /_/ /_/\__,_/_/ /_/\___/
 *
 *    Immune Class Implementation
 *    MASH Team
 *    October 2017
 *
 * ################################################################################
*/

#include "immune.hpp"

/*
 * ################################################################################
 *    Immune: Abstract Base Class
 * ################################################################################
*/

immune_base::immune_base(human_ptr _my_human, const std::string &_immune_model) : my_human(_my_human), immune_model(_immune_model) {
  #ifdef DEBUG_INFSIM
  std::cout << "immune_base " << " being born at memory location: " << this << std::endl;;
  #endif
};

immune_base::~immune_base(){
  #ifdef DEBUG_INFSIM
  std::cout << "immune_base " << " being killed at memory location: " << this << std::endl;;
  #endif
};

std::string immune_base::get_immune_model(){
  return immune_model;
};

human_ptr immune_base::get_my_human(){
  return my_human;
}


/*
 * ################################################################################
 *    Immune PfSI
 * ################################################################################
*/

