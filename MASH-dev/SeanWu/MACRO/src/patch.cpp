/*
 * ################################################################################
 *
 #        ____        __       __
 #       / __ \____ _/ /______/ /_
 #      / /_/ / __ `/ __/ ___/ __ \
 #     / ____/ /_/ / /_/ /__/ / / /
 #    /_/    \__,_/\__/\___/_/ /_/
 *
 *    Patch Class Implementation
 *    MASH Team
 *    November 2017
 *
 * ################################################################################
 */

#include "patch.hpp"


/*
 * ################################################################################
 *    Constructor & Destructor
 * ################################################################################
 */

patch::patch(const int &id_new){
  id = id_new;
  #ifdef DEBUG_INFSIM
  std::cout << "patch " << id << " being born at memory location: " << this << std::endl;;
  #endif
};

patch::~patch(){
  #ifdef DEBUG_INFSIM
  std::cout << "patch " << id << " being killed at memory location: " << this << std::endl;;
  #endif
};


/*
 * ################################################################################
 *    Getters & Setters
 * ################################################################################
 */

void patch::set_id(const int &i){
  id = i;
};

int patch::get_id(){
  return id;
};