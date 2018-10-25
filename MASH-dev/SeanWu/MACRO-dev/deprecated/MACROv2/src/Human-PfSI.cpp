/*
 * ################################################################################
 *        ____  _________ ____
 *       / __ \/ __/ ___//  _/
 *      / /_/ / /_ \__ \ / /
 *     / ____/ __/___/ // /
 *    /_/   /_/  /____/___/
 *
 *    PfSI Human Class Implementation
 *    MASH Team
 *    January 2017
 *
 * ################################################################################
*/

#include "Human-PfSI.hpp"

human_pfsi::human_pfsi(const int& _id, const double& _age, tile* _tileP) : human(_id, _age, _tileP) {
  #ifdef DEBUG_MACRO
  std::cout << "human_pfsi " << id << " being born at " << this << std::endl;
  #endif
};

human_pfsi::~human_pfsi(){
  #ifdef DEBUG_MACRO
  std::cout << "human_pfsi " << id << " being killed at " << this << std::endl;
  #endif
};

void human_pfsi::simHuman(){
  std::cout << "calling simHuman" << std::endl;
};
