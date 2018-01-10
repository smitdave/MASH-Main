/*
 * ################################################################################
 *        ____  _________ ____
 *       / __ \/ __/ ___//  _/
 *      / /_/ / /_ \__ \ / /
 *     / ____/ __/___/ // /
 *    /_/   /_/  /____/___/
 *
 *    PfSI Human Class Definition
 *    MASH Team
 *    January 2017
 *
 * ################################################################################
*/

#ifndef HUMAN_PFSI_HPP
#define HUMAN_PFSI_HPP

#include "Human.hpp"

#include "MACRO-DEBUG.hpp"

class human_pfsi : public human {

public:
  human_pfsi(const int& _id, const double& _age, tile* _tileP);
  ~human_pfsi();

  void                  simHuman();

private:

};

#endif
