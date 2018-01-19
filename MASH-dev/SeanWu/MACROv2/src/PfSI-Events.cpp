/*
 * ################################################################################
 *        ____  _________ ____
 *       / __ \/ __/ ___//  _/
 *      / /_/ / /_ \__ \ / /
 *     / ____/ __/___/ // /
 *    /_/   /_/  /____/___/
 *
 *    PfSI Events
 *    MASH Team
 *    January 2017
 *
 * ################################################################################
*/

#include "PfSI-Events.hpp"
#include "PfSI-Parameters.hpp"
#include "Human-PfSI.hpp"
#include "MACRO-PRNG.hpp"


/* ################################################################################
 * InfectiousBite
 * ################################################################################
*/

void InfectiousBite_pfsi::Fire(human_pfsi* human){
  if(prng::instance()->get_runif() < pfsi_parameters::instance()->get_b()){
    double tInfStart = ttInfectionPf();
  }
};

double InfectiousBite_pfsi::ttInfectionPf(){
  return pfsi_parameters::instance()->get_LatentPf();
}

void InfectiousBite_pfsi::OnMessage(human_pfsi *human, const Message &message){};
