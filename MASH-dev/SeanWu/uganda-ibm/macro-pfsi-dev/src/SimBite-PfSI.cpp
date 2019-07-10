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
#include "Human.hpp"
#include "Event-PfSI.hpp"

#include "Tile.hpp"

#include "PRNG.hpp"
#include "Parameters.hpp"


/* ################################################################################
 * sample waiting time (hazard functions for events)
################################################################################ */

/* duration of infection */
double pfsi_ttClearPf(human* h){
  double RecoveryPf = 1.0 / h->get_tile()->get_params()->get_param<double>("DurationPf");
  return h->get_tile()->get_prng()->get_rexp(RecoveryPf);
};

/* duration of latent period */
double psfi_ttInfectionPf(human* h){
  double LatentPf = h->get_tile()->get_params()->get_param<double>("LatentPf");
  return LatentPf;
};

/* timing of fever event (when does it start relative to infection?) */
double pfsi_ttFeverPf(human* h){
  double mnFeverPf = h->get_tile()->get_params()->get_param<double>("mnFeverPf");
  return mnFeverPf;
};

/* timing of treatment event (when does it occur relative to infection?) */
double pfsi_ttTreatPf(human* h){
  double mnTreatPf = h->get_tile()->get_params()->get_param<double>("mnTreatPf");
  return mnTreatPf;
};

/* duration of protection from chemoprophylaxis */
double pfsi_ttSusceptiblePf(human* h){
  double mnChemoprophylaxisPf = h->get_tile()->get_params()->get_param<double>("mnChemoprophylaxisPf");
  return mnChemoprophylaxisPf;
};

/* duration of protection by PE vaxx */
double pfsi_ttPEWanePf(human* h){
  double mnPEPf = h->get_tile()->get_params()->get_param<double>("mnPEPf");
  double vrPEPf = h->get_tile()->get_params()->get_param<double>("vrPEPf");
  return h->get_tile()->get_prng()->get_rnorm(mnPEPf,vrPEPf);
};

/* duration of protection by GS vaxx */
double pfsi_ttGSWanePf(human* h){
  double mnGSPf = h->get_tile()->get_params()->get_param<double>("mnGSPf");
  double vrGSPf = h->get_tile()->get_params()->get_param<double>("vrGSPf");
  return h->get_tile()->get_prng()->get_rnorm(mnGSPf,vrGSPf);
};


/* ################################################################################
 * SimBite events
################################################################################ */


/* simulated biting event */

/* constructor */
e_pfsi_bite::e_pfsi_bite(double tEvent_, human* h):
  event("PfSI_SimBite",tEvent_,[tEvent_,h](){

    /* transmission efficiency */
    double b = h->get_b();
    if(h->get_tile()->get_prng()->get_runif() < b){
      double tInfStart = tEvent_ + psfi_ttInfectionPf(h);
      h->addEvent2Q(e_pfsi_infect(tInfStart,h));
    }

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
