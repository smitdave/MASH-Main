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


/* ################################################################################
 * sample waiting time (hazard functions for events)
################################################################################ */

double pfsi_ttClearPf(human_pfsi* h); /* duration of infection */
double psfi_ttInfectionPf(human_pfsi* h); /* duration of latent period */
double pfsi_ttFeverPf(human_pfsi* h); /* timing of fever event (when does it start relative to infection?) */
double pfsi_ttTreatPf(human_pfsi* h); /* timing of treatment event (when does it occur relative to infection?) */
double pfsi_ttSusceptiblePf(human_pfsi* h); /* duration of protection from chemoprophylaxis */
double pfsi_ttPEWanePf(human_pfsi* h); /* duration of protection by PE vaxx */
double pfsi_ttGSWanePf(human_pfsi* h); /* duration of protection by GS vaxx */


/* ################################################################################
 * SimBite events
################################################################################ */

/* simulated infectious bite; tag: PfSI_SimBite */
class e_pfsi_bite : public event {
public:
  /* constructor */
  e_pfsi_bite(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_bite();

};


#endif /* SimBite_PfSI_hpp */
