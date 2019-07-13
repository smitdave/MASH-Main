/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfSI Event class
 *  (include SimBite for PfSI)
 *
 *  Sean Wu
 *  November 2018
*/

#ifndef EVENT_PFSI_HPP
#define EVENT_PFSI_HPP

/* C++ includes */
#include <functional>

/* Rcpp includes */
#include <RcppArmadillo.h>

/* base class include */
#include "Event.hpp"

/* forward includes */
class human;


/* ################################################################################
 * sample waiting time (hazard functions for events)
################################################################################ */

double pfsi_ttClearPf(human* h); /* duration of infection */
double psfi_ttInfectionPf(human* h); /* duration of latent period */
double pfsi_ttFeverPf(human* h); /* timing of fever event (when does it start relative to infection?) */
double pfsi_ttTreatPf(human* h); /* timing of treatment event (when does it occur relative to infection?) */
double pfsi_ttSusceptiblePf(human* h); /* duration of protection from chemoprophylaxis */
double pfsi_ttPEWanePf(human* h); /* duration of protection by PE vaxx */
double pfsi_ttGSWanePf(human* h); /* duration of protection by GS vaxx */


/* ################################################################################
 * SimBite events
################################################################################ */

/* simulated infectious bite; tag: PfSI_SimBite */
class e_pfsi_bite : public event {
public:
  /* constructor */
  e_pfsi_bite(double tEvent_, human* h);

  /* destructor */
  ~e_pfsi_bite() = default;

};


/* ################################################################################
 * PfSI events
################################################################################ */

/* use a special "event" for people infected upon simulation start */
class e_pfsi_initial : public event {
public:
  /* constructor */
  e_pfsi_initial(double tEvent_, human* h);

  /* destructor */
  ~e_pfsi_initial() = default;
};

/* start a PfSI infection; tag: PfSI_infection */
class e_pfsi_infect : public event {
public:
  /* constructor */
  e_pfsi_infect(double tEvent_, human* h);

  /* destructor */
  ~e_pfsi_infect() = default;
};


/* end a PfSI infection; tag: PfSI_recovery */
class e_pfsi_recover : public event {
public:
  /* constructor */
  e_pfsi_recover(double tEvent_, human* h);

  /* destructor */
  ~e_pfsi_recover() = default;
};

/* a fever bout; tag: PfSI_fever */
class e_pfsi_fever : public event {
public:
  /* constructor */
  e_pfsi_fever(double tEvent_, human* h);

  /* destructor */
  ~e_pfsi_fever() = default;
};

/* seek treatment; tag: PfSI_treatment */
class e_pfsi_treatment : public event {
public:
  /* constructor */
  e_pfsi_treatment(double tEvent_, human* h);

  /* destructor */
  ~e_pfsi_treatment() = default;
};

/* end of chemoprophylaxis; tag: PfSI_endprophylaxis */
class e_pfsi_endchx : public event {
public:
  /* constructor */
  e_pfsi_endchx(double tEvent_, human* h);

  /* destructor */
  ~e_pfsi_endchx() = default;
};


/* ################################################################################
 * PfSI Vaccination events
################################################################################ */

/* PE vaccination (sporozoite-blocking vaccination); tag: PfSI_PEVaxx */
class e_pfsi_pevaxx : public event {
public:
  /* constructor */
  e_pfsi_pevaxx(double tEvent_, const bool treat, human* h);

  /* destructor */
  ~e_pfsi_pevaxx() = default;
};

/* waning effectiveness of PE vaccination; tag: PfSI_PEWane */
class e_pfsi_pewane : public event {
public:
  /* constructor */
  e_pfsi_pewane(double tEvent_, human* h);

  /* destructor */
  ~e_pfsi_pewane() = default;
};

/* GS vaccination (gametocyte-blocking vaccination); tag: PfSI_GSVaxx */
class e_pfsi_gsvaxx : public event {
public:
  /* constructor */
  e_pfsi_gsvaxx(double tEvent_, const bool treat, human* h);

  /* destructor */
  ~e_pfsi_gsvaxx() = default;
};

/* waning effectiveness of GS vaccination; tag: PfSI_GSWane */
class e_pfsi_gswane : public event {
public:
  /* constructor */
  e_pfsi_gswane(double tEvent_, human* h);

  /* destructor */
  ~e_pfsi_gswane() = default;
};


#endif /* Event_PfSI_hpp */
