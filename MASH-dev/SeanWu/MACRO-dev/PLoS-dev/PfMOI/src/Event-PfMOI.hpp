/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfMOI Event class
 *  (include SimBite for pfmoi)
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  August 2019
*/

#ifndef EVENT_PFMOI_HPP
#define EVENT_PFMOI_HPP

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

double pfmoi_ttClearPf(human* h); /* duration of infection */
double psfi_ttInfectionPf(human* h); /* duration of latent period */
double pfmoi_ttFeverPf(human* h); /* timing of fever event (when does it start relative to infection?) */
double pfmoi_ttTreatPf(human* h); /* timing of treatment event (when does it occur relative to infection?) */
double pfmoi_ttSusceptiblePf(human* h); /* duration of protection from chemoprophylaxis */
double pfmoi_ttPEWanePf(human* h); /* duration of protection by PE vaxx */
double pfmoi_ttGSWanePf(human* h); /* duration of protection by GS vaxx */


/* ################################################################################
 * SimBite events
################################################################################ */

/* simulated infectious bite; tag: pfmoi_SimBite */
class e_pfmoi_bite : public event {
public:
  /* constructor */
  e_pfmoi_bite(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_bite() = default;

};


/* ################################################################################
 * pfmoi events
################################################################################ */

/* use a special "event" for people infected upon simulation start */
class e_pfmoi_initial : public event {
public:
  /* constructor */
  e_pfmoi_initial(double tEvent_, const u_int MOI_init, human* h);

  /* destructor */
  ~e_pfmoi_initial() = default;
};

/* start a pfmoi infection; tag: pfmoi_infection */
class e_pfmoi_infect : public event {
public:
  /* constructor */
  e_pfmoi_infect(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_infect() = default;
};


/* clear a pfmoi infection; tag: pfmoi_clear */
class e_pfmoi_clear : public event {
public:
  /* constructor */
  e_pfmoi_clear(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_clear() = default;
};

/* a fever bout; tag: pfmoi_fever */
class e_pfmoi_fever : public event {
public:
  /* constructor */
  e_pfmoi_fever(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_fever() = default;
};

/* seek treatment; tag: pfmoi_treatment */
class e_pfmoi_treatment : public event {
public:
  /* constructor */
  e_pfmoi_treatment(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_treatment() = default;
};

/* end of chemoprophylaxis; tag: pfmoi_endprophylaxis */
class e_pfmoi_endchx : public event {
public:
  /* constructor */
  e_pfmoi_endchx(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_endchx() = default;
};


/* ################################################################################
 * pfmoi Vaccination events
################################################################################ */

/* PE vaccination (sporozoite-blocking vaccination); tag: pfmoi_PEVaxx */
class e_pfmoi_pevaxx : public event {
public:
  /* constructor */
  e_pfmoi_pevaxx(double tEvent_, const bool treat, human* h);

  /* destructor */
  ~e_pfmoi_pevaxx() = default;
};

/* waning effectiveness of PE vaccination; tag: pfmoi_PEWane */
class e_pfmoi_pewane : public event {
public:
  /* constructor */
  e_pfmoi_pewane(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_pewane() = default;
};

/* GS vaccination (gametocyte-blocking vaccination); tag: pfmoi_GSVaxx */
class e_pfmoi_gsvaxx : public event {
public:
  /* constructor */
  e_pfmoi_gsvaxx(double tEvent_, const bool treat, human* h);

  /* destructor */
  ~e_pfmoi_gsvaxx() = default;
};

/* waning effectiveness of GS vaccination; tag: pfmoi_GSWane */
class e_pfmoi_gswane : public event {
public:
  /* constructor */
  e_pfmoi_gswane(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_gswane() = default;
};


#endif /* Event_pfmoi_hpp */
