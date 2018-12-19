/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfSI Event class
 *  Requires logging stream 'human_inf'
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef Event_PfSI_hpp
#define Event_PfSI_hpp

/* Rcpp includes */
#include <RcppArmadillo.h>

/* C++ includes */
#include <functional>

/* base class include */
#include "Event.hpp"

class human_pfsi;
class parameters;


/* ################################################################################
 * PfSI events
################################################################################ */

/* start a PfSI infection; tag: PfSI_infection */
class e_pfsi_infect : public event {
public:
  /* constructor */
  e_pfsi_infect(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_infect();
};


/* end a PfSI infection; tag: PfSI_recovery */
class e_pfsi_recover : public event {
public:
  /* constructor */
  e_pfsi_recover(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_recover();
};

/* a fever bout; tag: PfSI_fever */
class e_pfsi_fever : public event {
public:
  /* constructor */
  e_pfsi_fever(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_fever();
};

/* seek treatment; tag: PfSI_treatment */
class e_pfsi_treatment : public event {
public:
  /* constructor */
  e_pfsi_treatment(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_treatment();
};

/* end of chemoprophylaxis; tag: PfSI_endprophylaxis */
class e_pfsi_endchx : public event {
public:
  /* constructor */
  e_pfsi_endchx(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_endchx();
};


/* ################################################################################
 * PfSI Vaccination events
################################################################################ */

/* PE vaccination (sporozoite-blocking vaccination); tag: PfSI_PEVaxx */
class e_pfsi_pevaxx : public event {
public:
  /* constructor */
  e_pfsi_pevaxx(double tEvent_, const bool treat, human_pfsi* h);

  /* destructor */
  ~e_pfsi_pevaxx();
};

/* waning effectiveness of PE vaccination; tag: PfSI_PEWane */
class e_pfsi_pewane : public event {
public:
  /* constructor */
  e_pfsi_pewane(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_pewane();
};

/* GS vaccination (gametocyte-blocking vaccination); tag: PfSI_GSVaxx */
class e_pfsi_gsvaxx : public event {
public:
  /* constructor */
  e_pfsi_gsvaxx(double tEvent_, const bool treat, human_pfsi* h);

  /* destructor */
  ~e_pfsi_gsvaxx();
};

/* waning effectiveness of GS vaccination; tag: PfSI_GSWane */
class e_pfsi_gswane : public event {
public:
  /* constructor */
  e_pfsi_gswane(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_gswane();
};


#endif /* Event_PfSI_hpp */
