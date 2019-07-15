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
 *  Sean Wu (slwu89@berkeley.edu)
 *  November 2018
*/

#include "Event-PfSI.hpp"

// other object includes
#include "Human-PfSI.hpp"
#include "Tile.hpp"
#include "Patch.hpp"

// utiltiy includes
#include "Parameters.hpp"


/* ################################################################################
 * sample waiting time (hazard functions for events)
################################################################################ */

/* duration of infection */
double pfsi_ttClearPf(human* h){
  double DurationPf = h->get_tile()->get_params()->get_param("DurationPf");
  return R::rexp(DurationPf);
};

/* duration of latent period */
double psfi_ttInfectionPf(human* h){
  double LatentPf = h->get_tile()->get_params()->get_param("LatentPf");
  return LatentPf;
};

/* timing of fever event (when does it start relative to infection?) */
double pfsi_ttFeverPf(human* h){
  double mnFeverPf = h->get_tile()->get_params()->get_param("mnFeverPf");
  return mnFeverPf;
};

/* timing of treatment event (when does it occur relative to infection?) */
double pfsi_ttTreatPf(human* h){
  double mnTreatPf = h->get_tile()->get_params()->get_param("mnTreatPf");
  return mnTreatPf;
};

/* duration of protection from chemoprophylaxis */
double pfsi_ttSusceptiblePf(human* h){
  double mnChemoprophylaxisPf = h->get_tile()->get_params()->get_param("mnChemoprophylaxisPf");
  return mnChemoprophylaxisPf;
};

/* duration of protection by PE vaxx */
double pfsi_ttPEWanePf(human* h){
  double mnPEPf = h->get_tile()->get_params()->get_param("mnPEPf");
  double vrPEPf = h->get_tile()->get_params()->get_param("vrPEPf");
  return std::fmax(0., R::rnorm(mnPEPf,vrPEPf));
};

/* duration of protection by GS vaxx */
double pfsi_ttGSWanePf(human* h){
  double mnGSPf = h->get_tile()->get_params()->get_param("mnGSPf");
  double vrGSPf = h->get_tile()->get_params()->get_param("vrGSPf");
  return std::fmax(0., R::rnorm(mnGSPf,vrGSPf));
};


/* ################################################################################
 * SimBite events
################################################################################ */

/* constructor */
e_pfsi_bite::e_pfsi_bite(double tEvent_, human* h):
  event("PfSI_SimBite",tEvent_,[tEvent_,h](){

    /* transmission efficiency */
    double b = h->get_b();
    if(R::runif(0.,1.) < b){
      double tInfStart = tEvent_ + psfi_ttInfectionPf(h);
      h->addEvent2Q(e_pfsi_infect(tInfStart,h));
    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_bite constructor being called at " << this << std::endl;
  #endif

};



/* ################################################################################
 * PfSI initialization: start an infection at t=0
################################################################################ */

/* constructor */
e_pfsi_initial::e_pfsi_initial(double tEvent_, human* h):
  event("PfSI_initial",tEvent_,[tEvent_,h](){

    /* queue clearance event */
    double tEnd = tEvent_ + pfsi_ttClearPf(h);
    h->addEvent2Q(e_pfsi_recover(tEnd,h));

    /* queue fever event */
    double FeverPf = h->get_tile()->get_params()->get_param("FeverPf");
    if(R::runif(0.,1.) < FeverPf){
      double tFever = tEvent_ + pfsi_ttFeverPf(h);
      h->addEvent2Q(e_pfsi_fever(tFever,h));
    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_initial constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * start a PfSI infection; tag: PfSI_infection
################################################################################ */

/* constructor */
e_pfsi_infect::e_pfsi_infect(double tEvent_, human* h):
  event("PfSI_infection",tEvent_,[tEvent_,h](){

    /* no superinfection, and chx blocks new infections */
    if(h->get_state().compare("S") == 0){

      /* i'm now infected */
      h->set_state("I");

      // increment incidence where i am
      h->get_patch()->update_incidence(h->get_travel());

      /* queue clearance event */
      double tEnd = tEvent_ + pfsi_ttClearPf(h);
      h->addEvent2Q(e_pfsi_recover(tEnd,h));

      /* queue fever event */
      double FeverPf = h->get_tile()->get_params()->get_param("FeverPf");
      if(R::runif(0.,1.) < FeverPf){
        double tFever = tEvent_ + pfsi_ttFeverPf(h);
        h->addEvent2Q(e_pfsi_fever(tFever,h));
      }

    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_infect constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * end a PfSI infection; tag: PfSI_recovery
################################################################################ */

/* constructor */
e_pfsi_recover::e_pfsi_recover(double tEvent_, human* h):
  event("PfSI_recovery",tEvent_,[tEvent_,h](){

    /* i can only recover if i'm infected */
    if(h->get_state().compare("I") == 0){
      h->rmTagFromQ("PfSI_fever");
      h->set_state("S");

    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_recover constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * a fever bout; tag: PfSI_fever
################################################################################ */

/* constructor */
e_pfsi_fever::e_pfsi_fever(double tEvent_, human* h):
  event("PfSI_fever",tEvent_,[tEvent_,h](){

    /* if i get a fever, i'll seek treatment with this probability */
    double TreatPf = h->get_tile()->get_params()->get_param("TreatPf");
    if(R::runif(0.,1.) < TreatPf){
      double tTreat = tEvent_ + pfsi_ttTreatPf(h);
      h->addEvent2Q(e_pfsi_treatment(tTreat,h));
    }


  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_fever constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * seek treatment; tag: PfSI_treatment
################################################################################ */

/* constructor */
e_pfsi_treatment::e_pfsi_treatment(double tEvent_, human* h):
  event("PfSI_treatment",tEvent_,[tEvent_,h](){

    /* clear out this infection */
    h->set_state("P");
    h->rmTagFromQ("PfSI_recovery");
    h->rmTagFromQ("PfSI_endprophylaxis");

    /* initiate a period of protection from chemoprophylaxis */
    double tSusceptible = tEvent_ + pfsi_ttSusceptiblePf(h);
    h->addEvent2Q(e_pfsi_endchx(tSusceptible,h));

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_treatment constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * end of chemoprophylaxis; tag: PfSI_endprophylaxis
################################################################################ */

/* constructor */
e_pfsi_endchx::e_pfsi_endchx(double tEvent_, human* h):
  event("PfSI_endprophylaxis",tEvent_,[tEvent_,h](){

    /* chx protection ends */
    h->set_state("S");

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_endchx constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * PE vaccination (sporozoite-blocking vaccination); tag: PfSI_PEVaxx
################################################################################ */

/* constructor */
e_pfsi_pevaxx::e_pfsi_pevaxx(double tEvent_, const bool treat, human* h):
  event("PfSI_PEVaxx",tEvent_,[tEvent_,treat,h](){

    /* check if vaxx fails (vaxx efficacy) */
    double PEProtectPf = h->get_tile()->get_params()->get_param("PEProtectPf");
    if(R::runif(0.,1.) < PEProtectPf){

      /* lower my probability to get infected by mosquitos */
      double b =  h->get_tile()->get_params()->get_param("Pf_b");
      double peBlockPf = h->get_tile()->get_params()->get_param("peBlockPf");
      h->set_b(b * (1.0 - peBlockPf));

      h->rmTagFromQ("PfSI_PEWane");

      /* queue the vaxx wane event */
      double tWane = tEvent_ + pfsi_ttPEWanePf(h);
      h->addEvent2Q(e_pfsi_pewane(tWane,h));
    }

    /* if treatment accompanies vaccination */
    if(treat){

      /* copied from e_pfsi_treatment */
      h->set_state("P");
      h->rmTagFromQ("PfSI_recovery");
      h->rmTagFromQ("PfSI_endprophylaxis");

      /* initiate a period of protection from chemoprophylaxis */
      double tSusceptible = tEvent_ + pfsi_ttSusceptiblePf(h);
      h->addEvent2Q(e_pfsi_endchx(tSusceptible,h));
    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_pevaxx constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * PE vaccination (sporozoite-blocking vaccination); tag: PfSI_PEWane
################################################################################ */

/* constructor */
e_pfsi_pewane::e_pfsi_pewane(double tEvent_, human* h):
  event("PfSI_PEWane",tEvent_,[tEvent_,h](){

    /* vaxx protection wanes */
    double b =  h->get_tile()->get_params()->get_param("Pf_b");
    h->set_b(b);

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_pewane constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * GS vaccination (gametocyte-blocking vaccination); tag: PfSI_GSVaxx
################################################################################ */

/* constructor */
e_pfsi_gsvaxx::e_pfsi_gsvaxx(double tEvent_, const bool treat, human* h):
  event("PfSI_GSVaxx",tEvent_,[tEvent_,treat,h](){

    /* check if vaxx fails (vaxx efficacy) */
    double GSProtectPf = h->get_tile()->get_params()->get_param("GSProtectPf");
    if(R::runif(0.,1.) < GSProtectPf){

      /* lower my probability to infect mosquitos */
      double c = h->get_tile()->get_params()->get_param("Pf_c");
      double gsBlockPf = h->get_tile()->get_params()->get_param("gsBlockPf");
      h->set_c(c * (1.0 - gsBlockPf));

      h->rmTagFromQ("PfSI_GSWane");

      /* queue the vaxx wane event */
      double tWane = tEvent_ + pfsi_ttGSWanePf(h);
      h->addEvent2Q(e_pfsi_gswane(tWane,h));
    }

    /* if treatment accompanies vaccination */
    if(treat){

      /* copied from e_pfsi_treatment */
      h->set_state("P");
      h->rmTagFromQ("PfSI_recovery");
      h->rmTagFromQ("PfSI_endprophylaxis");

      /* initiate a period of protection from chemoprophylaxis */
      double tSusceptible = tEvent_ + pfsi_ttSusceptiblePf(h);
      h->addEvent2Q(e_pfsi_endchx(tSusceptible,h));
    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_gsvaxx constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * waning effectiveness of GS vaccination; tag: PfSI_GSWane
################################################################################ */

/* constructor */
e_pfsi_gswane::e_pfsi_gswane(double tEvent_, human* h):
  event("PfSI_GSWane",tEvent_,[tEvent_,h](){

    /* vaxx protection wanes */
    double c =  h->get_tile()->get_params()->get_param("Pf_c");
    h->set_c(c);

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_gswane constructor being called at " << this << std::endl;
  #endif

};
