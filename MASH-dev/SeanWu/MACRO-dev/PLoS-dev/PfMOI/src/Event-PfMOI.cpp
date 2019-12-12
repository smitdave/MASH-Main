/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfMOI Event class
 *  (include SimBite for PfMOI)
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  August 2019
*/

#include "Event-PfMOI.hpp"

// other object includes
#include "Human-PfMOI.hpp"
#include "Tile.hpp"
#include "Patch.hpp"

// utiltiy includes
#include "Parameters.hpp"


/* ################################################################################
 * sample waiting time (hazard functions for events)
################################################################################ */

/* duration of infection */
double pfmoi_ttClearPf(human* h){

  double DurationPf = h->get_tile()->get_params()->get_param("DurationPf");
  double sigma = h->get_tile()->get_params()->get_param("sigmaPf");
  double rho = (1./DurationPf) * std::pow((double)h->get_MOI(),sigma);

  return R::rexp(1./rho);
};

/* duration of latent period */
double psfi_ttInfectionPf(human* h){
  double LatentPf = h->get_tile()->get_params()->get_param("LatentPf");
  return LatentPf;
};

/* timing of fever event (when does it start relative to infection?) */
double pfmoi_ttFeverPf(human* h){
  double mnFeverPf = h->get_tile()->get_params()->get_param("mnFeverPf");
  return mnFeverPf;
};

/* timing of treatment event (when does it occur relative to infection?) */
double pfmoi_ttTreatPf(human* h){
  double mnTreatPf = h->get_tile()->get_params()->get_param("mnTreatPf");
  return mnTreatPf;
};

/* duration of protection from chemoprophylaxis */
double pfmoi_ttSusceptiblePf(human* h){
  double mnChemoprophylaxisPf = h->get_tile()->get_params()->get_param("mnChemoprophylaxisPf");
  return mnChemoprophylaxisPf;
};

/* duration of protection by PE vaxx */
double pfmoi_ttPEWanePf(human* h){
  double mnPEPf = h->get_tile()->get_params()->get_param("mnPEPf");
  double vrPEPf = h->get_tile()->get_params()->get_param("vrPEPf");
  return std::fmax(0., R::rnorm(mnPEPf,vrPEPf));
};

/* duration of protection by GS vaxx */
double pfmoi_ttGSWanePf(human* h){
  double mnGSPf = h->get_tile()->get_params()->get_param("mnGSPf");
  double vrGSPf = h->get_tile()->get_params()->get_param("vrGSPf");
  return std::fmax(0., R::rnorm(mnGSPf,vrGSPf));
};


/* ################################################################################
 * SimBite events
################################################################################ */

/* constructor */
e_pfmoi_bite::e_pfmoi_bite(double tEvent_, human* h):
  event("pfmoi_SimBite",tEvent_,[tEvent_,h](){

    /* transmission efficiency */
    double b = h->get_b();
    if(R::runif(0.,1.) < b){
      double tInfStart = tEvent_ + psfi_ttInfectionPf(h);
      h->addEvent2Q(e_pfmoi_infect(tInfStart,h));
    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_bite constructor being called at " << this << std::endl;
  #endif

};



/* ################################################################################
 * pfmoi initialization: start an infection at t=0
################################################################################ */

/* constructor */
e_pfmoi_initial::e_pfmoi_initial(double tEvent_, const u_int MOI_init, human* h):
  event("pfmoi_initial",tEvent_,[tEvent_,MOI_init,h](){

    // set MOI
    h->set_MOI(MOI_init);

    // queue 1st clearance event with correct clock
    h->rmTagFromQ("pfmoi_clear");
    double tEnd = tEvent_ + pfmoi_ttClearPf(h);
    h->addEvent2Q(e_pfmoi_clear(tEnd,h));

    // fever may occur
    double FeverPf = h->get_tile()->get_params()->get_param("FeverPf");
    if(R::runif(0.,1.) < FeverPf){
      double tFever = tEvent_ + pfmoi_ttFeverPf(h);
      h->addEvent2Q(e_pfmoi_fever(tFever,h));
    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_initial constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * start a pfmoi infection; tag: pfmoi_infection
################################################################################ */

/* constructor */
e_pfmoi_infect::e_pfmoi_infect(double tEvent_, human* h):
  event("pfmoi_infection",tEvent_,[tEvent_,h](){

    // if unprotected by drugs, increment MOI
    if(!h->get_chx()){

      // increment MOI
      h->inc_MOI();

      // increment incidence where i am
      h->get_patch()->update_incidence(h->get_travel());

      // queue clearance event: first erase old clock reading and re-sample
      h->rmTagFromQ("pfmoi_clear");
      double tEnd = tEvent_ + pfmoi_ttClearPf(h);
      h->addEvent2Q(e_pfmoi_clear(tEnd,h));

      // fever may occur
      double FeverPf = h->get_tile()->get_params()->get_param("FeverPf");
      if(R::runif(0.,1.) < FeverPf){
        double tFever = tEvent_ + pfmoi_ttFeverPf(h);
        h->addEvent2Q(e_pfmoi_fever(tFever,h));
      }

    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_infect constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * clear a pfmoi infection; tag: pfmoi_clear
################################################################################ */

/* constructor */
e_pfmoi_clear::e_pfmoi_clear(double tEvent_, human* h):
  event("pfmoi_clear",tEvent_,[tEvent_,h](){

    // decrement MOI (clear infection)
    h->dec_MOI();
    h->rmTagFromQ("pfmoi_fever");

    if(h->get_MOI() > 0){

      // sample clock for next clearance event with correct hazard
      // we do this here because by definition there is no clock for clearing the next infection (this one gets removed)
      // this makes sure its possible to clear a further infection before the next one arrives
      double tEnd = tEvent_ + pfmoi_ttClearPf(h);
      h->addEvent2Q(e_pfmoi_clear(tEnd,h));

    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_clear constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * a fever bout; tag: pfmoi_fever
################################################################################ */

/* constructor */
e_pfmoi_fever::e_pfmoi_fever(double tEvent_, human* h):
  event("pfmoi_fever",tEvent_,[tEvent_,h](){

    /* if i get a fever, i'll seek treatment with this probability */
    double TreatPf = h->get_tile()->get_params()->get_param("TreatPf");
    if(R::runif(0.,1.) < TreatPf){
      double tTreat = tEvent_ + pfmoi_ttTreatPf(h);
      h->addEvent2Q(e_pfmoi_treatment(tTreat,h));
    }


  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_fever constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * seek treatment; tag: pfmoi_treatment
################################################################################ */

/* constructor */
e_pfmoi_treatment::e_pfmoi_treatment(double tEvent_, human* h):
  event("pfmoi_treatment",tEvent_,[tEvent_,h](){

    /* clear out all infections & initiate chemoprotection */
    h->set_chx(true);
    h->zero_MOI();

    /* remove all queued events fom previous infections */
    h->rmTagFromQ("pfmoi_clear");
    h->rmTagFromQ("pfmoi_fever");
    h->rmTagFromQ("pfmoi_endprophylaxis");

    /* initiate a period of protection from chemoprophylaxis */
    double tSusceptible = tEvent_ + pfmoi_ttSusceptiblePf(h);
    h->addEvent2Q(e_pfmoi_endchx(tSusceptible,h));

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_treatment constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * end of chemoprophylaxis; tag: pfmoi_endprophylaxis
################################################################################ */

/* constructor */
e_pfmoi_endchx::e_pfmoi_endchx(double tEvent_, human* h):
  event("pfmoi_endprophylaxis",tEvent_,[tEvent_,h](){

    /* chx protection ends */
    h->set_chx(false);

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_endchx constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * PE vaccination (sporozoite-blocking vaccination); tag: pfmoi_PEVaxx
################################################################################ */

/* constructor */
e_pfmoi_pevaxx::e_pfmoi_pevaxx(double tEvent_, const bool treat, human* h):
  event("pfmoi_PEVaxx",tEvent_,[tEvent_,treat,h](){

    /* check if vaxx fails (vaxx efficacy) */
    double PEProtectPf = h->get_tile()->get_params()->get_param("PEProtectPf");
    if(R::runif(0.,1.) < PEProtectPf){

      /* lower my probability to get infected by mosquitos */
      double b =  h->get_tile()->get_params()->get_param("Pf_b");
      double peBlockPf = h->get_tile()->get_params()->get_param("peBlockPf");
      h->set_b(b * (1.0 - peBlockPf));

      h->rmTagFromQ("pfmoi_PEWane");

      /* queue the vaxx wane event */
      double tWane = tEvent_ + pfmoi_ttPEWanePf(h);
      h->addEvent2Q(e_pfmoi_pewane(tWane,h));
    }

    /* if treatment accompanies vaccination */
    if(treat){

      h->addEvent2Q(e_pfmoi_treatment(tEvent_+0.001,h));

    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_pevaxx constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * PE vaccination (sporozoite-blocking vaccination); tag: pfmoi_PEWane
################################################################################ */

/* constructor */
e_pfmoi_pewane::e_pfmoi_pewane(double tEvent_, human* h):
  event("pfmoi_PEWane",tEvent_,[tEvent_,h](){

    /* vaxx protection wanes */
    double b =  h->get_tile()->get_params()->get_param("Pf_b");
    h->set_b(b);

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_pewane constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * GS vaccination (gametocyte-blocking vaccination); tag: pfmoi_GSVaxx
################################################################################ */

/* constructor */
e_pfmoi_gsvaxx::e_pfmoi_gsvaxx(double tEvent_, const bool treat, human* h):
  event("pfmoi_GSVaxx",tEvent_,[tEvent_,treat,h](){

    /* check if vaxx fails (vaxx efficacy) */
    double GSProtectPf = h->get_tile()->get_params()->get_param("GSProtectPf");
    if(R::runif(0.,1.) < GSProtectPf){

      /* lower my probability to infect mosquitos */
      double c = h->get_tile()->get_params()->get_param("Pf_c");
      double gsBlockPf = h->get_tile()->get_params()->get_param("gsBlockPf");
      h->set_c(c * (1.0 - gsBlockPf));

      h->rmTagFromQ("pfmoi_GSWane");

      /* queue the vaxx wane event */
      double tWane = tEvent_ + pfmoi_ttGSWanePf(h);
      h->addEvent2Q(e_pfmoi_gswane(tWane,h));
    }

    /* if treatment accompanies vaccination */
    if(treat){

      h->addEvent2Q(e_pfmoi_treatment(tEvent_+0.001,h));

    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_gsvaxx constructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * waning effectiveness of GS vaccination; tag: pfmoi_GSWane
################################################################################ */

/* constructor */
e_pfmoi_gswane::e_pfmoi_gswane(double tEvent_, human* h):
  event("pfmoi_GSWane",tEvent_,[tEvent_,h](){

    /* vaxx protection wanes */
    double c =  h->get_tile()->get_params()->get_param("Pf_c");
    h->set_c(c);

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfmoi_gswane constructor being called at " << this << std::endl;
  #endif

};
