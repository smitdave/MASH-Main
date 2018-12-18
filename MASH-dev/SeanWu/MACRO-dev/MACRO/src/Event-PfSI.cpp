/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfSI Event class
 *
 *  Sean Wu
 *  November 2018
 */

/* PfSI includes */
#include "Event-PfSI.hpp"
#include "SimBite-PfSI.hpp"
#include "Human-PfSI.hpp"

#include "Tile.hpp"

/* movement model */
#include "Event-Move.hpp"

/* utility class includes */
#include "PRNG.hpp"
#include "Parameters.hpp"
#include "Logger.hpp"


/* ################################################################################
 * start a PfSI infection; tag: PfSI_infection
################################################################################ */

/* constructor */
e_pfsi_infect::e_pfsi_infect(double tEvent_, human_pfsi* h):
  event("PfSI_infection",tEvent_,[tEvent_,h](){

    /* no superinfection, and chx blocks new infections */
    if(!h->get_infection() && !h->get_chemoprophylaxis()){

      /* i'm not infected */
      h->set_infection(true);

      /* log this event */
      h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "I" << "\n";

      /* queue clearance event */
      double tEnd = tEvent_ + pfsi_ttClearPf(h);
      h->addEvent2Q(e_pfsi_recover(tEnd,h));

      /* queue fever event */
      double FeverPf = h->get_tile()->get_params()->get_param<double>("FeverPf");
      if(h->get_tile()->get_prng()->get_runif() < FeverPf){
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

/* destructor */
e_pfsi_infect::~e_pfsi_infect(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_infect destructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * end a PfSI infection; tag: PfSI_recovery
################################################################################ */

/* constructor */
e_pfsi_recover::e_pfsi_recover(double tEvent_, human_pfsi* h):
  event("PfSI_recovery",tEvent_,[tEvent_,h](){

    /* i can only recover if i'm infected */
    if(h->get_infection()){
      h->rmTagFromQ("PfSI_fever");
      h->set_infection(false);

      /* log this event */
      h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "S" << "\n";

    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_recover constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_pfsi_recover::~e_pfsi_recover(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_recover destructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * a fever bout; tag: PfSI_fever
################################################################################ */

/* constructor */
e_pfsi_fever::e_pfsi_fever(double tEvent_, human_pfsi* h):
  event("PfSI_fever",tEvent_,[tEvent_,h](){

    /* log this event */
    h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "F" << "\n";

    /* if i get a fever, i'll seek treatment with this probability */
    double TreatPf = h->get_tile()->get_params()->get_param<double>("TreatPf");
    if(h->get_tile()->get_prng()->get_runif() < TreatPf){
      double tTreat = tEvent_ + pfsi_ttTreatPf(h);
      h->addEvent2Q(e_pfsi_treatment(tTreat,h));
    }

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_fever constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_pfsi_fever::~e_pfsi_fever(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_fever destructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * seek treatment; tag: PfSI_treatment
################################################################################ */

/* constructor */
e_pfsi_treatment::e_pfsi_treatment(double tEvent_, human_pfsi* h):
  event("PfSI_treatment",tEvent_,[tEvent_,h](){

    /* clear out this infection */
    h->set_infection(false);
    h->rmTagFromQ("PfSI_recovery");
    h->rmTagFromQ("PfSI_endprophylaxis");
    h->set_chemoprophylaxis(true);

    /* log this event */
    h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "P" << "\n";

    /* initiate a period of protection from chemoprophylaxis */
    double tSusceptible = tEvent_ + pfsi_ttSusceptiblePf(h);
    h->addEvent2Q(e_pfsi_endchx(tSusceptible,h));

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_treatment constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_pfsi_treatment::~e_pfsi_treatment(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_treatment destructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * end of chemoprophylaxis; tag: PfSI_endprophylaxis
################################################################################ */

/* constructor */
e_pfsi_endchx::e_pfsi_endchx(double tEvent_, human_pfsi* h):
  event("PfSI_endprophylaxis",tEvent_,[tEvent_,h](){

    /* chx protection ends */
    h->set_chemoprophylaxis(false);

    /* log this event */
    h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "P2S" << "\n";

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_endchx constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_pfsi_endchx::~e_pfsi_endchx(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_endchx destructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * PE vaccination (sporozoite-blocking vaccination); tag: PfSI_PEVaxx
################################################################################ */

/* constructor */
e_pfsi_pevaxx::e_pfsi_pevaxx(double tEvent_, const bool treat, human_pfsi* h):
  event("PfSI_PEVaxx",tEvent_,[tEvent_,treat,h](){

    /* check if vaxx fails (vaxx efficacy) */
    double PEProtectPf = h->get_tile()->get_params()->get_param<double>("PEProtectPf");
    if(h->get_tile()->get_prng()->get_runif() < PEProtectPf){

      /* lower my probability to get infected by mosquitos */
      double b =  h->get_tile()->get_params()->get_param<double>("Pf_b");
      double peBlockPf = h->get_tile()->get_params()->get_param<double>("peBlockPf");
      h->set_b(b * (1.0 - peBlockPf));

      h->rmTagFromQ("PfSI_PEWane");

      /* log this event */
      h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "PEvaxx" << "\n";

      /* queue the vaxx wane event */
      double tWane = tEvent_ + pfsi_ttPEWanePf(h);
      h->addEvent2Q(e_pfsi_pewane(tWane,h));
    }

    /* if treatment accompanies vaccination */
    if(treat){

      /* copied from e_pfsi_treatment */
      h->set_infection(false);
      h->rmTagFromQ("PfSI_recovery");
      h->rmTagFromQ("PfSI_endprophylaxis");
      h->set_chemoprophylaxis(true);

      /* log this event */
      h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "P" << "\n";

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

/* destructor */
e_pfsi_pevaxx::~e_pfsi_pevaxx(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_pevaxx destructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * PE vaccination (sporozoite-blocking vaccination); tag: PfSI_PEVaxx
################################################################################ */

/* constructor */
e_pfsi_pewane::e_pfsi_pewane(double tEvent_, human_pfsi* h):
  event("PfSI_PEWane",tEvent_,[tEvent_,h](){

    /* vaxx protection wanes */
    double b =  h->get_tile()->get_params()->get_param<double>("Pf_b");
    h->set_b(b);

    /* log this event */
    h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "PEwane" << "\n";

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_pewane constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_pfsi_pewane::~e_pfsi_pewane(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_pewane destructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * GS vaccination (gametocyte-blocking vaccination); tag: PfSI_GSVaxx
################################################################################ */

/* constructor */
e_pfsi_gsvaxx::e_pfsi_gsvaxx(double tEvent_, const bool treat, human_pfsi* h):
  event("PfSI_GSVaxx",tEvent_,[tEvent_,treat,h](){

    /* check if vaxx fails (vaxx efficacy) */
    double GSProtectPf = h->get_tile()->get_params()->get_param<double>("GSProtectPf");
    if(h->get_tile()->get_prng()->get_runif() < GSProtectPf){

      /* lower my probability to infect mosquitos */
      double c = h->get_tile()->get_params()->get_param<double>("Pf_c");
      double gsBlockPf = h->get_tile()->get_params()->get_param<double>("gsBlockPf");
      h->set_c(c * (1.0 - gsBlockPf));

      h->rmTagFromQ("PfSI_GSWane");

      /* log this event */
      h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "GSvaxx" << "\n";

      /* queue the vaxx wane event */
      double tWane = tEvent_ + pfsi_ttGSWanePf(h);
      h->addEvent2Q(e_pfsi_gswane(tWane,h));
    }

    /* if treatment accompanies vaccination */
    if(treat){

      /* copied from e_pfsi_treatment */
      h->set_infection(false);
      h->rmTagFromQ("PfSI_recovery");
      h->rmTagFromQ("PfSI_endprophylaxis");
      h->set_chemoprophylaxis(true);

      /* log this event */
      h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "P" << "\n";

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

/* destructor */
e_pfsi_gsvaxx::~e_pfsi_gsvaxx(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_gsvaxx destructor being called at " << this << std::endl;
  #endif

};


/* ################################################################################
 * waning effectiveness of GS vaccination; tag: PfSI_GSWane
################################################################################ */

/* constructor */
e_pfsi_gswane::e_pfsi_gswane(double tEvent_, human_pfsi* h):
  event("PfSI_GSWane",tEvent_,[tEvent_,h](){

    /* vaxx protection wanes */
    double c =  h->get_tile()->get_params()->get_param<double>("Pf_c");
    h->set_c(c);

    /* log this event */
    h->get_tile()->get_logger()->get_stream("human_inf") << h->get_id() << "," << tEvent_ << "," << "GSwane" << "\n";

  })
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_gswane constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_pfsi_gswane::~e_pfsi_gswane(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_gswane destructor being called at " << this << std::endl;
  #endif

};
