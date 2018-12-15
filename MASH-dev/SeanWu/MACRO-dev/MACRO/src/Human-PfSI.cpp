/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfSI Human Class
 *
 *  Sean Wu
 *  November 2018
 */

/* PfSI includes */
#include "Human-PfSI.hpp"
#include "Event-PfSI.hpp"
#include "SimBite-PfSI.hpp"

/* movement model */
#include "Event-Move.hpp"

/* other includes */
#include "Event.hpp"
#include "Tile.hpp"
#include "Patch.hpp"
#include "Mosquito.hpp"

/* utility class includes */
#include "Parameters.hpp"
#include "PRNG.hpp"


/* ################################################################################
 * constructor & destructor
################################################################################ */

human_pfsi::human_pfsi(const int id_, const size_t home_patch_id_,
      const double trip_duration_, const double trip_frequency_,
      const double bweight_, tile* tileP_,
      /* human_pfsi specific arguments */
      const double age_, const bool inf_, const bool chx_) :
  human(id_,home_patch_id_,
        trip_duration_,trip_frequency_,
        bweight_,tileP_),
  infection(inf_), chemoprophylaxis(chx_), b(0.0), c(0.0), age(age_), kappa(0.0), EIR(0.0)
{

  /* transmission efficiencies */
  b = tileP->get_params()->get_param<double>("Pf_b");
  c = tileP->get_params()->get_param<double>("Pf_c");

  /* initialize kappa (my infectiousness to mosquitos) */
  update_kappa();

  /* logically inconsistent to have an individual who is infected AND on chemoprophylaxis (in PfSI) */
  if(infection && chemoprophylaxis){
    Rcpp::stop("error: human ",id," cannot both have active infection and under chemoprophylactic protection\n");
  }

  /* if infected, queue the initial event */
  if(infection){
    addEvent2Q(e_pfsi_infect(0.0,this));
  }

  /* chemoprophylaxis: queue up when protection expires */
  if(chemoprophylaxis){
    addEvent2Q(e_pfsi_endchx(0.0,this));
  }

  #ifdef DEBUG_MACRO
  std::cout << "human_pfsi " << " born at " << this << std::endl;
  #endif
};

human_pfsi::~human_pfsi(){
  #ifdef DEBUG_MACRO
  std::cout << "human_pfsi " << " dying at " << this << std::endl;
  #endif
};

/* move operators */
human_pfsi::human_pfsi(human_pfsi&&) = default;
human_pfsi& human_pfsi::operator=(human_pfsi&&) = default;


/* ################################################################################
 * simulation
################################################################################ */

void human_pfsi::simulate(){

  /* fire all events that occur on this time step */
  while(eventQ.size() > 0 && eventQ.front()->tEvent < tileP->get_tnow()){
    fireEvent();
  }

  /* update kappa (my infectiousness to mosquitos) */
  update_kappa();

  /* update my EIR */
  update_EIR();

  /* queue bites */
  queue_bites();
};


/* ################################################################################
 * initialize movement
################################################################################ */

void human_pfsi::initialize_movement(){

  /* queue my first trip */
  size_t dest_id = tileP->get_prng()->get_rcategorical(get_patch()->get_move());
  double trip_t = tileP->get_prng()->get_rexp(trip_frequency);

  addEvent2Q(e_move_takeTrip(trip_t,dest_id,this));

};


/* ################################################################################
 * kappa, EIR, biting
################################################################################ */

/* unnormalized kappa for an individual */
void human_pfsi::update_kappa(){

  double inf = static_cast<double>(infection);
  kappa = inf * c * bweight;

  get_patch()->accumulate_kappa(kappa);

};

/* EIR: rate I am getting bitten by mosquitos right now */
void human_pfsi::update_EIR(){

  /* check if in a reservoir */
  if(tileP->get_patch(patch_id)->get_reservoir()){
    EIR = tileP->get_patch(patch_id)->get_res_EIR();
  } else {
    double beta = tileP->get_mosquitos()->get_beta(patch_id);
    EIR = beta * (bweight / tileP->get_patch(patch_id)->get_bWeightHuman());
  }

};

/* queue bites for tomorrow based on my EIR */
void human_pfsi::queue_bites(){

  int nBites = tileP->get_prng()->get_rpois(EIR);

  if(nBites > 0){
    double tnow = tileP->get_tnow();
    for(size_t i=0; i<nBites; i++){
      addEvent2Q(e_pfsi_bite(tnow,this));
    }
  }

};
