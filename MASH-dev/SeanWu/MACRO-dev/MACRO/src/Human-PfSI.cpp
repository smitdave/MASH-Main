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
#include "Logger.hpp"


/* ################################################################################
 * class boilerplate
################################################################################ */

/* constructor */
human_pfsi::human_pfsi(
  const Rcpp::List& human_pars,
  tile* tileP_
) :
  human(Rcpp::as<int>(human_pars["id"]),
        Rcpp::as<size_t>(human_pars["home_patch_id"]),
        Rcpp::as<double>(human_pars["trip_duration"]),
        Rcpp::as<double>(human_pars["trip_frequency"]),
        Rcpp::as<double>(human_pars["bweight"]),
        tileP_),
  state("S"),
  /* THIS STUFF IS JUST TO RUN THE PRISM DATA */
  k(Rcpp::as<std::vector<double> >(human_pars["k"])),
  /* END PRISM DATA STUFF */
  b(0.0), c(0.0),
  age(Rcpp::as<double>(human_pars["age"])),
  kappa(0.0), EIR(0.0)
{

  std::string state_t0(Rcpp::as<std::string>(human_pars["state"]));

  /* transmission efficiencies */
  b = tileP->get_params()->get_param<double>("Pf_b");
  c = tileP->get_params()->get_param<double>("Pf_c");

  /* initialize the biting weight where i am at time = 0 */
  accumulate_bweight();

  /* initialize kappa (my infectiousness to mosquitos) */
  update_kappa();

  /* susceptible */
  if(state_t0.compare("S") == 0){
    tileP->get_logger()->get_stream("human_inf") << id << ",0.0,S,S," << patch_id << "\n";
  /* infected & infectious */
  } else if(state_t0.compare("I") == 0){
    addEvent2Q(e_pfsi_infect(0.0,this));
  /* chemoprophylactic protection */
  } else if(state_t0.compare("P") == 0){
    addEvent2Q(e_pfsi_treatment(0.0,this));
  /* bad input */
  } else {
    Rcpp::stop("error: human ",id," has been provided with invalid 'state' argument\n");
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
 * vaccination
################################################################################ */

void human_pfsi::addVaxx2Q(const Rcpp::List& vaxx){

  /* pull out information */
  std::string vaxx_t(Rcpp::as<std::string>(vaxx["type"]));
  double tEvent = Rcpp::as<double>(vaxx["tEvent"]);
  bool treat = Rcpp::as<bool>(vaxx["treat"]);

  /* add the vaccine event */
  if(vaxx_t.compare("PE") == 0){
    addEvent2Q(e_pfsi_pevaxx(tEvent,treat,this));
  } else if(vaxx_t.compare("GS") == 0){
    addEvent2Q(e_pfsi_gsvaxx(tEvent,treat,this));
  } else {
    Rcpp::stop("invalid vaccine type found; please check all events in 'vaxx_events' are valid");
  }

};


/* ################################################################################
 * kappa, EIR, biting
################################################################################ */

/* unnormalized kappa for an individual */
void human_pfsi::update_kappa(){

  /* only calculate kappa if i'm not in a reservoir */
  if(!tileP->get_patch(patch_id)->get_reservoir()){

    kappa = 0.0;
    if(state.compare("I") == 0){
      kappa = c * bweight;
    }
    tileP->get_patch(patch_id)->accumulate_kappa(kappa);

  }

};

/* EIR: rate I am getting bitten by mosquitos right now */
void human_pfsi::update_EIR(){

  /* check if in a reservoir */
  if(tileP->get_patch(patch_id)->get_reservoir()){
    // EIR = tileP->get_patch(patch_id)->get_res_EIR();

    /* THIS STUFF IS JUST TO RUN THE PRISM DATA */
    size_t t = tileP->get_tnow();
    EIR = tileP->get_patch(patch_id)->get_res_EIR_prism(t);
    /* END PRISM DATA STUFF */

  } else {
    double beta = tileP->get_mosquitos()->get_beta(patch_id);
    EIR = std::fmax(0.0,beta * (bweight / tileP->get_patch(patch_id)->get_bWeightHuman()));
  }

};

/* queue bites for tomorrow based on my EIR */
void human_pfsi::queue_bites(){

  // int nBites = tileP->get_prng()->get_rpois(EIR);

  /* THIS STUFF IS JUST TO RUN THE PRISM DATA */
  size_t t = tileP->get_tnow();
  // double lambda = tileP->get_prng()->get_gamma(size, (1 - prob) / prob); /* .gg functions */
  double lambda = tileP->get_prng()->get_gamma(k[t], EIR/k[t]); /* .ff functions */
  int nBites = tileP->get_prng()->get_rpois(lambda);
  /* END PRISM DATA STUFF */

  if(nBites > 0){
    double tnow = tileP->get_tnow();
    for(size_t i=0; i<nBites; i++){
      addEvent2Q(e_pfsi_bite(tnow,this));
    }
  }

};
