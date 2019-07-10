/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Generic Human Class: humans are specialized by model type
 *
 *  Sean Wu
 *  November 2018
 */

/* PfSI includes */
#include "Human.hpp"
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
human::human(/* basic values */
      const int id_, const size_t home_patch_id_,
      const std::vector<double> trip_duration_, const double trip_frequency_,
      const double bweight_, tile* tileP_,
      /* PfSI specific values */
      const std::string state_, const double age_) :
  id(id_), alive(true), tnow(0.0),
  patch_id(home_patch_id_), home_patch_id(home_patch_id_),
  trip_duration(trip_duration_), trip_frequency(trip_frequency_),
  bweight(bweight_), tileP(tileP_),
  state(state_), age(age_), b(0.), c(0.), kappa(0.), EIR(0.)
{

  #ifdef DEBUG_MACRO
  std::cout << "human " << id << " born at " << this << std::endl;
  #endif

};

/* define virtual destructor is ok */
human::~human(){

  #ifdef MACRO_DEBUG
  std::cout << "human " << id << " dying at " << this << std::endl;
  #endif

};

// /* move operators */
// human::human(human&&) = default;
// human& human::operator=(human&&) = default;


/* ################################################################################
 * event queue
################################################################################ */

/* add an event to my queue */
void human::addEvent2Q(event&& e){
  eventQ.emplace_back(std::make_unique<event>(e));
  std::sort(eventQ.begin(),eventQ.end(),[](const std::unique_ptr<event>& e1, const std::unique_ptr<event>& e2){
    return e1->tEvent < e2->tEvent;
  });
};

/* remove future queued events */
void human::rmTagFromQ(const std::string &tag){
  eventQ.erase(std::remove_if(eventQ.begin(),eventQ.end(),
                              [tag](eventP& e){
                                return(tag.compare(e->tag)==0);
                              }),eventQ.end());
};

/* fire the first event */
void human::fireEvent(){
  if(eventQ.size() > 0){
    tnow = eventQ.front()->tEvent; /* update local simulation time */
    eventQ.front()->eventF();
    eventQ.erase(eventQ.begin());
  }
};

/* print my event queue */
void human::printEventQ(){
  std::cout << "printing human " << id << ", event queue: " << std::endl;
  for(auto it = eventQ.begin(); it != eventQ.end(); it++){
    (*it)->print();
  }
};


/* ################################################################################
 * movement
################################################################################ */

patch* human::get_patch(){
  return tileP->get_patch(patch_id);
};

patch* human::get_home_patch(){
  return tileP->get_patch(home_patch_id);
};


/* ################################################################################
 * biting
################################################################################ */

/* decrement the biting weight where i am */
void human::decrement_bweight(){
  tileP->get_patch(patch_id)->decrement_bWeightHuman(bweight);
};

/* accumulate the biting weight where i am */
void human::accumulate_bweight(){
  tileP->get_patch(patch_id)->accumulate_bWeightHuman(bweight);
};


/* ################################################################################
 * PfSI - specific methods
################################################################################ */

/* ################################################################################
 * simulation
################################################################################ */

void human::simulate(){

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
 * initialize course of infection
################################################################################ */

void human::initialize_courseofinf(){

  /* transmission efficiencies */
  b = tileP->get_params()->get_param<double>("Pf_b");
  c = tileP->get_params()->get_param<double>("Pf_c");

  /* initialize the biting weight where i am at time = 0 */
  accumulate_bweight();

  /* initialize kappa (my infectiousness to mosquitos) */
  update_kappa();

  /* susceptible */
  if(state.compare("S") == 0){
    tileP->get_logger()->get_stream("human_inf") << id << ",0.0,S,S," << patch_id << "\n";
  /* infected & infectious */
  } else if(state.compare("I") == 0){
    addEvent2Q(e_pfsi_initial(0.0,this));
  /* chemoprophylactic protection */
  } else if(state.compare("P") == 0){
    addEvent2Q(e_pfsi_treatment(0.0,this));
  /* bad input */
  } else {
    Rcpp::stop("error: human ",id," has been provided with invalid 'state' argument\n");
  }

};


/* ################################################################################
 * initialize movement
################################################################################ */

void human::initialize_movement(){

  /* queue my first trip */
  size_t dest_id = tileP->get_prng()->get_rcategorical(get_patch()->get_move());
  double trip_t = tileP->get_prng()->get_rexp(trip_frequency);

  // std::cout << " --- human: " << id << " is calling initialize_movement and passing their 'this' pointer to e_move_takeTrip: " << this << " --- \n";

  addEvent2Q(e_move_takeTrip(trip_t,dest_id,this));

};


/* ################################################################################
 * vaccination
################################################################################ */

void human::addVaxx2Q(const Rcpp::List& vaxx){

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
void human::update_kappa(){

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
void human::update_EIR(){

  /* check if in a reservoir */
  if(tileP->get_patch(patch_id)->get_reservoir()){
    EIR = tileP->get_patch(patch_id)->get_res_EIR();
  } else {
    double beta = tileP->get_mosquitos()->get_beta(patch_id);
    EIR = std::fmax(0.0,beta * (bweight / tileP->get_patch(patch_id)->get_bWeightHuman()));
  }

};

/* queue bites for tomorrow based on my EIR */
void human::queue_bites(){

  int nBites = tileP->get_prng()->get_rpois(EIR);

  if(nBites > 0){
    double tnow = tileP->get_tnow();
    for(size_t i=0; i<nBites; i++){
      addEvent2Q(e_pfsi_bite(tnow,this));
    }
  }

};
