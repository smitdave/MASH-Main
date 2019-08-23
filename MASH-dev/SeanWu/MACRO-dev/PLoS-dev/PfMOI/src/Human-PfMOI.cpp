/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfMOI human
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  July 2019
*/

#include "Human-PfMOI.hpp"

// event includes
#include "Event-PfMOI.hpp"
#include "Event-Move.hpp"
#include "Event.hpp"

// biting includes
#include "Human-QueueBites.hpp"

// other object includes
#include "Mosquito-RM.hpp"
#include "Patch.hpp"
#include "Tile.hpp"

// utility includes
#include "RNG.hpp"
#include "Parameters.hpp"


/* ################################################################################
 * class boilerplate
################################################################################ */

// constructor
human::human(/* basic values */
  const int id_, const u_int home_patch_id_,
  const std::vector<double> trip_duration_, const double trip_frequency_,
  const double bweight_, tile* tileP_,
  // PfMOI CoI model
  const u_int MOI_, const bool chx_, const double age_
) :
  id(id_), alive(true), tnow(0.),
  patch_id(home_patch_id_), home_patch_id(home_patch_id_), travel(false),
  trip_duration(trip_duration_), trip_frequency(trip_frequency_),
  bweight(bweight_), tileP(tileP_),
  MOI(MOI_), chx(chx_), b(0.), c(0.), age(age_), kappa(0.), EIR(0.)
{};

// destructor
human::~human() = default;


/* ################################################################################
 * event queue
################################################################################ */

/* add an event to my queue */
// NOW call std::move into the call to std::make_unique
void human::addEvent2Q(event&& e){
  eventQ.emplace_back(std::make_unique<event>(std::move(e)));
  std::sort(eventQ.begin(),eventQ.end(),[](const std::unique_ptr<event>& e1, const std::unique_ptr<event>& e2){
    return e1->tEvent < e2->tEvent;
  });
};

/* remove future queued events */
void human::rmTagFromQ(const std::string &tag){
  eventQ.erase(std::remove_if(eventQ.begin(),eventQ.end(),
                              [tag](const std::unique_ptr<event>& e){
                                return(tag.compare(e->tag)==0);
                              }),eventQ.end());
};

/* fire the first event */
void human::fireEvent(){
  if(!eventQ.empty()){
    tnow = eventQ.front()->tEvent; /* update local simulation time */
    eventQ.front()->eventF();
    eventQ.erase(eventQ.begin());
  }
};

/* print my event queue */
void human::printEventQ(){
  std::cout << "printing human " << id << ", event queue: " << std::endl;
  for(auto it = eventQ.begin(); it != eventQ.end(); it++){
    it->get()->print();
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
 * PfMOI
 * MODEL SPECIFIC CODE
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

  /* update patch information on state */
  log_pfmoi();
};


/* ################################################################################
 * initialize biting algorithm
################################################################################ */

void human::initialize_biting(const int algorithm, void* data){
  biting_algorithm = queue_bites::factory(algorithm,this,data);
};


/* ################################################################################
 * initialize course of infection
################################################################################ */

void human::initialize_courseofinf(){

  /* transmission efficiencies */
  b = tileP->get_params()->get_param("Pf_b");
  c = tileP->get_params()->get_param("Pf_c");

  /* initialize the biting weight where i am at time = 0 */
  accumulate_bweight();

  /* initialize kappa (my infectiousness to mosquitos) */
  update_kappa();

  // infected
  if(MOI > 0){
    addEvent2Q(e_pfmoi_initial(-1.,MOI,this));
  } else if(chx){
    addEvent2Q(e_pfmoi_treatment(-1.,this));
  }
};


/* ################################################################################
 * initialize movement
################################################################################ */

void human::initialize_movement(){

  /* queue my first trip */
  int dest_id = rcategorical(get_patch()->get_move());
  double trip_t = R::rexp(1./trip_frequency);

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
    addEvent2Q(e_pfmoi_pevaxx(tEvent,treat,this));
  } else if(vaxx_t.compare("GS") == 0){
    addEvent2Q(e_pfmoi_gsvaxx(tEvent,treat,this));
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
    if(MOI > 0){
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

  int nbite = biting_algorithm->sample_bites(EIR);

  if(nbite > 0){
    double tnow = tileP->get_tnow();
    for(size_t i=0; i<nbite; i++){
      addEvent2Q(e_pfmoi_bite(tnow,this));
    }
  }

};


/* ################################################################################
 * PfMOI logging
################################################################################ */

void human::log_pfmoi(){

  if(travel){
    get_home_patch()->update_SIP_resident_away(MOI,chx);
    get_patch()->update_SIP_visitor(MOI,chx);
  } else {
    get_home_patch()->update_SIP_resident_home(MOI,chx);
  }

};
