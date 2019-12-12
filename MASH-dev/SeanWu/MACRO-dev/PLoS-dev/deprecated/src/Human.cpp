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

#include "Event.hpp"
#include "Human.hpp"
#include "Tile.hpp"
#include "Patch.hpp"
#include "Human-QueueBites.hpp"


/* ################################################################################
 * class boilerplate
################################################################################ */

human::human(const int id_, const size_t home_patch_id_,
      const std::vector<double> trip_duration_, const double trip_frequency_,
      const double bweight_,
      const int bite_algorithm, void* bite_data,
      tile* tileP_) :
  id(id_), alive(true), tnow(0.0),
  patch_id(home_patch_id_), home_patch_id(home_patch_id_),
  trip_duration(trip_duration_), trip_frequency(trip_frequency_),
  bweight(bweight_), tileP(tileP_)
{
  // initialize the specific sampling algorithm for queueing bites
  queue_bites_impl = queue_bites::factory(bite_algorithm, this, bite_data);

};

// destructor
human::~human() = default;


/* ################################################################################
 * derived class factory
################################################################################ */

/* derived classes the factory needs to know about */
#include "Human-PfSI.hpp"

/* factory method */
std::unique_ptr<human> human::factory(const Rcpp::List& human_pars, tile* tileP_){

  /* check what model we need to make */
  std::string model(Rcpp::as<std::string>(human_pars["model"]));

  /* make a derived class */
  if(model.compare("PfSI") == 0){
    return std::make_unique<human_pfsi>(human_pars,tileP_);
  } else {
    Rcpp::stop("invalid 'model' field in 'human_pars'\n");
  }
};


/* ################################################################################
 * event queue
################################################################################ */

/* add an event to my queue */
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

bool human::get_travel(){
  if(patch_id != home_patch_id){
    return true;
  } else {
    return false;
  }
}


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
