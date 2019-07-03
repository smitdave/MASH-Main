/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Patch: the one for PfSI
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  July 2019
 */

/* class header */
#include "Patch-PfSI.hpp"

/* MASH headers */
#include "Human-PfSI.hpp"
#include "Event.hpp"
#include "Tile.hpp"
#include "Logger.hpp"


/* ################################################################################
 * constructor & destructor
################################################################################ */

patch_pfsi::patch_pfsi(const Rcpp::List& patch_pars, tile* tileP_) :
  patch(patch_pars,tileP_), inc(0)
{
  SIP_travel.fill(0);
  SIP_resident.fill(0);
};

patch_pfsi::~patch_pfsi(){};


/* ################################################################################
 * logging interface
################################################################################ */

// log an individual's state
void patch_pfsi::log_human(void* human){

  human_pfsi* h_ptr = static_cast<human_pfsi*>(human);

  /* is this person a resident or traveller */
  bool travel = id == h_ptr->get_patch_id();
  std::string state(h_ptr->get_state());

  /* update appropriately */
  if(state.compare("S") == 0){
    if(travel){
      SIP_travel[0] += 1;
    } else {
      SIP_resident[0] += 1;
    }
  } else if(state.compare("I") == 0){
    if(travel){
      SIP_travel[1] += 1;
    } else {
      SIP_resident[1] += 1;
    }
  } else if(state.compare("P") == 0){
    if(travel){
      SIP_travel[2] += 1;
    } else {
      SIP_resident[2] += 1;
    }
  } else {
    std::string msg = "error: patch_pfsi::log_human has unrecognized human state: " + state;
    Rcpp::stop(msg);
  }


}

// incidence logging
void patch_pfsi::log_incidence(void* human){
  inc += 1;
}

// log output at end of day
void patch_pfsi::log_output(const int tnow){

  /* write out this patch */
  tileP->get_logger()->get_stream("patch_ts") << tnow << "," << id << "," << SIP_resident[0] << "," << SIP_travel[0] << "," << SIP_resident[1] << "," << SIP_travel[1] << "," << SIP_resident[2] << "," << SIP_travel[2] << "," << inc << "\n";

  /* zero out data before the clock strikes midnight */
  SIP_travel.fill(0);
  SIP_resident.fill(0);
  inc = 0;
};
