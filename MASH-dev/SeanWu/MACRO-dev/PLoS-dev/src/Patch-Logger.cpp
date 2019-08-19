/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  A hierarchy of classes that live in the Patch.
 *
 *  Sean Wu
 *  August 2019
*/

// header
#include "Patch-Logger.hpp"

// other project headers
#include "Tile.hpp"
#include "Logger.hpp"
#include "Human.hpp"


/* ################################################################################
 * base logging class
################################################################################ */

// constructor
patch_logger::patch_logger(patch* const patchP_) :
  patchP(patchP_)
{};

// factory method
std::unique_ptr<patch_logger> patch_logger::factory(const std::string model){
  if(model.compare("pfsi") == 0){
    // return std::make_unique<human_pfsi>(human_pars,tileP_);
  } else if(model.compare("pfmoi") == 0){

  } else {
    Rcpp::stop("illegal string 'model' passed to patch_logger::factory\n");
  }
};


/* ################################################################################
 * PfSI logging class
################################################################################ */

// constructor
patch_logger_pfsi::patch_logger_pfsi(patch* const patchP_) :
  patch_logger(patchP_),
  inc_travel(0),
  inc_resident(0)
{
  SIP_visitor.fill(0);
  SIP_resident_home.fill(0);
  SIP_resident_away.fill(0);
};

// destructor
patch_logger_pfsi::~patch_logger_pfsi() = default;

// log incidence information
void patch_logger_pfsi::log_incidence(const bool travel){
  if(travel){
    inc_travel += 1;
  } else {
    inc_resident += 1;
  }
};

// log a human's state if this is home
void patch_logger_pfsi::log_human_home(const human* const h){

  // cast the pointer to pfsi human and log appropriately

};

// log a human's state if this is not home
void patch_logger_pfsi::log_human_travel(const human* const h){

  // cast the pointer to pfsi human and log appropriately

};

// write output daily
void patch_logger_pfsi::log_output(){

};

// reset data
void patch_logger_pfsi::reset_log(){
  SIP_visitor.fill(0);
  SIP_resident_home.fill(0);
  SIP_resident_away.fill(0);
  inc_travel = 0;
  inc_resident = 0;
}


/* ################################################################################
 * PfMOI logging class
################################################################################ */

// constructor
patch_logger_pfmoi::patch_logger_pfmoi(patch* const patchP_) :
  patch_logger(patchP_),
  inc_travel(0),
  inc_resident(0)
{
  SIP_visitor.fill(0);
  SIP_resident_home.fill(0);
  SIP_resident_away.fill(0);
};

// destructor
patch_logger_pfmoi::~patch_logger_pfmoi() = default;

// log incidence information
void patch_logger_pfmoi::log_incidence(const bool travel){
  if(travel){
    inc_travel += 1;
  } else {
    inc_resident += 1;
  }
};

// log a human's state if this is home
void patch_logger_pfmoi::log_human_home(const human* const h){

  // cast the pointer to pfmoi human and log appropriately

};

// log a human's state if this is not home
void patch_logger_pfmoi::log_human_travel(const human* const h){

  // cast the pointer to pfsi human and log appropriately

};

// write output daily
void patch_logger_pfmoi::log_output(){

};

// reset data
void patch_logger_pfmoi::reset_log(){
  SIP_visitor.fill(0);
  SIP_resident_home.fill(0);
  SIP_resident_away.fill(0);
  inc_travel = 0;
  inc_resident = 0;
}
