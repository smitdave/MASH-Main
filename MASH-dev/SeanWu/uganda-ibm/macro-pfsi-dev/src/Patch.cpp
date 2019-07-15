/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Patch: a cell in a tile (conditional on stuff in the patch, the humans & mosquitos are independent)
 *
 *  Sean Wu
 *  November 2018
*/

#include "Patch.hpp"

// other object includes
#include "Tile.hpp"

// utility includes
#include "Logger.hpp"


/* ################################################################################
 * class boilerplate
################################################################################ */

/* constructor */
patch::patch(
  const Rcpp::List& patch_pars,
  tile* tileP_
) :
  id(Rcpp::as<u_int>(patch_pars["id"])),
  move(Rcpp::as<arma::Row<double> >(patch_pars["move"])),
  bWeightHuman(0.0),
  bWeightZoo(Rcpp::as<double>(patch_pars["bWeightZoo"])),
  bWeightZootox(Rcpp::as<double>(patch_pars["bWeightZootox"])),
  kappa(0.0),
  reservoir(Rcpp::as<bool>(patch_pars["reservoir"])),
  res_EIR(Rcpp::as<double>(patch_pars["res_EIR"])),
  tileP(tileP_),
  inc_travel(0), inc_resident(0)
{
  SIP_visitor.fill(0);
  SIP_resident_home.fill(0);
  SIP_resident_away.fill(0);
};

/* debug */
void patch::print(){
  std::cout << "patch: " << id << ", with kappa: " << kappa << ", bWeightHuman: " << bWeightHuman << ", reservoir: " << reservoir << ", res_EIR: " << res_EIR << std::endl;
};


/* ################################################################################
 * kappa
################################################################################ */

void patch::normalize_kappa(){
  /* no divide by zero errors; also no need to calc kappa for 'reservoir' patches */
  if((bWeightHuman > 0.0) && (!reservoir)){
    kappa = kappa / (bWeightHuman + bWeightZoo + bWeightZootox);
  } else {
    kappa = 0;
  }
}


/* ################################################################################
 * PfSI member functions
################################################################################ */

// humans call this to let the patch know they got sick there
void patch::update_incidence(const bool travel){
  if(travel){
    inc_travel += 1;
  } else {
    inc_resident += 1;
  }
};

// // this is called by humans at the end of their daily simulation
// void patch::update_SIP(const std::string state, const bool travel){
//   if(travel){
//     if(state.compare("S") == 0){
//       SIP_travel.at(0) += 1;
//     } else if(state.compare("I") == 0){
//       SIP_travel.at(1) += 1;
//     } else if(state.compare("P") == 0){
//       SIP_travel.at(2) += 1;
//     } else {
//       Rcpp::stop("error, illegal human state detected: " + state);
//     }
//   } else {
//     if(state.compare("S") == 0){
//       SIP_resident.at(0) += 1;
//     } else if(state.compare("I") == 0){
//       SIP_resident.at(1) += 1;
//     } else if(state.compare("P") == 0){
//       SIP_resident.at(2) += 1;
//     } else {
//       Rcpp::stop("error, illegal human state detected: " + state);
//     }
//   }
// };

// this is called by humans to log state
void patch::update_SIP_visitor(const std::string state){
  if(state.compare("S") == 0){
    SIP_visitor.at(0) += 1;
  } else if(state.compare("I") == 0){
    SIP_visitor.at(1) += 1;
  } else if(state.compare("P") == 0){
    SIP_visitor.at(2) += 1;
  } else {
    Rcpp::stop("error, illegal human state detected: " + state);
  }
};


// this is called by humans to log state
void patch::update_SIP_resident_home(const std::string state){
  if(state.compare("S") == 0){
    SIP_resident_home.at(0) += 1;
  } else if(state.compare("I") == 0){
    SIP_resident_home.at(1) += 1;
  } else if(state.compare("P") == 0){
    SIP_resident_home.at(2) += 1;
  } else {
    Rcpp::stop("error, illegal human state detected: " + state);
  }
};


// this is called by humans to log state
void patch::update_SIP_resident_away(const std::string state){
  if(state.compare("S") == 0){
    SIP_resident_away.at(0) += 1;
  } else if(state.compare("I") == 0){
    SIP_resident_away.at(1) += 1;
  } else if(state.compare("P") == 0){
    SIP_resident_away.at(2) += 1;
  } else {
    Rcpp::stop("error, illegal human state detected: " + state);
  }
};


// reset at the end of the day
void patch::reset_SIP(){
  SIP_visitor.fill(0);
  SIP_resident_home.fill(0);
  SIP_resident_away.fill(0);
  inc_travel = 0;
  inc_resident = 0;
};

// log output at the end of the day
void patch::log_output(){
  int tnow(tileP->get_tnow());
  tileP->get_logger()->get_stream("pfsi") << tnow << "," << id << "," << SIP_visitor.at(0) << "," << SIP_resident_home.at(0) << "," << SIP_resident_away.at(0) << "," << SIP_visitor.at(1) << "," << SIP_resident_home.at(1) << "," << SIP_resident_away.at(1) << "," << SIP_visitor.at(2) << "," << SIP_resident_home.at(2) << "," << SIP_resident_away.at(2) << "," << inc_resident << "," << inc_travel << "\n";
  reset_SIP();
};
