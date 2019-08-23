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
#include "Stats.hpp"


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
  inc_travel(0), inc_resident(0),
  MOI_visitor(std::make_unique<RunningStat>()),
  MOI_resident_home(std::make_unique<RunningStat>()),
  MOI_resident_away(std::make_unique<RunningStat>())
{
  SIP_visitor.fill(0);
  SIP_resident_home.fill(0);
  SIP_resident_away.fill(0);
};

// dtor
patch::~patch() = default;

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
 * PfMOI member functions
################################################################################ */

// humans call this to let the patch know they got sick there
void patch::update_incidence(const bool travel){
  if(travel){
    inc_travel += 1;
  } else {
    inc_resident += 1;
  }
};

// this is called by humans to log state
void patch::update_SIP_visitor(const u_int moi, const bool chx){

  if(moi > 0 && !chx){
    SIP_visitor.at(1) += 1;
    MOI_visitor->Push((double)moi);
  } else if(moi == 0 && !chx){
    SIP_visitor.at(0) += 1;
    MOI_visitor->Push((double)moi);
  } else if(moi == 0 && chx){
    SIP_visitor.at(2) += 1;
    MOI_visitor->Push((double)moi);
  } else {
    std::string error = "error, illegal human state detected, MOI: " + std::to_string(moi) + " chx: " + std::to_string(chx);
    Rcpp::stop(error);
  }

};


// this is called by humans to log state
void patch::update_SIP_resident_home(const u_int moi, const bool chx){

  if(moi > 0 && !chx){
    SIP_resident_home.at(1) += 1;
    MOI_resident_home->Push((double)moi);
  } else if(moi == 0 && !chx){
    SIP_resident_home.at(0) += 1;
    MOI_resident_home->Push((double)moi);
  } else if(moi == 0 && chx){
    SIP_resident_home.at(2) += 1;
    MOI_resident_home->Push((double)moi);
  } else {
    std::string error = "error, illegal human state detected, MOI: " + std::to_string(moi) + " chx: " + std::to_string(chx);
    Rcpp::stop(error);
  }

};


// this is called by humans to log state
void patch::update_SIP_resident_away(const u_int moi, const bool chx){

  if(moi > 0 && !chx){
    SIP_resident_away.at(1) += 1;
    MOI_resident_away->Push((double)moi);
  } else if(moi == 0 && !chx){
    SIP_resident_away.at(0) += 1;
    MOI_resident_away->Push((double)moi);
  } else if(moi == 0 && chx){
    SIP_resident_away.at(2) += 1;
    MOI_resident_away->Push((double)moi);
  } else {
    std::string error = "error, illegal human state detected, MOI: " + std::to_string(moi) + " chx: " + std::to_string(chx);
    Rcpp::stop(error);
  }

};


// reset at the end of the day
void patch::reset_SIP(){
  SIP_visitor.fill(0);
  SIP_resident_home.fill(0);
  SIP_resident_away.fill(0);
  inc_travel = 0;
  inc_resident = 0;
  MOI_visitor->Clear();
  MOI_resident_home->Clear();
  MOI_resident_away->Clear();
};

// log output at the end of the day
void patch::log_output(){
  int tnow(tileP->get_tnow());
  tileP->get_logger()->get_stream("pfmoi") << tnow << "," << id << "," << SIP_visitor.at(0) << "," << SIP_resident_home.at(0) << "," << SIP_resident_away.at(0) << "," << SIP_visitor.at(1) << "," << SIP_resident_home.at(1) << "," << SIP_resident_away.at(1) << "," << SIP_visitor.at(2) << "," << SIP_resident_home.at(2) << "," << SIP_resident_away.at(2) << "," << inc_resident << "," << inc_travel << "," << MOI_visitor->Mean() << "," << MOI_visitor->StandardDeviation() << "," << MOI_resident_home->Mean() << "," << MOI_resident_home->StandardDeviation() << "," << MOI_resident_away->Mean() << "," << MOI_resident_away->StandardDeviation() << "\n";
  reset_SIP();
};
