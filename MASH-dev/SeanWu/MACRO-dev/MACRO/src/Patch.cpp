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
#include "Tile.hpp"


/* ################################################################################
 * class boilerplate
################################################################################ */

/* constructor */
patch::patch(
  const Rcpp::List& patch_pars,
  tile* tileP_
) :
  id(Rcpp::as<size_t>(patch_pars["id"])),
  move(Rcpp::as<arma::Row<double> >(patch_pars["move"])),
  bWeightHuman(0.0),
  bWeightZoo(Rcpp::as<double>(patch_pars["bWeightZoo"])),
  bWeightZootox(Rcpp::as<double>(patch_pars["bWeightZootox"])),
  kappa(0.0),
  reservoir(Rcpp::as<bool>(patch_pars["reservoir"])),
  res_EIR(Rcpp::as<double>(patch_pars["res_EIR"])),
  tileP(tileP_)
{

  #ifdef DEBUG_MACRO
  std::cout << "patch " << id << " born at " << this << std::endl;
  #endif

};

/* destructor */
patch::~patch(){

  #ifdef DEBUG_MACRO
  std::cout << "patch " << id << " dying at " << this << std::endl;
  #endif

}

/* debug */
void patch::print(){
  std::cout << "patch: " << id << ", with kappa: " << kappa << ", bWeightHuman: " << bWeightHuman << ", reservoir: " << reservoir << ", res_EIR: " << res_EIR << std::endl;
};


/* ################################################################################
 * derived class factory
################################################################################ */

/* derived classes the factory needs to know about */
#include "Patch-PfSI.hpp"

/* factory method */
std::unique_ptr<patch> patch::factory(const Rcpp::List& patch_pars, const std::string model, tile* tileP_){

  /* make a derived class */
  if(model.compare("PfSI") == 0){
    return std::make_unique<patch_pfsi>(patch_pars,tileP_);
  } else {
    Rcpp::stop("invalid 'model' field in patch::factory\n");
  }
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
