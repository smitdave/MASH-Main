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
// #include "Debug.hpp"
#include "Patch.hpp"
#include "Tile.hpp"

/* constructor & destructor */
// patch::patch(const size_t id_, const arma::Row<double>& move_,
//       const double bWeightZoo_, const double bWeightZootox_,
//       const bool reservoir_, const double res_EIR_, tile* tileP_) :
//       id(id_), move(move_), bWeightZoo(bWeightZoo_), bWeightZootox(bWeightZootox_),
//       reservoir(reservoir_), res_EIR(res_EIR_), tileP(tileP_)
// {
//
//   #ifdef DEBUG_MACRO
//   std::cout << "patch " << id << " born at " << this << std::endl;
//   #endif
//
// };

patch::patch(
  const Rcpp::List& patch_pars,
  tile* tileP_
) :
  id(Rcpp::as<size_t>(patch_pars["id"])),
  move(Rcpp::as<arma::Row<double> >(patch_pars["move"])),
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

  print();

};

patch::~patch(){

  #ifdef DEBUG_MACRO
  std::cout << "patch " << id << " dying at " << this << std::endl;
  #endif

}

/* debug */
void patch::print(){
  std::cout << "patch: " << id << ", with kappa: " << kappa << ", bWeightHuman: " << bWeightHuman << ", reservoir: " << reservoir << ", res_EIR: " << res_EIR << std::endl;
};

/* normalize kappa */
void patch::normalize_kappa(){
  /* no divide by zero errors; also no need to calc kappa for 'reservoir' patches */
  if((bWeightHuman + bWeightZoo + bWeightZootox) > 0. && !reservoir){
    kappa = kappa / (bWeightHuman + bWeightZoo + bWeightZootox);
  } else {
    kappa = 0;
  }
}
