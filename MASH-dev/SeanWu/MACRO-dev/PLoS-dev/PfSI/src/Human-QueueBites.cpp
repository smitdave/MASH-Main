/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Queue bites algorithms for MACRO
 *
 *  Sean Wu
 *  August 2019
*/

// header
#include "Human-QueueBites.hpp"

// other project headers
#include "Human-PfSI.hpp"
#include "Tile.hpp"
// #include "PRNG.hpp"


/* ################################################################################
 * base class factory method
################################################################################ */

// return the correct algorithm
std::unique_ptr<queue_bites> queue_bites::factory(const int type, human* const humanP, void* data){
  if(type == 0){
    return std::make_unique<queue_bites_pois>(humanP);
  } else if(type == 1){
    double* disp = static_cast<double*>(data);
    return std::make_unique<queue_bites_nbinom>(humanP,*disp);
  } else {
    return nullptr;
  }
};


/* ################################################################################
 * poisson biting
################################################################################ */

// sample a poisson
int queue_bites_pois::sample_bites(const double eir){
  return R::rpois(eir);
};


/* ################################################################################
 * negative binomial biting
################################################################################ */

// sample a negative binomial
int queue_bites_nbinom::sample_bites(const double eir){
  double p = disp / (disp + eir);
  int k = (eir*p)/(1.-p);
  return R::rnbinom(k,p);
};
