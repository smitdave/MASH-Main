/* Unit Tests */

#include <memory>
#include <Rcpp.h>

#include "Tile.hpp"
#include "Human.hpp"
#include "Human-PfSI.hpp"
#include "MACRO-PRNG.hpp"

//' @export
// [[Rcpp::export]]
void test_human(){

  std::unique_ptr<tile> tileP = std::make_unique<tile>();

  std::unique_ptr<human> humanP = std::make_unique<human_pfsi>(1,0.0,tileP.get());
  humanP->simHuman();
};


//' @export
// [[Rcpp::export]]
void test_prng(const uint_least32_t& seed_){

  prng::instance()->set_seed(seed_);

  /* sample some random variates */
  std::cout << "testing sampling prng ..." << std::endl;
  std::cout << "runif: " << prng::instance()->get_runif() << std::endl;
  std::cout << "rbinom: " << prng::instance()->get_rbinom(10, 0.5) << std::endl;
  std::cout << "rexp: " << prng::instance()->get_rexp(1.0/100.0) << std::endl;
  std::cout << "rlnorm: " << prng::instance()->get_rlnorm(0, 1.0) << std::endl;
  std::cout << "rnorm: " << prng::instance()->get_rnorm(0, 1.0) << std::endl;

  /* kill instance */
  prng::instance()->suicide();
}
