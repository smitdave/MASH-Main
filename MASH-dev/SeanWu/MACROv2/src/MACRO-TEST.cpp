/* Unit Tests */

#include <memory>
#include <Rcpp.h>

#include "Tile.hpp"
#include "Human.hpp"
#include "Human-PfSI.hpp"

//' @export
// [[Rcpp::export]]
void test_human(){

  std::unique_ptr<tile> tileP = std::make_unique<tile>();

  std::unique_ptr<human> humanP = std::make_unique<human_pfsi>(1,0.0,tileP.get());

};
