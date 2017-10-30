#include <RcppGSL.h>

#include "human.hpp"

using namespace Rcpp;



//'@export
// [[Rcpp::export]]
void testHuman(){
  human* h = new human(1);
  h->get_id();
  delete h;
}