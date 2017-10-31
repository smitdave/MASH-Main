#include <RcppGSL.h>

#include "human.hpp"

using namespace Rcpp;



//'@export
// [[Rcpp::export]]
void testHuman(){
  human* h = new human(1);
  h->get_id();
  h->set_state("state1");
  std::cout << "state: " << h->get_state() << std::endl;
  delete h;
}