#include <RcppGSL.h>

#include "human.hpp"

using namespace Rcpp;



//'@export
// [[Rcpp::export]]
void testHuman(){
  human* h = new human(1);
  std::cout << "my id: " << h->get_id() << std::endl;
  h->set_state("state1");
  std::cout << "state: " << h->get_state() << std::endl;
  h->add2Q_set_state(1.00,"state2");
  h->fireEvent();
  std::cout << "state: " << h->get_state() << std::endl;
  delete h;
}