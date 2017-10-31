#include <RcppGSL.h>

#include "human.hpp"

using namespace Rcpp;



//'@export
// [[Rcpp::export]]
void testHuman(){
  human* h = new human(1);
  std::cout << "human 1 id: " << h->get_id() << std::endl;
  h->set_state("state1");
  std::cout << "human 1 state: " << h->get_state() << std::endl;
  h->add2Q_set_state(1.00,"state2");
  h->fireEvent();
  std::cout << "human 1 state: " << h->get_state() << std::endl;
  
  human* h1 = new human(2);
  std::cout << "human 2 id: " << h1->get_id() << std::endl;
  h1->set_state("state53");
  std::cout << "human 2 state: " << h1->get_state() << std::endl;
  h1->add2Q_set_state(1.00,"state223");
  h1->fireEvent();
  std::cout << "human 2 state: " << h1->get_state() << std::endl;
  std::cout << "human 1 state: " << h->get_state() << std::endl;
  
  delete h;
  delete h1;
}