/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfSI Event class
 *
 *  Sean Wu
 *  November 2018
 */

#include "Event-PfSI.hpp"
#include "Human-PfSI.hpp"

/* infection event */

/* constructor */
e_pfsi_infect::e_pfsi_infect(double tEvent_, human_pfsi* h):
  event("PfSI_infection",tEvent_,std::bind(&human_pfsi::set_state,h,"I"))
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_infect constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_pfsi_infect::~e_pfsi_infect(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_infect destructor being called at " << this << std::endl;
  #endif

};


/* recovery event */

/* constructor */
e_pfsi_recover::e_pfsi_recover(double tEvent_, human_pfsi* h):
  event("PfSI_recovery",tEvent_,std::bind(&human_pfsi::set_state,h,"S"))
{

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_recover constructor being called at " << this << std::endl;
  #endif

};

/* destructor */
e_pfsi_recover::~e_pfsi_recover(){

  #ifdef DEBUG_MACRO
  std::cout << "e_pfsi_recover destructor being called at " << this << std::endl;
  #endif

};

/* fever event */

// /* constructor */
// e_pfsi_fever::e_pfsi_fever(double tEvent_, double fever_, human_pfsi* h):
//   //    event("PFSI_fever",tEvent_,std::bind(&human_pfsi::set_fever,h,fever_))
//   // event("PFSI_fever",tEvent_,[h,fever_](){
//   //   // do other crap if you want.
//   //   h->set_fever(fever_);
//   // })
// {
//   std::cout << "e_pfsi_fever constructor being called at " << this << std::endl;
// };
//
// /* destructor */
// e_pfsi_fever::~e_pfsi_fever(){
//   std::cout << "e_pfsi_fever destructor being called at " << this << std::endl;
// };
