/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfSI Human Class
 *
 *  Sean Wu
 *  November 2018
 */

#include "Human-PfSI.hpp"
#include "Event.hpp"
#include "Tile.hpp"
// #include "Event-PfSI.hpp"

human_pfsi::human_pfsi(const int id_, tile* tileP_, const double age_) :
  human(id_,tileP_), state("S"), age(age_)
{
  #ifdef DEBUG_MACRO
  std::cout << "human_pfsi " << " born at " << this << std::endl;
  #endif
};

human_pfsi::~human_pfsi(){
  #ifdef DEBUG_MACRO
  std::cout << "human_pfsi " << " dying at " << this << std::endl;
  #endif
};

/* move operators */
human_pfsi::human_pfsi(human_pfsi&&) = default;
human_pfsi& human_pfsi::operator=(human_pfsi&&) = default;

/* simulation */
void human_pfsi::simulate(){

  /* fire all events that occur on this time step */
  while(tnow < tileP->get_tnow()){
    fireEvent();
  }

  /* update kappa (my infectiousness to mosquitos) */
  update_kappa();

  /* update my EIR */
  update_EIR();

  /* queue bites */
  queue_bites();
};
