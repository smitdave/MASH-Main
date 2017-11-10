/*
 * ################################################################################
 *
 *        ____
 *       /  _/___ ___  ____ ___  __  ______  ___
 *       / // __ `__ \/ __ `__ \/ / / / __ \/ _ \
 *     _/ // / / / / / / / / / / /_/ / / / /  __/
 *    /___/_/ /_/ /_/_/ /_/ /_/\__,_/_/ /_/\___/
 *
 *    Immune Class Implementation
 *    MASH Team
 *    October 2017
 *
 * ################################################################################
*/

#include "immune.hpp"
#include "human.hpp"

/*
 * ################################################################################
 *    Immune: Abstract Base Class
 * ################################################################################
*/

immune_base::immune_base(const std::string &_immune_model) :  immune_model(_immune_model) {
  #ifdef DEBUG_INFSIM
  std::cout << "immune_base " << " being born at memory location: " << this << std::endl;;
  #endif
};

immune_base::~immune_base(){
  #ifdef DEBUG_INFSIM
  std::cout << "immune_base " << " being killed at memory location: " << this << std::endl;;
  #endif
};

std::string immune_base::get_immune_model(){
  return immune_model;
};

human* immune_base::get_my_human(){
  return my_human;
};

// class methods
void immune_base::set_infected(const bool &i){
  infected = i;
};

bool immune_base::get_infected(){
  return infected;
};

// factory method
immune_base* immune_base::make_immune(std::string model){
  if(model.compare("PfSI")==0){
    return new immune_PfSI("PfSI",false);
  } else {
    std::cout << "not a valid immune model" << std::endl;
    exit;
  }
};


/*
 * ################################################################################
 *    Immune PfSI
 * ################################################################################
*/

// constructor
immune_PfSI::immune_PfSI(const std::string &_immune_model, const bool &_infected) : immune_base(_immune_model) {
  infected = _infected;
  #ifdef DEBUG_INFSIM
  std::cout << "immune_PfSI " << " being born at memory location: " << this << std::endl;;
  #endif
};

// destructor
immune_PfSI::~immune_PfSI(){
  #ifdef DEBUG_INFSIM
  std::cout << "immune_PfSI " << " being killed at memory location: " << this << std::endl;;
  #endif
};

// void immune_PfSI::pfsi_dynamics(){
// };