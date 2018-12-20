/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Generic Mosquito Class: mosquitos are specialized by model type
 *
 *  Sean Wu
 *  November 2018
 */
// #include "Debug.hpp"
#include "Mosquito.hpp"
#include "Tile.hpp"

/* define virtual destructor is ok; ensures base class parts get destroyed */
mosquito::~mosquito(){
  #ifdef MACRO_DEBUG
  std::cout << "mosquito dying at " << this << std::endl;
  #endif
};

/* move operators */
mosquito::mosquito(mosquito&&) = default;
mosquito& mosquito::operator=(mosquito&&) = default;


/* ################################################################################
 * derived class factory
################################################################################ */

/*
  * this include is in a weird place;
  * but i think it is more legible if we put the derived classes
  * we need the factory to handle here; if a better solution exists
  * it should be implemented
*/

#include "Mosquito-RM.hpp"

/* factory method */
std::unique_ptr<mosquito> mosquito::factory(const Rcpp::List &mosquito_pars, tile *tileP_){

  /* check what model we need to make */
  std::string model(Rcpp::as<std::string>(mosquito_pars["model"]));

  /* make a derived class */
  if(model.compare("RM") == 0){
    return std::make_unique<mosquito_rm>(mosquito_pars,tileP_);
  } else {
    Rcpp::stop("invalid 'model' field in 'mosquito_pars'\n");
  }
};
