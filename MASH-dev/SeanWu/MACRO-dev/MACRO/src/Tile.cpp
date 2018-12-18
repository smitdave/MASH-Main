/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Tile: the unit of simulation
 *
 *  Sean Wu
 *  November 2018
 */


/* state class includes */
#include "Tile.hpp"
#include "Patch.hpp"
#include "Human.hpp"
#include "Mosquito.hpp"

/* event class includes */
#include "Event.hpp"

/* utilty class incldues */
#include "PRNG.hpp"
#include "Logger.hpp"
#include "Parameters.hpp"

/*
  TO-DO:
  * check that the 'normalize_kappa' in Patch.cpp is right
  * set up factory method for Mosquito
  * set up factory method for Human
*/

/* constructor */
tile::tile(const uint_least32_t seed,
     const Rcpp::List& human_pars,
     const Rcpp::List& mosquito_pars,
     const Rcpp::List& patch_pars
) :

  /* tile's own data members */
  tnow(0),

  /* state space classes */
  humans(human_pars.size()),
  // mosquitos(std::make_unique<>(_Args &&__args...)),
  //
  // Rcpp::as<size_t>(mosquito_pars["N"]),
  // Rcpp::as<arma::Mat<double> >(mosquito_pars["lambda"]),
  // Rcpp::as<arma::Mat<double> >(mosquito_pars["psi"]),
  // Rcpp::as<arma::Col<double> >(mosquito_pars["EIP"]),
  // Rcpp::as<size_t>(mosquito_pars["maxEIP"]),
  // Rcpp::as<double>(mosquito_pars["p"]),
  // Rcpp::as<double>(mosquito_pars["f"]),
  // Rcpp::as<double>(mosquito_pars["Q"]),
  // Rcpp::as<double>(mosquito_pars["v"]),
  // Rcpp::as<arma::Row<double> >(mosquito_pars["M"]),
  // Rcpp::as<arma::Row<double> >(mosquito_pars["Y"]),
  // Rcpp::as<arma::Row<double> >(mosquito_pars["Z"]),
  // this
  patches(patch_pars.size()),

  /* construct utility classes */
  prngPtr(std::make_unique<prng>(seed)),
  loggerPtr(std::make_unique<logger>()),
  parametersPtr(std::make_unique<parameters>(50))

{
  /*
  what needs to happen:
    utility classes are constructed in the member initializer list
    1. PRNG
    2. logger
    3. parameters

    we dont do anything with the model state pointers yet. those get done in the constructor body
    1. initialize patches (this is easy; no inheritance)
    2. initialize mosquito
    3. initialize humans

    after these are all done, then we can initialize vaccinations (if present)
  */

  #ifdef DEBUG_MACRO
  std::cout << "tile born at " << this << std::endl;
  #endif

}

/* destructor */
tile::~tile(){

  #ifdef DEBUG_MACRO
  std::cout << "tile dying at " << this << std::endl;
  #endif

};


/* accessors */
patch* tile::get_patch(size_t id){
  return patches.at(id).get();
};

human* tile::get_human(u_int id){
  auto h = std::find_if(humans.begin(), humans.end(), [id](const humanP& hh){
    return hh->get_id() == id;
  });
  return h->get();
};

mosquito* tile::get_mosquitos(){
  return mosquitos.get();
}


/* simulation */
void tile::simulation(const u_int tmax){

  /* main simulation loop */
  while(tnow < tmax){

    /* simulate mosquitos */
    mosquitos->simulate();

    /* clear kappa so humans can update their personal contributions when they move */
    for(auto& p : patches){
      p->zero_kappa();
    }

    /* sim humans (they will accumulate kappa in their simulation method) */
    for(auto& h : humans){
      h->simulate();
    }

    /* normalize kappa */
    for(auto& p : patches){
      p->normalize_kappa();
    }

    /* increment time */
    tnow++;
  }

}
