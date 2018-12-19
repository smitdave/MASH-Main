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
     const Rcpp::List& patch_pars,
     const Rcpp::List& log_streams,
     const Rcpp::List& vaxx_events
) :

  /* tile's own data members */
  tnow(0),

  /* state space classes */
  humans(human_pars.size()),
  mosquitos(mosquito::factory(mosquito_pars,this)),
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

  /* construct patches */
  Progress pp(patch_pars.size(), true);
  std::cout << "begin constructing patches" << std::endl;
  for(size_t i=0; i<patch_pars.size(); i++){
    Rcpp::List p_par = Rcpp::as<Rcpp::List>(patch_pars[i]);
    patches.emplace_back(std::make_unique<patch>(p_par,this));
    pp.increment();
  }

  /* construct human population */
  Progress ph(human_pars.size(), true);
  std::cout << "begin constructing human population" << std::endl;
  for(size_t i=0; i<human_pars.size(); i++){
    Rcpp::List h_par = Rcpp::as<Rcpp::List>(human_pars[i]);
    humans.emplace_back(human::factory(h_par,this));
    ph.increment();
  }

  /* set up logging */
  std::cout << "begin initializing logging streams" << std::endl;
  for(size_t i=0; i<log_streams.size(); i++){
    Rcpp::List log = Rcpp::as<Rcpp::List>(log_streams[i]);
    loggerPtr->open(
      Rcpp::as<std::string>(log["outfile"]),
      Rcpp::as<std::string>(log["key"])
    );
  }

  /* initialize vaccinations */
  std::cout << "begin initializing vaccinations" << std::endl;
  if(vaxx_events.size() > 0){

    Progress pv(vaxx_events.size(), true);
    for(size_t i=0; i<vaxx_events.size(); i++){

      /* this vaccination 'packet' */
      Rcpp::List vaxx = Rcpp::as<Rcpp::List>(vaxx_events[i]);

      /* get its intended recipient */
      u_int id = Rcpp::as<u_int>(vaxx["id"]);
      auto h = std::find_if(humans.begin(), humans.end(), [id](const std::unique_ptr<human>& hh){
        return hh->get_id() == id;
      });
      h->get()->addVaxx2Q(vaxx);

      pv.increment();
    }
  }

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
