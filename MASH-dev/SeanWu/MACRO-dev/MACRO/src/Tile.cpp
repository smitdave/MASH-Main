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
// #include "Debug.hpp"
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
     const Rcpp::List& model_pars,
     const Rcpp::List& log_streams,
     const Rcpp::List& vaxx_events
) :

  /* tile's own data members */
  tnow(0),

  /* state space classes */
  // humans(human_pars.size()),
  mosquitos(mosquito::factory(mosquito_pars,this)),
  // patches(patch_pars.size()),

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

    now the body of the construtor fires up:
    1. we need to init parameters object (humans and patches need their info)

    we dont do anything with the model state pointers yet. those get done in the constructor body
    1. initialize patches (this is easy; no inheritance)
    2. initialize mosquito
    3. initialize humans

    after these are all done, then we can initialize vaccinations (if present)
  */

  /* initialize parameters */
  parametersPtr->init_params(model_pars);
  // std::cout << "test getting a par: "

  /* set up logging */
  std::cout << "begin initializing logging streams" << std::endl;
  for(size_t i=0; i<log_streams.size(); i++){
    Rcpp::List log = Rcpp::as<Rcpp::List>(log_streams[i]);
    loggerPtr->open(
      Rcpp::as<std::string>(log["outfile"]),
      Rcpp::as<std::string>(log["key"]),
      Rcpp::as<std::string>(log["header"])
    );
  }


  /* construct patches */
  // patches.clear();
  // Progress pp(patch_pars.size(), true);
  std::cout << "begin constructing patches" << std::endl;
  for(size_t i=0; i<patch_pars.size(); i++){
    Rcpp::List p_par = Rcpp::as<Rcpp::List>(patch_pars[i]);
    patches.emplace_back(std::make_unique<patch>(p_par,this));
    // pp.increment();
  }

  // for(auto const& p : patches){
  //   p->print();
  // }
  std::cout << "print patch" << std::endl;
  patches.at(0).get()->print();

  /* construct human population */
  // Progress ph(human_pars.size(), true);
  std::cout << "begin constructing human population" << std::endl;
  // std::cout << "human pop size: " << human_pars.size() << std::endl;
  for(size_t i=0; i<human_pars.size(); i++){
    Rcpp::List h_par = Rcpp::as<Rcpp::List>(human_pars[i]);
    humans.emplace_back(human::factory(h_par,this));
    // ph.increment();
    // std::cout << " next human --------- " << std::endl;
  }
  //
  // std::cout << "print the eventQ of all humans" << std::endl;
  // for(auto& h : humans){
  //   h->printEventQ();
  // }

  /* initialize vaccinations */
  std::cout << "begin initializing vaccinations" << std::endl;
  if(vaxx_events.size() > 0){

    // Progress pv(vaxx_events.size(), true);
    for(size_t i=0; i<vaxx_events.size(); i++){

      /* this vaccination 'packet' */
      Rcpp::List vaxx = Rcpp::as<Rcpp::List>(vaxx_events[i]);

      /* get its intended recipient */
      u_int id = Rcpp::as<u_int>(vaxx["id"]);
      auto h = std::find_if(humans.begin(), humans.end(), [id](const std::unique_ptr<human>& hh){
        return hh->get_id() == id;
      });
      h->get()->addVaxx2Q(vaxx);

      // pv.increment();
    }
  }

  /* initialize human movement */
  std::cout << "begin initializing human movement algorithms" << std::endl;
  for(auto& h : humans){
    h->initialize_movement();
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
  // return patches.at(id).get();
  auto h = std::find_if(patches.begin(), patches.end(), [id](const patchP& pp){
    return pp->get_id() == id;
  });
  return h->get();
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

  /* initialize logging */
  mosquitos->initialize_logging();

  std::cout << "begin simulation" << std::endl;
  Progress ps(tmax, true);

  /* main simulation loop */
  while(tnow < tmax){
    // std::cout << "tnow: " << tnow << std::endl;

    /* simulate mosquitos */
    // std::cout << "sim mosquitos" << std::endl;
    mosquitos->simulate();

    /* clear kappa so humans can update their personal contributions when they move */
    // std::cout << "clear kappa" << std::endl;
    for(auto& p : patches){
      p->zero_kappa();
    }

    /* sim humans (they will accumulate kappa in their simulation method) */
    // std::cout << "sim humans" << std::endl;
    for(auto& h : humans){
      h->simulate();
    }

    /* normalize kappa */
    // std::cout << "normalize kappa" << std::endl;
    for(auto& p : patches){
      p->normalize_kappa();
    }

    /* increment time */
    tnow++;
    ps.increment();

    /* check abort */
    if(Progress::check_abort()){
      loggerPtr->close();
      Rcpp::stop("user abort detected: exiting program");
    }
  }

  std::cout << "see everyone's event queue" << std::endl;
  for(auto& h : humans){
    h->printEventQ();
  }

  std::cout << "end simulation" << std::endl;

}
