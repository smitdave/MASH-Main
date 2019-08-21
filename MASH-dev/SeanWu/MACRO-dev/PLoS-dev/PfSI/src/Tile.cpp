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

#include "Tile.hpp"

// other object includes
#include "Patch.hpp"
#include "Human-PfSI.hpp"
#include "Mosquito-RM.hpp"

// utility includes
#include "Logger.hpp"
#include "Parameters.hpp"


/* ################################################################################
 * class boilerplate
################################################################################ */

/* constructor */
tile::tile(
     const Rcpp::List& human_pars,
     const Rcpp::List& mosquito_pars,
     const Rcpp::List& patch_pars,
     const Rcpp::List& model_pars,
     const Rcpp::List& log_streams,
     const Rcpp::List& vaxx_events,
     const bool        verbose_
) :

  /* tile's own data members */
  tnow(0),
  verbose(verbose_),

  /* state space classes */
  mosquitos(std::make_unique<mosquito>(mosquito_pars,this)),

  /* construct utility classes */
  loggerPtr(std::make_unique<logger>()),
  parametersPtr(std::make_unique<parameters>(model_pars.size()))

{
  /* initialize parameters */
  if(verbose){std::cout << "begin initializing parameters" << std::endl;}
  parametersPtr->init_params(model_pars);

  /* set up logging */
  if(verbose){std::cout << "begin initializing logging streams" << std::endl;}
  for(size_t i=0; i<log_streams.size(); i++){
    Rcpp::List log = Rcpp::as<Rcpp::List>(log_streams[i]);
    loggerPtr->open(
      Rcpp::as<std::string>(log["outfile"]),
      Rcpp::as<std::string>(log["key"]),
      Rcpp::as<std::string>(log["header"])
    );
  }

  /* construct patches */
  if(verbose){std::cout << "begin constructing patches" << std::endl;}
  patches.reserve(patch_pars.size());
  for(size_t i=0; i<patch_pars.size(); i++){
    Rcpp::List p_par = Rcpp::as<Rcpp::List>(patch_pars[i]);
    patches.emplace_back(std::make_unique<patch>(p_par,this));
  }

  /* construct human population */
  if(verbose){std::cout << "begin constructing human population" << std::endl;}
  humans.reserve(human_pars.size());
  for(size_t i=0; i<human_pars.size(); i++){
    Rcpp::List h_par = Rcpp::as<Rcpp::List>(human_pars[i]);
    humans.emplace_back(std::make_unique<human>(
      Rcpp::as<int>(h_par["id"]),
        Rcpp::as<size_t>(h_par["home_patch_id"]),
        Rcpp::as<std::vector<double> >(h_par["trip_duration"]),
        Rcpp::as<double>(h_par["trip_frequency"]),
        Rcpp::as<double>(h_par["bweight"]),
        this,
        Rcpp::as<std::string>(h_par["state"]),
        Rcpp::as<double>(h_par["age"])
    ));

    // set up the biting algorithm
    double bite_disp = Rcpp::as<double>(h_par["bite_disp"]);
    humans.back()->initialize_biting(Rcpp::as<int>(h_par["bite_algorithm"]),&bite_disp);
  }

  /* initialize vaccinations */
  if(verbose){std::cout << "begin initializing vaccinations" << std::endl;}
  if(vaxx_events.size() > 0){
    for(size_t i=0; i<vaxx_events.size(); i++){

      /* this vaccination 'packet' */
      Rcpp::List vaxx = Rcpp::as<Rcpp::List>(vaxx_events[i]);

      /* get its intended recipient */
      u_int id = Rcpp::as<u_int>(vaxx["id"]);
      auto h = std::find_if(humans.begin(), humans.end(), [id](const std::unique_ptr<human>& hh){
        return hh->get_id() == id;
      });
      h->get()->addVaxx2Q(vaxx);

    }
  }

  /* initialize human movement */
  if(verbose){std::cout << "begin initializing human simulation algorithms" << std::endl;}
  for(auto& h : humans){
    h->initialize_courseofinf();
    h->initialize_movement();
  }
  // need to renormalize kappa after calling h->initialize_courseofinf: it accumulates it in the patches
  for(auto& p : patches){
    p->normalize_kappa();
  }

  #ifdef DEBUG_MACRO
  std::cout << "tile born at " << this << std::endl;
  #endif

}

tile::~tile() = default;


/* ################################################################################
 * accessors
################################################################################ */

patch* tile::get_patch(u_int id){
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


/* ################################################################################
 * simulation algorithm
################################################################################ */

void tile::simulation(const int tmax){

  if(verbose){std::cout << "begin simulation" << std::endl;}
  Progress ps(tmax, verbose);

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

    /* normalize kappa & log output */
    for(auto& p : patches){
      p->normalize_kappa();
      p->log_output();
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

  if(verbose){std::cout << "end simulation" << std::endl;}
}
