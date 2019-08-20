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

#ifndef TILE_HPP
#define TILE_HPP

/* standard includes */
#include <iostream>

/* data */
#include <vector>

/* for smart pointers */
#include <memory>

/* finding stuff */
#include <algorithm>

/* Rcpp */
#include <RcppArmadillo.h>

/* RcppProgress */
#include "progress.hpp"


/* ################################################################################
 * forward declarations & alias/typedefs
################################################################################ */

/* forward declarations */
class human;
using humanP = std::unique_ptr<human>;

class mosquito;
using mosquitoP = std::unique_ptr<mosquito>;

class patch;
using patchP = std::unique_ptr<patch>;

class logger;
using loggerP = std::unique_ptr<logger>;

class parameters;
using parametersP = std::unique_ptr<parameters>;


/* ################################################################################
 * class declaration
################################################################################ */

/* tile class */
class tile {
public:

  tile(const Rcpp::List& human_pars,
       const Rcpp::List& mosquito_pars,
       const Rcpp::List& patch_pars,
       const Rcpp::List& model_pars,
       const Rcpp::List& log_streams,
       const Rcpp::List& vaxx_events,
       const bool        verbose_
  );
  ~tile();

  /* move operators */
  tile(tile&&) = default;
  tile& operator=(tile&&) = default;

  /* copy operators */
  tile(tile&) = delete;
  tile& operator=(tile&) = delete;

  /* accessors */
  int                           get_tnow(){return tnow;};
  void                          set_tnow(int t){ tnow = t; };

  /* state classes */
  patch*                        get_patch(u_int id);
  human*                        get_human(u_int id);
  mosquito*                     get_mosquitos();

  /* utility classes */
  logger*                       get_logger(){return loggerPtr.get();};
  parameters*                   get_params(){return parametersPtr.get();};

  /* simulation */
  void                          simulation(const int tmax);

private:

  int                           tnow;
  bool                          verbose;

  /* state space (agents & environment) */
  std::vector<humanP>           humans;
  mosquitoP                     mosquitos;
  std::vector<patchP>           patches;

  /* utility classes */
  loggerP                       loggerPtr;
  parametersP                   parametersPtr;

};


#endif
