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
#include <stdio.h>
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

class human;
using humanP = std::unique_ptr<human>;

class mosquito;
using mosquitoP = std::unique_ptr<mosquito>;

class patch;
using patchP = std::unique_ptr<patch>;

class prng;
using prngP = std::unique_ptr<prng>;

class logger;
using loggerP = std::unique_ptr<logger>;

class parameters;
using parametersP = std::unique_ptr<parameters>;


/* ################################################################################
 * class declaration
################################################################################ */

/* tile class definition */
class tile {
public:

  tile(const uint_least32_t seed,
       const Rcpp::List& human_pars,
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
  u_int                         get_tnow(){return tnow;}
  void                          set_tnow(u_int t){ tnow = t; }

  /* state classes */
  patch*                        get_patch(size_t id);
  human*                        get_human(u_int id);
  mosquito*                     get_mosquitos();

  /* utility classes */
  prng*                         get_prng(){return prngPtr.get();}
  logger*                       get_logger(){return loggerPtr.get();}
  parameters*                   get_params(){return parametersPtr.get();}

  /* simulation */
  void                          simulation(const u_int tmax);

private:

  u_int                         tnow;
  bool                          verbose;

  /* state space (agents & environment) */
  std::vector<humanP>           humans;
  mosquitoP                     mosquitos;
  std::vector<patchP>           patches;

  /* utility classes */
  prngP                         prngPtr;
  loggerP                       loggerPtr;
  parametersP                   parametersPtr;

};


#endif
