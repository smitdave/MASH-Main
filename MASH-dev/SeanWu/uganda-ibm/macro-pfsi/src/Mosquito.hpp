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

#ifndef Mosquito_hpp
#define Mosquito_hpp

/* Rcpp includes */
#include <RcppArmadillo.h>

/* standard includes */
#include <stdio.h>
#include <iostream>

/* to return a base-class pointer from the factory */
#include <memory>

/* forward declarations */
class tile;


/* abstract base mosquito */
class mosquito {
public:

  /* constructor & destructor */
  mosquito(tile* tileP_) : tileP(tileP_) {

    #ifdef DEBUG_MACRO
    std::cout << "mosquito born at " << this << std::endl;
    #endif

  };
  virtual ~mosquito() = 0;

  /* move operators */
  mosquito(mosquito&&);
  mosquito& operator=(mosquito&&);

  /* copy operators */
  mosquito(mosquito&) = delete;
  mosquito& operator=(mosquito&) = delete;

  /* factory method */
  static std::unique_ptr<mosquito> factory(const Rcpp::List& mosquito_pars, tile* tileP_);

  /* interface */
  virtual void simulate() = 0;
  virtual double get_beta(const size_t p) = 0; /* beta: number of infectious bites mosquitos produce today in patch p; in RM models it is a*Z */
  virtual void initialize_logging() = 0;

protected:

  tile*                 tileP;

};


#endif
