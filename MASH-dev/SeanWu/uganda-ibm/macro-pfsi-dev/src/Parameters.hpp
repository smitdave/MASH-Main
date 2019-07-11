/*
*      __  ______   __________  ____
*     /  |/  /   | / ____/ __ \/ __ \
*    / /|_/ / /| |/ /   / /_/ / / / /
*   / /  / / ___ / /___/ _, _/ /_/ /
*  /_/  /_/_/  |_\____/_/ |_|\____/
*
*  Generic Parameters Class
*
*  Sean Wu
*  November 2018
*/

#ifndef PARAMETERS_HPP
#define PARAMETERS_HPP

/* C++ includes */
#include <iostream>

/* hash-table */
#include <unordered_map>
#include <string> /* for keys */

/* Rcpp includes */
#include <RcppArmadillo.h>


/* ################################################################################
 * class interface
################################################################################ */

/* class definition */
class parameters {
public:

  /* constructor */
  parameters(const u_int n) : pars_map(n) {
      #ifdef DEBUG_MACRO
      std::cout << "parameters born at " << this << std::endl;
      #endif
  };

  /* destructor */
  ~parameters(){
      #ifdef DEBUG_MACRO
      std::cout << "parameters dying at " << this << std::endl;
      #endif
  };

  /* delete copy constructor/assignment operator, default move constructor/assignment operator */
  parameters(const parameters&) = delete;
  parameters& operator=(const parameters&) = delete;
  parameters(parameters&&) = default;
  parameters& operator=(parameters&&) = default;

  /* assign a key-value pair */
  void set_param(const std::string& key, const double val){
    pars_map.insert(std::make_pair(key,val));
  };

  /* get a value by key */
  double get_param(const std::string& key){
    return pars_map.at(key);
  };

  /* initialize a parameter set from R side input */
  void init_params(const Rcpp::List& pars){

    /* check input */
    if(pars.size() < 1){
      Rcpp::stop("input model parameters cannot be an empty list");
    }

    /* fill hash table */
    for(u_int i=0; i<pars.size(); i++){

      Rcpp::List par = Rcpp::as<Rcpp::List>(pars[i]);
      pars_map.insert(std::make_pair(
        Rcpp::as<std::string>(par["name"]),
        Rcpp::as<double>(par["val"])
      ));

    }

  };

private:
  std::unordered_map<std::string, double>      pars_map;
};

#endif
