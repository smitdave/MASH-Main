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
  ~parameters() = default;

  /* move operators */
  parameters(parameters&&) = default;
  parameters& operator=(parameters&&) = default;

  /* copy operators */
  parameters(const parameters&) = delete;
  parameters& operator=(const parameters&) = delete;

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

    if(pars.size() < 1){
      Rcpp::stop("input model parameters cannot be an empty list");
    }

    Rcpp::CharacterVector par_names = pars.names();

    /* fill hash table */
    for(u_int i=0; i<pars.size(); i++){

      // std::string parname(Rcpp::as<std::string>(par_names[i]));
      // double parvalue = Rcpp::as<double>(pars[i]);
      // std::cout << "making i: " << i << "th parameter, name: " << parname << " value: " << parvalue << "\n";

      pars_map.insert(std::make_pair(
        Rcpp::as<std::string>(par_names[i]),
        Rcpp::as<double>(pars[i])
      ));

    }

  };

private:
  std::unordered_map<std::string, double>      pars_map;
};

#endif
