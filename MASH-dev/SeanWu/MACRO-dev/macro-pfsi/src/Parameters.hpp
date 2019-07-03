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

#ifndef Parameters_hpp
#define Parameters_hpp

/* Rcpp includes */
#include <RcppArmadillo.h>

/* C++ includes */
#include <iostream>

/* hash-table */
#include <unordered_map>
#include <string> /* for keys */


/* ################################################################################
 * class interface
################################################################################ */

/* class definition */
class parameters {
public:

  /* constructor */
  parameters(const size_t n) : params_map(n) {
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
  void                                         set_param(const std::string& key, const double val);

  /* get a value by key */
  double                                       get_param(const std::string& key);

  /* initialize a parameter set from R side input */
  void                                         init_params(const Rcpp::List& pars);

private:
  std::unordered_map<std::string, double>      params_map;
};




#endif
