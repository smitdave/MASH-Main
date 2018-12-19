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

class parameters {
public:

  /* constructor */
  parameters(const size_t n) : paramsI(n), paramsD(n) {
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
  template <typename T>
  void                                         set_param(const std::string& key, const T val);

  /* get a value by key */
  template <typename T>
  T                                            get_param(const std::string& key);

  /* initialize a parameter set from R side input */
  void                                         init_params(const Rcpp::List& pars);

private:
  std::unordered_map<std::string, int>         paramsI;
  std::unordered_map<std::string, double>      paramsD;
};


/* ################################################################################
 * class methods
################################################################################ */

/* assign a key-value pair */
template <>
inline void parameters::set_param<int>(const std::string &key, const int val){
  paramsI.insert(std::make_pair(key,val));
};

template <>
inline void parameters::set_param<double>(const std::string &key, const double val){
  paramsD.insert(std::make_pair(key,val));
};

/* get a value by key */
template <>
inline double parameters::get_param<double>(const std::string &key){
  return paramsD.at(key);
}

template <>
inline int parameters::get_param<int>(const std::string &key){
  return paramsI.at(key);
}

/* initialize parameters with list from R */
inline void parameters::init_params(const Rcpp::List &pars){

  /* check input */
  if(pars.size() < 1){
    Rcpp::stop("input model parameters cannot be an empty list");
  }

  /* fill hash table */
  for(size_t i=0; i<pars.size(); i++){

    Rcpp::List par = Rcpp::as<Rcpp::List>(pars[i]);
    size_t type = Rcpp::as<size_t>(par["type"]);

    /* integer */
    if(type == 0){

      paramsI.insert(std::make_pair(
        Rcpp::as<std::string>(par["name"]),
        Rcpp::as<int>(par["val"])
      ));

    /* double */
    } else if(type == 1){

      paramsD.insert(std::make_pair(
        Rcpp::as<std::string>(par["name"]),
        Rcpp::as<double>(par["val"])
      ));

    } else {
      Rcpp::stop("unrecognized 'type' specifier in parameter input");
    }

  }

}


#endif
