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

#include "Parameters.hpp"


/* ################################################################################
 * class methods
################################################################################ */

/* assign a key-value pair */
void parameters::set_param(const std::string &key, const double val){
  params_map.insert(std::make_pair(key,val));
};

/* get a value by key */
double parameters::get_param(const std::string &key){
  return params_map.at(key);
}

/* initialize parameters with list from R */
void parameters::init_params(const Rcpp::List &pars){

  /* check input */
  if(pars.size() < 1){
    Rcpp::stop("input model parameters cannot be an empty list");
  }

  /* fill hash table */
  for(size_t i=0; i<pars.size(); i++){

    Rcpp::List par = Rcpp::as<Rcpp::List>(pars[i]);

    params_map.insert(std::make_pair(
      Rcpp::as<std::string>(par["name"]),
      Rcpp::as<double>(par["val"])
    ));

  }

}
