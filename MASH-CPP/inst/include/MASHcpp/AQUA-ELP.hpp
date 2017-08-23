///////////////////////////////////////////////////////////////////////////////
//
//      ___   ____  __  _____
//     /   | / __ \/ / / /   |
//    / /| |/ / / / / / / /| |
//   / ___ / /_/ / /_/ / ___ |
//  /_/  |_\___\_\____/_/  |_|
//
//  MASH-CPP
//  AQUATIC ECOLOGY
//  ELP class definition
//  Sean Wu
//  August 21, 2017
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MASHCPP_ELP_HPP_
#define _MASHCPP_ELP_HPP_

#include <Rcpp.h>

namespace MASHcpp {

// ELP: ELP (larval pool) class definition
class ELP {
// public members
public:

  ///////////////////////////////////
  // ELP Constructor
  ///////////////////////////////////

  // constructor defined below
  ELP(const double &alpha_new = 0.1, const double &gamma_new = 0.1, const double &psi_new = 0.01, const double &sigma_new = 1);


  ///////////////////////////////////
  // ELP difference equations
  ///////////////////////////////////

  // oneStep: run one step difference equations for ELP pool
  void oneDay_aquaticDynamics(){

    double L0 = L;
    P = alpha*L0;
    L = E - (alpha + gamma + psi * pow(L0,sigma))*L0;
    E = 0.0;

  };

  // return number of emerging adults
  double oneDay_Emergence(){
    double lambda = P;
    P = 0.0;
    return(lambda)
  };

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  // psi
  double get_psi(){
    return(psi);
  };

  void set_psi(const double &psi_new){
    psi = psi_new;
  };

  // alpha
  double get_alpha(){
    return(alpha);
  };

  void set_alpha(const double &alpha_new){
    alpha = alpha_new;
  };

  // gamma
  double get_gamma(){
    return(gamma);
  };

  void set_gamma(const double &gamma_new){
    gamma = gamma_new;
  };

  // sigma
  double get_sigma(){
    return(sigma);
  };

  void set_sigma(const double &sigma_new){
    sigma = sigma_new;
  };

  // L
  double get_L(){
    return(L);
  };

  void set_L(const double &L_new){
    L = L_new;
  }

  // E
  double get_E(){
    return(E);
  };

  void set_E(const double &E_new){
    E = E_new;
  };

  // P
  double get_P(){
    return(P);
  };

  void set_P(const double &P_new){
    P = P_new;
  };

  ///////////////////////////////////
  // Other Methods
  ///////////////////////////////////

  Rcpp::List get_ELP(){
    return(
      Rcpp::List::create(
        Rcpp::Named("Eggs") = E,
        Rcpp::Named("Larvae") = L,
        Rcpp::Named("Pupae") = P
      )
    );
  };

  void set_ELP(const double &E_new, const double &L_new, const double &P_new){
    E = E_new;
    L = L_new;
    P = P_new;
  };

  Rcpp::List get_parameters(){
    return(
      Rcpp::List::create(
        Rcpp::Named("alpha") = alpha,
        Rcpp::Named("gamma") = gamma,
        Rcpp::Named("psi") = psi,
        Rcpp::Named("sigma") = sigma
      )
    );
  };

  void set_parameters(const double &alpha_new, const double &gamma_new, const double &psi_new, const double &sigma_new){
    alpha = alpha_new;
    gamma = gamma_new;
    psi = psi_new;
    sigma = sigma_new;
  };

  void reset(){
    E = 0.0;
    L = 0.0;
    P = 0.0;
  };

// private members
private:

  // pool state variables
  double E; // number of eggs in the pool
  double L; // number of larvae in the pool
  double P; // number of pupae in the pool

  // pool parameters
  double alpha; // per-day maturation rate of larvae
  double gamma; // density independent mortality
  double psi;   // density dependent mortality
  double sigma; // scaling exponent on density dependent mortality (sigma=1 gives mean crowding)

};

// inline definition of constructor to accept default argument values
inline ELP::ELP(const double &alpha_new, const double &gamma_new, const double &psi_new, const double &sigma_new){

  E = 0.0;
  L = 0.0;
  P = 0.0;

  alpha = alpha_new;
  gamma = gamma_new;
  psi = psi_new;
  sigma = sigma_new;

}

}

#endif
