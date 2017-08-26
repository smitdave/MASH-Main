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
  ELP(const double &alpha_new = 0.1, const double &gamma_new = 0.1, const double &psi_new = 0.01, const double &tGrain = 1);


  ///////////////////////////////////
  // ELP difference equations
  ///////////////////////////////////

  // oneStep: run one step difference equations for ELP pool
  void oneDay_aquaticDynamics(const double &eggIn){

    double Ltot = L1+L2+L3+L4;

    double dL1 = eggIn - (alpha*4)*L1 - gamma*L1 - (psi*Ltot)*L1;
    double dL2 = (alpha*4)*L1 - (alpha*4)*L2 - gamma*L2 - (psi*Ltot)*L2;
    double dL3 = (alpha*4)*L2 - (alpha*4)*L3 - gamma*L3 - (psi*Ltot)*L3;
    double dL4 = (alpha*4)*L3 - (alpha*4)*L4 - gamma*L4 - (psi*Ltot)*L4;

    L1 += dL1;
    L2 += dL2;
    L3 += dL3;
    L4 += dL4;

  };

  // return number of emerging adults
  double oneDay_Emergence(){

    return((alpha*4)*L4);

  };

  // ecologicalSimulation: run simulation model for tMax steps; must use after setting initial parameters
  std::vector<Rcpp::List> ecologicalSimulation(const double &g, const double &f, const double &v, const double &L_init, const double &M_init, const int &tMax){

    std::vector<Rcpp::List> out;
    out.reserve(tMax+1);

    double Ltot = 0.0;

    double dL1 = 0.0;
    double dL2 = 0.0;
    double dL3 = 0.0;
    double dL4 = 0.0;
    double dM = 0.0;

    double M = M_init;
    L1 = L_init/4;
    L2 = L_init/4;
    L3 = L_init/4;
    L4 = L_init/4;

    Rcpp::List stateVar = Rcpp::List::create(
            Rcpp::Named("L1") = L1,
            Rcpp::Named("L2") = L2,
            Rcpp::Named("L3") = L3,
            Rcpp::Named("L4") = L4,
            Rcpp::Named("M") = M
          );

    out.push_back(stateVar);

    for(int i = 0; i < tMax; i++){

      Ltot = L1+L2+L3+L4;

      dL1 = f*v*M - (alpha*4)*L1 - gamma*L1 - (psi*Ltot)*L1;
      dL2 = (alpha*4)*L1 - (alpha*4)*L2 - gamma*L2 - (psi*Ltot)*L2;
      dL3 = (alpha*4)*L2 - (alpha*4)*L3 - gamma*L3 - (psi*Ltot)*L3;
      dL4 = (alpha*4)*L3 - (alpha*4)*L4 - gamma*L4 - (psi*Ltot)*L4;
      dM = (alpha*4)*L4 - g*M;

      Rcpp::Rcout << "dL1: " << dL1 << " dL2: " << dL2 << " dL3: " << dL3 << " dL4: " << dL4 << " dM: " << dM << std::endl;

      L1 += dL1;
      L2 += dL2;
      L3 += dL3;
      L4 += dL4;
      M += dM;

      Rcpp::Rcout << "L1: " << L1 << " L2: " << L2 << " L3: " << L3 << " L4: " << L4 << " M: " << M << std::endl;

      Rcpp::List stateVar = Rcpp::List::create(
              Rcpp::Named("L1") = L1,
              Rcpp::Named("L2") = L2,
              Rcpp::Named("L3") = L3,
              Rcpp::Named("L4") = L4,
              Rcpp::Named("M") = M
            );

      // stateVar["L1"] = L1;
      // stateVar["L2"] = L2;
      // stateVar["L3"] = L3;
      // stateVar["L4"] = L4;
      // stateVar["M"] = M;

      out.push_back(stateVar);
    }

    this->reset(); // reset L1-L4 compartments at end of simulation
    return(out);
  };

  Rcpp::List ecologicalSimulation2(const double &g_init, const double &f_init, const double &v_init, const double &L_init, const double &M_init, const int &tMax){

    double g = g_init/tGrain;
    double f = f_init/tGrain;
    double v = v_init;

    Rcpp::Rcout << "g: " << g << " f: " << f << " v: " << v << std::endl;

    std::vector<double> L1hist;
    L1hist.reserve(tMax*tGrain);
    std::vector<double> L2hist;
    L2hist.reserve(tMax*tGrain);
    std::vector<double> L3hist;
    L3hist.reserve(tMax*tGrain);
    std::vector<double> L4hist;
    L4hist.reserve(tMax*tGrain);
    std::vector<double> Mhist;
    Mhist.reserve(tMax*tGrain);

    Mhist.push_back(M_init);
    L1hist.push_back(L_init/4.0);
    L2hist.push_back(L_init/4.0);
    L3hist.push_back(L_init/4.0);
    L4hist.push_back(L_init/4.0);

    Rcpp::Rcout << " Mhist init: " << Mhist.back() << "L init's: " << L1hist.back() << " " << L2hist.back() << " " << L3hist.back() << " " << L4hist.back() << std::endl;

    for(int i = 0; i < tMax; i++){

      double L1now = L1hist.back();
      double L2now = L2hist.back();
      double L3now = L3hist.back();
      double L4now = L4hist.back();
      double Mnow = Mhist.back();

      Rcpp::Rcout << " Mnow: " << Mnow << "L's: " << L1now << " " << L2now << " " << L3now << " " << L4now << std::endl;

      double Ltot = L1now+L2now+L3now+L4now;

      Rcpp::Rcout << "Ltot: " << Ltot << std::endl;

      double dL1 = f*v*Mnow - (alpha*4)*L1now - gamma*L1now - (psi*Ltot)*L1now;
      Rcpp::Rcout << "dL1: " << dL1 << std::endl;
      double dL2 = (alpha*4)*L1now - (alpha*4)*L2now - gamma*L2now - (psi*Ltot)*L2now;
      Rcpp::Rcout << "dL2: " << dL2 << std::endl;
      double dL3 = (alpha*4)*L2now - (alpha*4)*L3now - gamma*L3now - (psi*Ltot)*L3now;
      Rcpp::Rcout << "dL3: " << dL3 << std::endl;
      double dL4 = (alpha*4)*L3now - (alpha*4)*L4now - gamma*L4now - (psi*Ltot)*L4now;
      Rcpp::Rcout << "dL4: " << dL4 << std::endl;
      double dM = (alpha*4)*L4now - g*Mnow;
      Rcpp::Rcout << "dM: " << dM << std::endl;

      L1hist.push_back((L1now+dL1));
      L2hist.push_back((L2now+dL2));
      L3hist.push_back((L3now+dL3));
      L4hist.push_back((L4now+dL4));
      Mhist.push_back((Mnow+dM));

    }

    this->reset(); // reset L1-L4 compartments at end of simulation
    return(
      Rcpp::List::create(
        Rcpp::Named("L1") = L1hist,
        Rcpp::Named("L2") = L2hist,
        Rcpp::Named("L3") = L3hist,
        Rcpp::Named("L4") = L4hist,
        Rcpp::Named("M") = Mhist
      )
    );
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
  double get_tGrain(){
    return(tGrain);
  };


  ///////////////////////////////////
  // Other Methods
  ///////////////////////////////////

  Rcpp::List get_ELP(){
    return(
      Rcpp::List::create(
        Rcpp::Named("L1") = L1,
        Rcpp::Named("L2") = L2,
        Rcpp::Named("L3") = L3,
        Rcpp::Named("L4") = L4
      )
    );
  };

  void set_ELP(const double &L1_new, const double &L2_new, const double &L3_new, const double &L4_new){
    L1 = L1_new;
    L2 = L2_new;
    L3 = L3_new;
    L4 = L4_new;
  };

  Rcpp::List get_parameters(){
    return(
      Rcpp::List::create(
        Rcpp::Named("alpha") = alpha,
        Rcpp::Named("gamma") = gamma,
        Rcpp::Named("psi") = psi,
        Rcpp::Named("tGrain") = tGrain
      )
    );
  };

  void set_parameters(const double &alpha_new, const double &gamma_new, const double &psi_new, const double &tGrain_new){
    alpha = alpha_new/tGrain;
    gamma = gamma_new/tGrain;
    psi = psi_new/tGrain;
    tGrain = tGrain_new;
  };

  void reset(){
    L1 = 0.0;
    L2 = 0.0;
    L3 = 0.0;
    L4 = 0.0;
  };

// private members
private:

  // pool state variables
  double L1; // number of larvae instar 1 in the pool
  double L2; // number of larvae instar 2 in the pool
  double L3; // number of larvae instar 3 in the pool
  double L4; // number of larvae instar 4 in the pool

  // pool parameters
  double alpha; // per-day maturation rate of larvae
  double gamma; // density independent mortality
  double psi;   // density dependent mortality
  double tGrain; // temporal granularity

};

// inline definition of constructor to accept default argument values
inline ELP::ELP(const double &alpha_new, const double &gamma_new, const double &psi_new, const double &tGrain_new){

  L1 = 0.0;
  L2 = 0.0;
  L3 = 0.0;
  L4 = 0.0;

  alpha = alpha_new/tGrain_new;
  gamma = gamma_new/tGrain_new;
  psi = psi_new/tGrain_new;
  tGrain = tGrain_new;

}

}

#endif
