///////////////////////////////////////////////////////////////////////////////
//      ____  ______  _______  ____
//     / __ \/ __/  |/  / __ \/  _/
//    / /_/ / /_/ /|_/ / / / // /
//   / ____/ __/ /  / / /_/ // /
//  /_/   /_/ /_/  /_/\____/___/
//
//  MASH
//  PfMOI Pathogen Definition
//  MASH Team
//  December 2017
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MASHCPP_PFMOI_HPP_
#define _MASHCPP_PFMOI_HPP_

#include <Rcpp.h>

#include "MASHcpp/DEBUG.hpp"

namespace MASHcpp {

/*  ///////////////////////////////////////////////////////////////////////////////
 *  Human-stage PfMOI Pathogen
 */ ///////////////////////////////////////////////////////////////////////////////

class humanPfMOI {
// public members
public:

  // constructor
  humanPfMOI(const double &b_init = 0.55, const double &c_init = 0.15, const bool &chemoprophylaxis_init = false);

  // destructor
  ~humanPfMOI();


  // Getters & Setters
  std::vector<int> get_PfID();

  std::vector<double> get_tInf();

  int get_MOI();

  double get_b();
  void set_b(const double &b_new);

  double get_c();
  void set_c(const double &c_new);

  bool get_chemoprophylaxis();
  void set_chemoprophylaxis(const bool &chemoprophylaxis_new);

  // Infection Dynamics
  void add_Infection(const int &PfID_new, const double &tInf_new); // add a new infection
  void clear_Infection(const int &PfID_ix); // completely clear the infection associated with index ix
  void clear_Infections(); // clear all infections
  std::vector<int> get_Infection(); // get all infections where PfID != -1

// private members
private:

  // PfMOI Parameters & State Variables
  std::vector<int> PfID; // pathogen ID
  std::vector<double> tInf; // time of infection (mosquito to human transmission)
  int MOI; // multiplicity of infection
  double b; // transmission efficiency: infected mosquito to human
  double c; // transmission efficiency: infected human to mosquito
  bool chemoprophylaxis;

};


/*  ///////////////////////////////////////////////////////////////////////////////
 *  Mosquito-stage PfMOI Pathogen
 */ ///////////////////////////////////////////////////////////////////////////////

class mosquitoPfMOI {
// public members
public:

  // constructor
  mosquitoPfMOI();

  // destructor
  ~mosquitoPfMOI();

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  std::vector<int> get_PfID();

  int get_MOI();

  // Infection Dynamics
  void add_infection(const int &PfID_new, const double &tInfected_new, const double &tInfectious_new); // add a new infection
  std::vector<int> get_infections(const double &tNow);

// private members
private:

  // PfMOI Parameters & State Variables
  std::vector<int>              PfID;
  std::vector<double>           tInfected;
  std::vector<double>           tInfectious;
  int                           MOI;

};

}

#endif
