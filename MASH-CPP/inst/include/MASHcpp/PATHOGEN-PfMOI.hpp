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
  humanPfMOI(const double &_b = 0.55, const double &_c = 0.15, const bool &_chemoprophylaxis = false);

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
  void push_PfID(const int &PfID_new);

  std::string get_MosquitoID();
  void set_MosquitoID(const std::string &MosquitoID_new);

  std::vector<double> get_tInf();
  void push_tInf(const double &tInf_new);

  int get_MOI();
  void set_MOI(const int &MOI_new);

  std::vector<std::string> get_humanInf();
  void push_humanInf(const std::string &humanInf_new);

  // Infection Dynamics

  void add_Infection(const int &PfID_new, const double &tInf_new, const std::string &humanInf_new); // add a new infection

  int get_Infection_PfID(const int &PfID_ix); // return the clonal variant associated with given PfID

  int get_Infection_ix(const int &ix); // get clonal variants just depending on their position in vector

  // get_InfectionEIP: argument 'incubation' = tNow - EIP; only return infections that started at tNow - EIP in the past
  // because only those can possibly have passed the EIP and produced sporozoites.
  std::vector<int> get_PfID_EIP(const double &incubation);

// private members
private:

  // PfMOI Parameters & State Variables
  std::vector<int>              PfID;
  std::string                   MosquitoID;
  std::vector<double>           tInf;
  int                           MOI;
  std::vector<std::string>      humanInf;

};

}

#endif
