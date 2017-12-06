///////////////////////////////////////////////////////////////////////////////
//      ____  ______  _______  ____
//     / __ \/ __/  |/  / __ \/  _/
//    / /_/ / /_/ /|_/ / / / // /
//   / ____/ __/ /  / / /_/ // /
//  /_/   /_/ /_/  /_/\____/___/
//
//  MASH
//  PfMOI Pathogen Implementation
//  MASH Team
//  December 2017
//
///////////////////////////////////////////////////////////////////////////////

#include <Rcpp.h>

#include "MASHcpp/PATHOGEN-PfMOI.hpp"

namespace MASHcpp {


/*  ///////////////////////////////////////////////////////////////////////////////
 *  Human-stage PfMOI Pathogen
 */ ///////////////////////////////////////////////////////////////////////////////

// constructor
humanPfMOI::humanPfMOI(const int &_PfID, const double &_tInf, const int &_MOI, const double &_b, const double &_c, const bool &_chemoprophylaxis) : MOI(_MOI), b(_b), c(_c), chemoprophylaxis(_chemoprophylaxis) {
    PfID.push_back(_PfID);
    tInf.push_back(_tInf);
    #ifdef DEBUG_MGDRIVE
    std::cout << "humanPfMOI being born at memory location: " << this << std::endl;;
    #endif
};

// destructor
humanPfMOI::~humanPfMOI(){
  #ifdef DEBUG_MGDRIVE
  std::cout << "humanPfMOI being killed at memory location: " << this << std::endl;;
  #endif
};

// Getters & Setters

std::vector<int> humanPfMOI::get_PfID(){
  return(PfID);
};
void humanPfMOI::push_PfID(const int &PfID_new){
  PfID.push_back(PfID_new);
};

std::vector<double> humanPfMOI::get_tInf(){
  return(tInf);
};
void humanPfMOI::push_tInf(const double &tInf_new){
  tInf.push_back(tInf_new);
};

int humanPfMOI::get_MOI(){
  return(MOI);
};
void humanPfMOI::set_MOI(const int &MOI_new){
  MOI = MOI_new;
};

double humanPfMOI::get_b(){
  return(b);
};
void humanPfMOI::set_b(const double &b_new){
  b = b_new;
};

double humanPfMOI::get_c(){
  return(c);
};
void humanPfMOI::set_c(const double &c_new){
  c = c_new;
};

bool humanPfMOI::get_chemoprophylaxis(){
  return(chemoprophylaxis);
};
void humanPfMOI::set_chemoprophylaxis(const bool &chemoprophylaxis_new){
  chemoprophylaxis = chemoprophylaxis_new;
};

// Infection Dynamics

// add a new infection
void humanPfMOI::add_Infection(const int &PfID_new){
  PfID.push_back(PfID_new);
  MOI += 1;
  // eventually can push back this stuff to some history vectors.
};

// completely clear the infection associated with index ix
void humanPfMOI::clear_Infection(const int &PfID_ix){
  // find infection associated with this PfID
  auto it = std::find(PfID.begin(), PfID.end(), PfID_ix);
  size_t ix = std::distance(PfID.begin(), it);
  PfID.erase(PfID.begin()+ix);
  MOI -= 1;
};

// get all infections where PfID != -1
std::vector<int> humanPfMOI::get_Infection(){

  std::vector<int> infIx;
  auto it = std::find_if(PfID.begin(), PfID.end(), [](const int &PfID_iter){
    return(PfID_iter != -1);
  });
  while(it != PfID.end()){
    infIx.emplace_back(std::distance(PfID.begin(), it));
    it = std::find_if(std::next(it), std::end(PfID), [](const int &PfID_iter){
      return(PfID_iter != -1);
    });
  }

  // export these infections that have passed the EIP
  std::vector<int> PfID_out;
  std::transform(infIx.begin(), infIx.end(), std::back_inserter(PfID_out), [this](size_t ix){
    return(PfID[ix]);
  });

  return(PfID_out);
};


/*  ///////////////////////////////////////////////////////////////////////////////
 *  Mosquito-stage PfMOI Pathogen
 */ ///////////////////////////////////////////////////////////////////////////////

// constructor
mosquitoPfMOI::mosquitoPfMOI(const int &_PfID, const std::string &_MosquitoID, const double &_tInf, const int &_MOI) : MosquitoID(_MosquitoID), MOI(_MOI) {
  PfID.push_back(_PfID);
  tInf.push_back(_tInf);
  #ifdef DEBUG_MGDRIVE
  std::cout << "mosquitoPfMOI being born at memory location: " << this << std::endl;;
  #endif
};

// destructor
mosquitoPfMOI::~mosquitoPfMOI(){
  #ifdef DEBUG_MGDRIVE
  std::cout << "mosquitoPfMOI being killed at memory location: " << this << std::endl;;
  #endif
};

// Getters & Setters

std::vector<int> mosquitoPfMOI::get_PfID(){
  return(PfID);
};
void mosquitoPfMOI::push_PfID(const int &PfID_new){
  PfID.push_back(PfID_new);
};

std::string mosquitoPfMOI::get_MosquitoID(){
  return(MosquitoID);
};
void mosquitoPfMOI::set_MosquitoID(const std::string &MosquitoID_new){
  MosquitoID = MosquitoID_new;
};

std::vector<double> mosquitoPfMOI::get_tInf(){
  return(tInf);
};
void mosquitoPfMOI::push_tInf(const double &tInf_new){
  tInf.push_back(tInf_new);
};

int mosquitoPfMOI::get_MOI(){
  return(MOI);
};
void mosquitoPfMOI::set_MOI(const int &MOI_new){
  MOI = MOI_new;
}

std::vector<std::string> mosquitoPfMOI::get_humanInf(){
  return(humanInf);
};
void mosquitoPfMOI::push_humanInf(const std::string &humanInf_new){
  humanInf.push_back(humanInf_new);
};

// Infection Dynamics

void mosquitoPfMOI::add_Infection(const int &PfID_new, const double &tInf_new, const std::string &humanInf_new){
  PfID.push_back(PfID_new);
  tInf.push_back(tInf_new);
  humanInf.push_back(humanInf_new);
  MOI += 1;
};

Rcpp::List mosquitoPfMOI::get_Infection(const int &PfID_ix){
  auto it = std::find(PfID.begin(), PfID.end(), PfID_ix);
  size_t ix = std::distance(PfID.begin(), it);
  return(
    Rcpp::List::create(
      Rcpp::Named("PfID") = PfID[ix],
      Rcpp::Named("humanInf") = humanInf[ix]
    )
  );
};

Rcpp::List mosquitoPfMOI::get_InfectionIx(const int &ix){
  return(
    Rcpp::List::create(
      Rcpp::Named("PfID") = PfID[ix],
      Rcpp::Named("humanInf") = humanInf[ix]
    )
  );
};

// get_InfectionEIP: argument 'incubation' = tNow - EIP; only return infections that started at tNow - EIP in the past
// because only those can possibly have passed the EIP and produced sporozoites.
Rcpp::List mosquitoPfMOI::get_InfectionEIP(const double &incubation){

  // find infections where tInf < tNow - EIP (incubation)
  std::vector<int> incubationIx;
  auto it = std::find_if(tInf.begin(), tInf.end(), [incubation](const double &infectionTime){
    return(infectionTime < incubation && infectionTime != -1);
  });
  while(it != tInf.end()){
    incubationIx.emplace_back(std::distance(tInf.begin(), it));
    it = std::find_if(std::next(it), std::end(tInf), [incubation](const double &infectionTime){
      return(infectionTime < incubation && infectionTime != -1);
    });
  }

  // export these infections that have passed the EIP
  std::vector<int> PfID_out;
  std::vector<std::string> humanInf_out;
  std::transform(incubationIx.begin(), incubationIx.end(), std::back_inserter(PfID_out), [this](size_t ix){
    return(PfID[ix]);
  });
  std::transform(incubationIx.begin(), incubationIx.end(), std::back_inserter(humanInf_out), [this](size_t ix){
    return(humanInf[ix]);
  });

  return(
    Rcpp::List::create(
      Rcpp::Named("PfID") = PfID_out,
      Rcpp::Named("humanInf") = humanInf_out
    )
  );
};

// same as above but only return the indices
std::vector<int> mosquitoPfMOI::which_EIP(const double &incubation) {

  // find infections where tInf < tNow - EIP (incubation)
  std::vector<int> incubationIx;
  auto it = std::find_if(tInf.begin(), tInf.end(), [incubation](const double &infectionTime){
    return(infectionTime < incubation && infectionTime != -1);
  });
  while(it != tInf.end()){
    incubationIx.emplace_back(std::distance(tInf.begin(), it));
    it = std::find_if(std::next(it), std::end(tInf), [incubation](const double &infectionTime){
      return(infectionTime < incubation && infectionTime != -1);
    });
  }

  return(incubationIx);
};

}
