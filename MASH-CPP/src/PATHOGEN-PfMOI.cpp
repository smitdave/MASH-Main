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
humanPfMOI::humanPfMOI(const double &b_init, const double &c_init, const bool &chemoprophylaxis_init) : MOI(0), b(b_init), c(c_init), chemoprophylaxis(chemoprophylaxis_init) {
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

std::vector<double> humanPfMOI::get_tInf(){
  return(tInf);
};

int humanPfMOI::get_MOI(){
  return(MOI);
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
void humanPfMOI::add_Infection(const int &PfID_new, const double &tInf_new){
  PfID.push_back(PfID_new);
  tInf.push_back(tInf_new);
  MOI += 1;
};

// completely clear the infection associated with index ix
void humanPfMOI::clear_Infection(const int &PfID_ix){
  // find infection associated with this PfID
  auto it = std::find(PfID.begin(), PfID.end(), PfID_ix);
  size_t ix = std::distance(PfID.begin(), it);
  PfID.erase(PfID.begin()+ix);
  tInf.erase(tInf.begin()+ix);
  MOI -= 1;
};

// clear all current infections
void humanPfMOI::clear_Infections(){
  PfID.clear();
  tInf.clear();
  MOI = 0;
};

// get all infections where PfID != -1
std::vector<int> humanPfMOI::get_Infection(){

  // std::vector<int> infIx;
  // auto it = std::find_if(PfID.begin(), PfID.end(), [](const int &PfID_iter){
  //   return(PfID_iter != -1);
  // });
  // while(it != PfID.end()){
  //   infIx.emplace_back(std::distance(PfID.begin(), it));
  //   it = std::find_if(std::next(it), std::end(PfID), [](const int &PfID_iter){
  //     return(PfID_iter != -1);
  //   });
  // }
  //
  // // export these infections that have passed the EIP
  // std::vector<int> PfID_out;
  // std::transform(infIx.begin(), infIx.end(), std::back_inserter(PfID_out), [this](size_t ix){
  //   return(PfID[ix]);
  // });

  std::vector<int> PfID_out;
  std::copy_if(PfID.begin(),PfID.end(),std::back_inserter(PfID_out),
                [](const int& pfid){
                  return pfid!=-1;
                }
             );

  return(PfID_out);
};


/*  ///////////////////////////////////////////////////////////////////////////////
 *  Mosquito-stage PfMOI Pathogen
 */ ///////////////////////////////////////////////////////////////////////////////

// constructor
mosquitoPfMOI::mosquitoPfMOI(const std::string &MosquitoID_init) : MosquitoID(MosquitoID_init) {
  MOI = 0;
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

std::string mosquitoPfMOI::get_MosquitoID(){
  return(MosquitoID);
};

int mosquitoPfMOI::get_MOI(){
  return(MOI);
};

// Infection Dynamics

void mosquitoPfMOI::add_infection(const int &PfID_new, const double &tInfected_new, const double &tInfectious_new){
  PfID.push_back(PfID_new);
  tInfected.push_back(tInfected_new);
  tInfectious.push_back(tInfectious_new);
  MOI += 1;
};

std::vector<int> mosquitoPfMOI::get_infections(const double &tNow){
  std::vector<int> infections;
  for(size_t i=0; i<tInfectious.size(); i++){
    if(tInfectious.at(i) <= tNow){
      infections.push_back(PfID[i]);
    }
  }
  return(infections);
};

// int mosquitoPfMOI::get_Infection_PfID(const int &PfID_ix){
//   auto it = std::find(PfID.begin(), PfID.end(), PfID_ix);
//   size_t ix = std::distance(PfID.begin(), it);
//   return(PfID[ix]);
// };
//
// int mosquitoPfMOI::get_Infection_ix(const int &ix){
//   return PfID[ix];
// };
//
// // get_InfectionEIP: argument 'incubation' = tNow - EIP; only return infections that started at tNow - EIP in the past
// // because only those can possibly have passed the EIP and produced sporozoites.
// std::vector<int> mosquitoPfMOI::get_PfID_EIP(const double &incubation){
//
//   // find infections where tInf < tNow - EIP (incubation)
//   std::vector<int> incubationIx;
//   auto it = std::find_if(tInf.begin(), tInf.end(), [incubation](const double &infectionTime){
//     return(infectionTime < incubation && infectionTime != -1);
//   });
//   while(it != tInf.end()){
//     incubationIx.emplace_back(std::distance(tInf.begin(), it));
//     it = std::find_if(std::next(it), std::end(tInf), [incubation](const double &infectionTime){
//       return(infectionTime < incubation && infectionTime != -1);
//     });
//   }
//
//   // export these infections that have passed the EIP
//   std::vector<int> PfID_out;
//   std::transform(incubationIx.begin(), incubationIx.end(), std::back_inserter(PfID_out), [this](size_t ix){
//     return(PfID[ix]);
//   });
//
//
//   return(PfID_out);
// };

}
