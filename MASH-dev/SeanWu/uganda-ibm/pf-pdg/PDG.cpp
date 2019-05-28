/* ################################################################################
#
#   Testing PDG (simple standalone c++ conversion)
#   Based on the R code in John Henry's dev folder
#
################################################################################ */

#include "PDG.h"

// constructor & destructor
PDG_human::PDG_human(const double age_, const bool sex_) :
  ixH(global_ixH),
  age(age_),
  sex(sex_)
{
  global_ixH++;
};

PDG_human::~PDG_human(){};

// infection methods
void PDG_human::begin_infection(size_t nInfections){
  Pf[0] += nInfections;
  MOI += 1;
};

void PDG_human::clear_infections(){
  std::fill(Pf.begin(),Pf.end(),0);
  Pt = 0.;
  Gt = 0.;
};

// update functions
void PDG_human::update_PDG(){

  age_infections();
  update_Pt();
  update_Gt();
  update_MOI();
  update_TE();
  update_pFever();

  // update_age(dt)
};

void PDG_human::age_infections(){

  // attrition of infections at final age category
  if(Pf.back() > 0){
    Pf.back() -= (int)R::rbinom((double)Pf.back(), pfdr);
  }

  // some proportion of patent infections move into subpatent phase; each independent
  if( ((MOI - Pf.back() - Pf.front()) > 0) && (std::accumulate(Pf.begin(),Pf.end(),0) > 0) ){ // we want to only move them into subpatency AFTER the intrinsic incubation period & first fortnight of infection

  }

};

// void PDG_human::update_Pt();
// void PDG_human::update_Gt();
// void PDG_human::update_MOI();
// void PDG_human::update_Imm();
// void PDG_human::update_TE();
// void PDG_human::update_pFever();
// void PDG_human::upate_age(const double dt);
//
// // diagnostics
// bool PDG_human::diagnostic_LM(); /* light microscopy */
