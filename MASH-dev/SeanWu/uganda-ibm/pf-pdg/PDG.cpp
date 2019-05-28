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
  sex(sex_),
  Pf(pfAges,0),
  Pt(0.), Gt(0.),
  MOI(0),
  Imm(0.), immCounter(0.),
  pFever(0.),
  TE(0.)
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
  int nPf = std::accumulate(Pf.begin(),Pf.end(),0);
  // we want to only move them into subpatency AFTER the intrinsic incubation period & first fortnight of infection
  if( ((MOI - Pf.back() - Pf.front()) > 0) && (nPf  > 0) ){

    // number of patent cohorts to 'terminate'
    int term = R::rbinom((double)MOI-Pf.back(),pfpatency);
    if(term > 0){
      // only do the sampling if we need to)
      if(nPf > 1){

        // sample the cohorts that are being removed
        std::vector<int> subs(pfAges,0);
        rmhyper(subs.data(),Pf.data(),term,pfAges);

        // remove the newly subpatent infections
        for(size_t i=0; i<pfAges; i++){
          Pf[i] -= subs[i];
        }

        // add the subpatent infections to the oldest age group
        Pf.back() += std::accumulate(subs.begin(),subs.end(),0);

      // if we can we'd prefer to avoid sampling the multivariate hypergeom dist
      } else {
        std::fill(Pf.begin(),Pf.end(),0);
        Pf.back() = 1;
      }
    }
  }

  // shift all cohorts to the next age group
  for(size_t i=pfAges-1; i>=0; i--){
    if(i == pfAges-1){
      Pf[i] += Pf[i-1];
    } else if(i == 0){
      Pf[i] = 0;
    } else {
      Pf[i] = Pf[i-1];
    }
  }

};

void PDG_human::update_Pt(){

  if(MOI > 0){

    Pt = 0.;

    // pull from all of the age-specific distributions, sum to get total Pt; limit tails of distn
    for(size_t k=0; k<pfAges; k++){
      if(Pf[k] > 0){
        double sum = 0.;
        for(size_t pf=0; pf<Pf[k]; pf++){
          sum += std::pow(10,(Ptmax[k] - R::rgamma(Ptshape[k], 1./Ptrate[k])));
        }
        Pt = std::log10(std::pow(10,Pt) +  sum);
      }
    }

    // include immune effect (this is just a stub, here we just discount Pt by at most 99 percent)
    Pt = std::log10((1.-.99*Imm) * std::pow(10,Pt));

  } else {
    Pt = NAN;
  }
};

// void PDG_human::update_Gt();
// void PDG_human::update_MOI();
// void PDG_human::update_Imm();
// void PDG_human::update_TE();
// void PDG_human::update_pFever();
// void PDG_human::upate_age(const double dt);
//
// // diagnostics
// bool PDG_human::diagnostic_LM(); /* light microscopy */
