/* ################################################################################
#
#   Testing PDG (simple standalone c++ conversion)
#   Based on the R code in John Henry's dev folder
#
################################################################################ */

#include "PDG.h"

// utility inclues
#include "RNG.hpp"

/* ################################################################################
#   PDG static members
################################################################################ */

size_t PDG_human::global_ixH = 0;

size_t PDG_human::pfAges = 27;
double PDG_human::pfdr = 0.75;

double PDG_human::gdk = std::log10(0.7);

double PDG_human::pgm = 1.184;
double PDG_human::pgb = -2.004;

double PDG_human::gm = 1.652;
double PDG_human::gb = 1.662;

std::vector<double> PDG_human::Ptmax = {0.,5.210246,4.573131,4.373306,4.218014,4.079337,4.157638,3.805582,3.821811,3.827757,3.467524,3.740757,3.349316,3.732802,3.652580,3.231724,2.567026,2.283804,2.929233,1.962211,2.944931,1.851258,2.193125,2.309303,1.564271,3.205746,2.719204,1.915100};
std::vector<double> PDG_human::Ptshape = {0.,4.639315,2.275418,2.258965,2.311616,2.474827,3.176543,2.943449,3.419812,4.387235,2.755179,5.014499,4.114957,8.849801,6.918817,4.470316,3.726024,4.332549,4.547252,1.829113,5.378781,1.581417,1.094105,1.000000,1.030259,2.641712,2.991721,5.007464};
std::vector<double> PDG_human::Ptrate = {0.,4.008275,1.539217,1.612759,1.756409,1.907544,2.034369,1.949314,2.043045,2.571400,1.800733,2.482635,2.385546,3.573325,3.135033,2.244577,2.825106,3.004125,2.390421,2.198324,2.916877,1.992531,1.051514,1.572928,1.460975,1.294750,2.077740,2.618999};

double PDG_human::pfpatency = 1. - std::exp(-0.1385);
double PDG_human::pgv = 0.2704;

// Immunity
double PDG_human::immHalf = 3.5246;
double PDG_human::immSlope = 3.038;
double PDG_human::immThresh = 0.;
double PDG_human::immP = 0.;

// Health
double PDG_human::feverHalf = 3.5246;
double PDG_human::feverSlope = 3.038;
double PDG_human::feverMax = .8835;

// Infectivity
double PDG_human::TEHalf = 2.3038;
double PDG_human::TESlope = 3.5524;
double PDG_human::TEMax = .4242;

// Diagnostics - Light Microscopy
double PDG_human::LMHalf = 2.;
double PDG_human::LMSlope = 3.;
double PDG_human::LMMax = 0.9;
double PDG_human::LMMin = 0.05;

// special functions
double PDG_human::sigmoid(const double x, const double xhalf, const double b){
  return std::pow((x/xhalf),b) / (std::pow((x/xhalf),b) + 1.);
};

double PDG_human::sigmoidexp(const double x, const double xhalf, const double b){
  return std::exp(x*b)/(std::exp(x*b)+std::exp(xhalf*b));
};


/* ################################################################################
#   PDG implementation
################################################################################ */

// constructor & destructor
PDG_human::PDG_human(const double age_, const bool sex_) :
  ixH(global_ixH),
  age(age_),
  sex(sex_),
  Pf(pfAges,0),
  Pt(NAN), Gt(NAN),
  MOI(0),
  Imm(0.), immCounter(0),
  pFever(0.),
  TE(0.)
{
  global_ixH++;
};

// infection methods
void PDG_human::begin_infection(size_t nInfections){
  Pf[0] += nInfections;
  MOI = std::accumulate(Pf.begin(),Pf.end(),0);
};

void PDG_human::clear_infections(){
  std::fill(Pf.begin(),Pf.end(),0);
  Pt = 0.;
  Gt = 0.;
};

// update functions
void PDG_human::update_PDG(){

  // gametocytes: depend on lagged Pf
  update_Gt();

  // update Pf, Pt, MOI, TE
  age_infections();
  update_Pt();
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
  for(int i=pfAges-1; i>=0; i--){
    if(i == pfAges-1){
      Pf[i] += Pf[i-1];
    } else if(i == 0){
      Pf[i] = 0;
    } else {
      Pf[i] = Pf[i-1];
    }
  }

};

// update parasite densities
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

// update gametocyte densities
void PDG_human::update_Gt(){

  int nPf = std::accumulate(Pf.begin(),Pf.end(),0);
  if(nPf > 0){
    // skip computations if Pt is NaN (if the infection just happened, no merozoites yet)
    if(isnan(Pt)){
      Gt = NAN;

    // use power law to translate from Pt to Gt; add unbiased noise due to uncertainty in P2G fit
    } else {
      Gt = (pgm * Pt) + pgb;
      if(((Gt*gm) + gb) > 0.){
        Gt += R::rnorm(0,std::sqrt(pgv));
      } else {
        Gt = NAN;
      }
    }
  } else {
    Gt += gdk;
  }

};

// update multiplicity of infection
void PDG_human::update_MOI(){
  // add total active infections in each age category
  MOI = std::accumulate(Pf.begin(),Pf.end(),0);
};

void PDG_human::update_Imm(){

  // count up at random rate proportional to Pt, down by geometric if below
  // be sure to ensure nonnegative-definiteness of counters
  if(isnan(Pt) || Pt < immThresh){
    immCounter = std::max(immCounter - 1, 0);
  } else {
    immCounter = std::max(immCounter + (int)R::rgeom(immP),0);
  }

  // sigmoidal conversion of counter to immune effect
  Imm = sigmoid((double)immCounter, immHalf, immSlope);
};

// scaled sigmoid signal; Gametocytes assumed to encode TE
void PDG_human::update_TE(){

  if(isnan(Gt)){
    TE = 0.;
  } else {
    TE = TEMax * sigmoidexp(Gt, TEHalf, TESlope);
  }

};

void PDG_human::update_pFever(){

  if(isnan(Pt)){
    pFever = 0.;
  } else {
    pFever = feverMax * sigmoidexp(Pt, feverHalf, feverSlope);
  }

};

// void PDG_human::upate_age(const double dt);

// diagnostics

/* light microscopy */
bool PDG_human::diagnostic_LM(){
  double p = 0.;
  if(!isnan(Pt) && (Pt > 0.)){
    p = (LMMax-LMMin) * sigmoidexp(Pt,LMHalf,LMSlope) + LMMin;
  }

  bool res = false;
  if(R::runif(0.,1.) < p){
    res = true;
  }

  return res;
};
