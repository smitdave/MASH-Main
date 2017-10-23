////////////////////////////////////////////////////////////
//
//  MASH
//  AQUATIC ECOLOGY
//  EL4P class definition
//  Sean Wu
//  August 7, 2017
//
////////////////////////////////////////////////////////////

#ifndef _MASH_EL4P_HPP_
#define _MASH_EL4P_HPP_

#include <Rcpp.h>

namespace MASH {

// EL4P: struct to store the EL4P object for by genotype
struct EL4Pslot{
  EL4Pslot(const double &eggs_new,
    const double &L1_new,
    const double &L2_new,
    const double &L3_new,
    const double &L4_new,
    const double &P_new,
    const double &lambda_new,
    const int &genotype_new
  );
  double eggs;    // eggs
  double L1;      // larval instar 1
  double L2;      // larval instar 2
  double L3;      // larval instar 3
  double L4;      // larval instar 4
  double P;       // pupae
  double lambda;  // emerging adults
  int genotype;   // genotype
};

// explicit constructor for EL4Pslot struct
inline EL4Pslot::EL4Pslot(const double &eggs_new,
  const double &L1_new,
  const double &L2_new,
  const double &L3_new,
  const double &L4_new,
  const double &P_new,
  const double &lambda_new,
  const int &genotype_new
){
  eggs = eggs_new;
  L1 = L1_new;
  L2 = L2_new;
  L3 = L3_new;
  L4 = L4_new;
  P = P_new;
  lambda = lambda_new;
  genotype = genotype_new;
}

// EL4Pvector: store EL4Pslot structs by genotype
typedef std::vector<EL4Pslot> EL4Pvector;

// EL4P: EL4P class definition
class EL4P {
// public members
public:

  ///////////////////////////////////
  // EL4P Constructor
  ///////////////////////////////////

  // constructor defined below
  EL4P(const int &numGenotypes, const double &psi_new, const double &alpha_new, const double &p_new);


  ///////////////////////////////////
  // EL4P difference equations
  ///////////////////////////////////

  // oneStep: run one step difference equations for EL4P pool
  void oneStep(){

    // total larval density in pool
    double D = 0;
    for(auto it = EL4Pvec.begin(); it != EL4Pvec.end(); it++){
      D += it->L1;
      D += it->L2;
      D += it->L3;
      D += it->L4;
    }

    // aquatic stage mortality
    double s1 = exp(-alpha);
    double s2 = exp(-(alpha + psi*D));

    // run difference equations for each genotype in EL4P pool
    for(int i=0; i<EL4Pvec.size(); i++){

      // initial larval sizes
      double L10 = EL4Pvec[i].L1;
      double L20 = EL4Pvec[i].L2;
      double L30 = EL4Pvec[i].L3;
      double L40 = EL4Pvec[i].L4;

      // difference equations
      EL4Pvec[i].lambda = s1*EL4Pvec[i].P; // P to lambda
      EL4Pvec[i].P = s2*p*L40; // L4 to P
      EL4Pvec[i].L4 = s2*(p*L30 + (1-p)*L40); // L3 to L4 (and L4 who do not advance)
      EL4Pvec[i].L3 = s2*(p*L20 + (1-p)*L30); // L2 to L3 (and L3 who do not advance)
      EL4Pvec[i].L2 = s2*(p*L10 + (1-p)*L20); // L1 to L2 (and L2 who do not advance)
      EL4Pvec[i].L1 = EL4Pvec[i].eggs + s2*(1-p)*L10; // eggs to L1 (and L1 who do not advance)
      EL4Pvec[i].eggs = 0.0;
    }

  };

  ///////////////////////////////////
  // EL4P Fitting
  ///////////////////////////////////

  // run one step of difference equations with equilibrium RM input egg laying
  void oneStep_GEL4P(const double &M, const double &eqAqua, const double &G, const double &lifespan){

    // total larval density in pool
    double D = 0;
    for(auto it = EL4Pvec.begin(); it != EL4Pvec.end(); it++){
      D += it->L1;
      D += it->L2;
      D += it->L3;
      D += it->L4;
    }

    // aquatic stage mortality
    double s1 = exp(-alpha);
    double s2 = exp(-(alpha + psi*D));

    // run difference equations for each genotype in EL4P pool
    for(int i=0; i<EL4Pvec.size(); i++){

      // initial larval sizes
      double L10 = EL4Pvec[i].L1;
      double L20 = EL4Pvec[i].L2;
      double L30 = EL4Pvec[i].L3;
      double L40 = EL4Pvec[i].L4;

      // difference equations
      EL4Pvec[i].lambda = s1*EL4Pvec[i].P; // P to lambda
      EL4Pvec[i].P = s2*p*L40; // L4 to P
      EL4Pvec[i].L4 = s2*(p*L30 + (1-p)*L40); // L3 to L4 (and L4 who do not advance)
      EL4Pvec[i].L3 = s2*(p*L20 + (1-p)*L30); // L2 to L3 (and L3 who do not advance)
      EL4Pvec[i].L2 = s2*(p*L10 + (1-p)*L20); // L1 to L2 (and L2 who do not advance)
      EL4Pvec[i].L1 = EL4Pvec[i].eggs + s2*(1-p)*L10; // eggs to L1 (and L1 who do not advance)
      EL4Pvec[i].eggs = M*eqAqua*(G/lifespan); // eggs are deposited accoridng to RM model at equilibrium
    }

  };

  // run the EL4P pool through a burnin period
  void burnIn_GEL4P(const double &M, const double &eqAqua, const double &G, const double &lifespan, const int &tMax = 800){
    for(int i=0; i<tMax; i++){
      this->oneStep_GEL4P(M,eqAqua,G,lifespan);
    }
  };

  // run the EL4P pool with simulated adult dynamics
  void G2K_GEL4P(const double &eqAqua, const double &G, const double &lifespan, const int &tMax = 800){

    // set adult population equal to emergence
    double M = this->get_totalLambda();
    M += 1;

    // run EL4P pool through tMax
    for(int i=0; i<tMax; i++){
      // run daily time step with RM egg laying
      this->oneStep_GEL4P(M,eqAqua,G,lifespan);

      // overall lambda for all genotypes
      double lambda = this->get_totalLambda();

      // simulate adult population dynamics
      M = ((exp(-1/lifespan))*M) + lambda;
    }

  };

  // run the EL4P pool with simulated adult dynamics and output vector of daily emergence 'lambda'; please initialize the pop before running this function
  std::vector<double> checkDX_GEL4P(const double &eqAqua, const double &G, const double &lifespan, const int &tMax = 800){

    // set adult population equal to emergence
    double lambda = this->get_totalLambda();
    double M = lambda;

    // vector of lambda
    std::vector<double> lambdaH;
    lambdaH.reserve(tMax+1);
    lambdaH.push_back(lambda);

    // simulate adult dynamics through transient period
    for(int i=0; i<100; i++){
      M = ((exp(-1/lifespan))*M) + lambda;
    }

    // run daily simulation with RM adult dynamics
    for(int i=0; i<tMax; i++){
      this->oneStep_GEL4P(M,eqAqua,G,lifespan);
      lambda = this->get_totalLambda();
      M = ((exp(-1/lifespan))*M) + lambda;
      lambdaH.push_back(lambda);
    }

    return(lambdaH);
  };

  ///////////////////////////////////
  // Add Egg Batch
  ///////////////////////////////////

  void addEggs(const double &eggs_N, const int &genotype){

    if(genotype >= EL4Pvec.size()){
      Rcpp::stop("genotype to add not found in EL4P object");
    }

    EL4Pvec[genotype].eggs += eggs_N;
  };

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  // return all genotypes
  Rcpp::List get_allGenotypes(){

    std::vector<Rcpp::List> out;
    out.reserve(EL4Pvec.size());
    for(auto it = EL4Pvec.begin(); it != EL4Pvec.end(); it++){
      out.push_back(
        Rcpp::List::create(
          Rcpp::Named("eggs") = it->eggs,
          Rcpp::Named("L1") = it->L1,
          Rcpp::Named("L2") = it->L2,
          Rcpp::Named("L3") = it->L3,
          Rcpp::Named("L4") = it->L4,
          Rcpp::Named("P") = it->P,
          Rcpp::Named("lambda") = it->lambda,
          Rcpp::Named("genotype") = it->genotype
        )
      );
    }

    return(Rcpp::wrap(out));
  };

  // return a single genotype
  Rcpp::List get_genotypeIx(const int &ix){
    return(
      Rcpp::List::create(
        Rcpp::Named("eggs") = EL4Pvec[ix].eggs,
        Rcpp::Named("L1") = EL4Pvec[ix].L1,
        Rcpp::Named("L2") = EL4Pvec[ix].L2,
        Rcpp::Named("L3") = EL4Pvec[ix].L3,
        Rcpp::Named("L4") = EL4Pvec[ix].L4,
        Rcpp::Named("P") = EL4Pvec[ix].P,
        Rcpp::Named("lambda") = EL4Pvec[ix].lambda,
        Rcpp::Named("genotype") = EL4Pvec[ix].genotype
      )
    );
  };

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

  // p
  double get_p(){
    return(p);
  };

  void set_p(const double &p_new){
    p = p_new;
  };

  // number of genotypes
  int get_numGenotypes(){
    return(EL4Pvec.size());
  };

  // get total lambda across all genotypes
  double get_totalLambda(){
    double lambda = 0.0;
    for(auto it = EL4Pvec.begin(); it != EL4Pvec.end(); it++){
      lambda += it->lambda;
    }
    return(lambda);
  };

  // get genotype specific lambda
  double get_specificLambda(const int &ix){
    if(ix >= EL4Pvec.size()){
      Rcpp::stop("invalid genotype index");
    }
    return(EL4Pvec[ix].lambda);
  };

  ///////////////////////////////////
  // Other Functions
  ///////////////////////////////////

  void reset(){
    for(auto it = EL4Pvec.begin(); it != EL4Pvec.end(); it++){
      it->eggs = 0.0;
      it->L1 = 0.0;
      it->L2 = 0.0;
      it->L3 = 0.0;
      it->L4 = 0.0;
      it->P = 0.0;
      it->lambda = 0.0;
    }
  };

  void set_pop(const Rcpp::List &initPop){

    // make sure sizes of initial pop sizes match
    if(EL4Pvec.size() != initPop.size()){
      Rcpp::stop("size of initPop not equal to number of genotypes");
    }

    // set initial population
    for(int i=0; i<EL4Pvec.size(); i++){
      Rcpp::List currPop = initPop[i];
      EL4Pvec[i].eggs = currPop["eggs"];
      EL4Pvec[i].L1 = currPop["L1"];
      EL4Pvec[i].L2 = currPop["L2"];
      EL4Pvec[i].L3 = currPop["L3"];
      EL4Pvec[i].L4 = currPop["L4"];
      EL4Pvec[i].P = currPop["P"];
      EL4Pvec[i].lambda = currPop["lambda"];
    }
  };

// private members
private:

  EL4Pvector EL4Pvec; // vector of EL4Pslot objects (one for each genotype)
  double alpha; // density-dependent mortality independent of carrying capacity
  double psi; // density-dependent mortality dependent on carrying capacity
  double p; // expected fraction of cohort that advances to next life stage (1/p is expected time spent in stages L1,L2,L3,L4,P)

};

// inline definition of constructor to accept default argument values
inline EL4P::EL4P(const int &numGenotypes, const double &psi_new, const double &alpha_new, const double &p_new){

  EL4Pvec.reserve(numGenotypes);
  for(int i=0; i<numGenotypes; i++){
    EL4Pvec.push_back(EL4Pslot(0,0,0,0,0,0,0,i));
  }

  psi = psi_new;
  alpha = alpha_new;
  p = p_new;

}

}

#endif
