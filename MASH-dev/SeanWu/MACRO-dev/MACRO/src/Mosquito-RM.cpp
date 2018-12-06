/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Simple Ross-MacDonald mosquito model
 *
 *  Sean Wu
 *  November 2018
 */

#include "Mosquito-RM.hpp"
#include "Tile.hpp"
#include "Patch.hpp"
#include "PRNG.hpp"

/* constructor */
mosquito_rm::mosquito_rm(const size_t N_, const arma::Mat<double>& lambda_, const arma::Mat<double>& psi_,
           const arma::Col<size_t>& EIP_, const size_t maxEIP_,
           const double p_, const double f_, const double Q_, const double v_,
           const arma::Row<double>& M_, const arma::Row<double>& Y_, const arma::Row<double>& Z_,
           tile* tileP_) :
           mosquito(tileP_),
           N(N_), lambda(lambda_), psi(psi_), EIP(EIP_), maxEIP(maxEIP_), P(maxEIP_), kappa(N_),
           p(p_), f(f_), Q(Q_), a(f*Q), v(v_),
           M(M_), Y(Y_), Y0(N_), Z(Z_), ZZ(maxEIP_,N_), ZZ_shift(maxEIP_,maxEIP_)
{

  #ifdef MACRO_DEBUG
  std::cout << "mosquito_rm born at " << this << std::endl;
  #endif

  /* compute P */
  unsigned int i = 0;
  std::generate(P.begin(),P.end(),[&](){ return std::pow(p,++i); });

  /* zero out kappa */
  kappa.fill(0.0);

  /* compute ZZ_shift */
  ZZ_shift.fill(0);
  ZZ_shift.submat(0,1,maxEIP-2,maxEIP-1) = arma::eye<arma::Mat<int> >(maxEIP-1,maxEIP-1);

  /* compute ZZ */
  for(size_t i=0; i<ZZ.n_cols; i++){
    for(auto it = ZZ.begin_col(i); it != ZZ.end_col(i); it++){
      (*it) = Z.at(i)*(1.0 - p);
    }
  }

  /* zero out Y0 */
  Y0.fill(0.0);

};

/* destructor */
mosquito_rm::~mosquito_rm(){

  #ifdef MACRO_DEBUG
  std::cout << "mosquito_rm dying at " << this << std::endl;
  #endif

};

/* move operators */
mosquito_rm::mosquito_rm(mosquito_rm&&) = default;
mosquito_rm& mosquito_rm::operator=(mosquito_rm&&) = default;

/* simulation interface */
void mosquito_rm::simulate(){

  u_int today = tileP->get_tnow() % 365;

  aquatic_dynamics(today);
  adult_dynamics(today);

};

double mosquito_rm::get_beta(const size_t p){
  return a * Z.at(p);
}

/* aquatic dynamics */
void mosquito_rm::aquatic_dynamics(const u_int tnow){

  arma::Row<double> lambda_today = lambda.row(tnow);

  for(size_t i=0; i<M.size(); i++){
    M.at(i) += tileP->get_prng()->get_rpois(lambda_today.at(i));
  }

};

/* adult dynamics */
void mosquito_rm::adult_dynamics(const u_int tnow){

  u_int EIP_today = EIP.at(tnow);

  /* daily dynamics */
  M = p*M;
  Y = p*Y;
  Z = p*Z;

  /* transmission */
  for(size_t i=0; i<N; i++){
    kappa.at(i) = tileP->get_patch(i)->get_kappa();
  }
  Y0 = (a * kappa) % (M - Y);
  if(any(Y0 < 0.0)){
   std::replace_if(Y0.begin(), Y0.end(), [](const double y0){
     return y0 < 0.0;
   },0.0);
  }

  /* newly incubating mosquitos */
  Y += Y0;

  /* migration & incubation */
  M = M * psi;
  Y = Y * psi;
  Z = (Z + ZZ.row(0)) * psi;

  /* incubating mosquitos */
  ZZ = ZZ_shift * ZZ;
  ZZ.row(EIP_today-1) += P.at(EIP_today-1) * (Y0 * psi);

};
