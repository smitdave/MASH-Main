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
           N(N_), lambda(lambda_), psi(psi_), EIP(EIP_), maxEIP(maxEIP_), P(maxEIP_),
           p(p_), f(f_), Q(Q_), a(f*Q), v(v_),
           M(M_), Y(Y_), Y0(N_), Z(Z_), ZZ(maxEIP_,N_), ZZ_shift(maxEIP_,maxEIP_)
{

  #ifdef MACRO_DEBUG
  std::cout << "mosquito_rm born at " << this << std::endl;
  #endif

  /* compute P */
  unsigned int i = 0;
  std::generate(P.begin(),P.end(),[&](){ return std::pow(p,++i); });

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

//   arma::Row<double> lambda_today = lambda.row(tnow);
// for(size_t i=0; i<M.size(); i++){
//   // M.at(i) += R::rpois(lambda.at(i));
//   M.at(i) += lambda_today.at(i);
// }

};
