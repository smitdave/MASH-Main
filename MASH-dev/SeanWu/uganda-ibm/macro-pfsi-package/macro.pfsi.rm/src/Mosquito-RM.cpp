/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Simple Ross-MacDonald mosquito model
 *  Requires logging stream 'mosquito'
 *
 *  Sean Wu
 *  November 2018
*/

#include "Mosquito-RM.hpp"

// other object includes
#include "Patch.hpp"
#include "Tile.hpp"

// utility includes
#include "Logger.hpp"


/* ################################################################################
 * class boilerplate
################################################################################ */

/* constructor */
mosquito::mosquito(
  const Rcpp::List& mosquito_pars,
  tile* tileP_
) :
   tileP(tileP_),
   N(Rcpp::as<u_int>(mosquito_pars["N"])),
   lambda(Rcpp::as<arma::Mat<double> >(mosquito_pars["lambda"])),
   psi(Rcpp::as<arma::SpMat<double> >(mosquito_pars["psi"])),
   EIP(Rcpp::as<arma::Col<u_int> >(mosquito_pars["EIP"])),
   maxEIP(Rcpp::as<u_int>(mosquito_pars["maxEIP"])),
   P(maxEIP),
   kappa(N),
   p(Rcpp::as<double>(mosquito_pars["p"])),
   f(Rcpp::as<double>(mosquito_pars["f"])),
   Q(Rcpp::as<double>(mosquito_pars["Q"])),
   a(f*Q),
   v(Rcpp::as<double>(mosquito_pars["v"])),
   M(Rcpp::as<arma::Row<double> >(mosquito_pars["M"])),
   Y(Rcpp::as<arma::Row<double> >(mosquito_pars["Y"])),
   Y0(N),
   Z(Rcpp::as<arma::Row<double> >(mosquito_pars["Z"])),
   ZZ(maxEIP,N),
   ZZ_shift(maxEIP,maxEIP)
{

  #ifdef MACRO_DEBUG
  std::cout << "mosquito born at " << this << std::endl;
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
  for(u_int i=0; i<ZZ.n_cols; i++){
    for(auto it = ZZ.begin_col(i); it != ZZ.end_col(i); it++){
      (*it) = Z.at(i)*(1.0 - p);
    }
  }

  /* zero out Y0 */
  Y0.fill(0.0);

};


/* ################################################################################
 * simulation (implement the interface)
################################################################################ */

/* simulation */
void mosquito::simulate(){

  /* absolute time and day of the year */
  u_int tnow = tileP->get_tnow();
  u_int today = tnow % 365;

  /* simulation */
  aquatic_dynamics(today);
  adult_dynamics(today);

  /* logging */

  /* write M */
  tileP->get_logger()->get_stream("mosquito") << tnow << ",M,";
  for(auto it = M.begin(); it != M.end()-1; it++){
    tileP->get_logger()->get_stream("mosquito") << *it << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << *(M.end()-1) << "\n";

  /* write Y */
  tileP->get_logger()->get_stream("mosquito") << tnow << ",Y,";
  for(auto it = Y.begin(); it != Y.end()-1; it++){
    tileP->get_logger()->get_stream("mosquito") << *it << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << *(Y.end()-1) << "\n";

  /* write Z */
  tileP->get_logger()->get_stream("mosquito") << tnow << ",Z,";
  for(auto it = Z.begin(); it != Z.end()-1; it++){
    tileP->get_logger()->get_stream("mosquito") << *it << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << *(Z.end()-1) << "\n";

};

/* initialize logging */
void mosquito::initialize_logging(){

  u_int tnow = tileP->get_tnow();

  /* write M */
  tileP->get_logger()->get_stream("mosquito") << tnow << ",M,";
  for(auto it = M.begin(); it != M.end()-1; it++){
    tileP->get_logger()->get_stream("mosquito") << *it << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << *(M.end()-1) << "\n";

  /* write Y */
  tileP->get_logger()->get_stream("mosquito") << tnow << ",Y,";
  for(auto it = Y.begin(); it != Y.end()-1; it++){
    tileP->get_logger()->get_stream("mosquito") << *it << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << *(Y.end()-1) << "\n";

  /* write Z */
  tileP->get_logger()->get_stream("mosquito") << tnow << ",Z,";
  for(auto it = Z.begin(); it != Z.end()-1; it++){
    tileP->get_logger()->get_stream("mosquito") << *it << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << *(Z.end()-1) << "\n";

}


/* ################################################################################
 * RM-specific methods
################################################################################ */

/* aquatic dynamics */
void mosquito::aquatic_dynamics(const u_int tnow){

  arma::Row<double> lambda_today = lambda.row(tnow);

  for(u_int i=0; i<M.size(); i++){
    if(lambda_today.at(i) > 0.0){
      M.at(i) += (int)R::rpois(lambda_today.at(i));
    }
  }

};

/* adult dynamics */
void mosquito::adult_dynamics(const u_int tnow){

  u_int EIP_today = EIP.at(tnow);

  /* daily dynamics */
  M = p*M;
  Y = p*Y;
  Z = p*Z;

  /* transmission */
  for(u_int i=0; i<N; i++){
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
