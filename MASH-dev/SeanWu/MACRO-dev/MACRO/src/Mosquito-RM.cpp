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
// #include "Debug.hpp"
/* movement model */
#include "Mosquito-RM.hpp"

/* model includes */
#include "Tile.hpp"
#include "Patch.hpp"

/* utility class includes */
#include "PRNG.hpp"
#include "Logger.hpp"


/* ################################################################################
 * construtor & destructor
################################################################################ */

// mosquito_rm::mosquito_rm(const size_t N_, const arma::Mat<double>& lambda_, const arma::Mat<double>& psi_,
//            const arma::Col<size_t>& EIP_, const size_t maxEIP_,
//            const double p_, const double f_, const double Q_, const double v_,
//            const arma::Row<double>& M_, const arma::Row<double>& Y_, const arma::Row<double>& Z_,
//            tile* tileP_) :
//            mosquito(tileP_),
//            N(N_), lambda(lambda_), psi(psi_), EIP(EIP_), maxEIP(maxEIP_), P(maxEIP_), kappa(N_),
//            p(p_), f(f_), Q(Q_), a(f*Q), v(v_),
//            M(M_), Y(Y_), Y0(N_), Z(Z_), ZZ(maxEIP_,N_), ZZ_shift(maxEIP_,maxEIP_), patches(N_)
// {
//
//   #ifdef MACRO_DEBUG
//   std::cout << "mosquito_rm born at " << this << std::endl;
//   #endif
//
//   /* compute P */
//   unsigned int i = 0;
//   std::generate(P.begin(),P.end(),[&](){ return std::pow(p,++i); });
//
//   /* zero out kappa */
//   kappa.fill(0.0);
//
//   /* compute ZZ_shift */
//   ZZ_shift.fill(0);
//   ZZ_shift.submat(0,1,maxEIP-2,maxEIP-1) = arma::eye<arma::Mat<int> >(maxEIP-1,maxEIP-1);
//
//   /* compute ZZ */
//   for(size_t i=0; i<ZZ.n_cols; i++){
//     for(auto it = ZZ.begin_col(i); it != ZZ.end_col(i); it++){
//       (*it) = Z.at(i)*(1.0 - p);
//     }
//   }
//
//   /* zero out Y0 */
//   Y0.fill(0.0);
//
//   /* integer sequence of patches (just for writing output) */
//   std::generate(patches.begin(), patches.end(), [n = 1] () mutable { return n++; });
//
// };

mosquito_rm::mosquito_rm(
  const Rcpp::List& mosquito_pars,
  tile* tileP_
) :
   mosquito(tileP_),
   N(Rcpp::as<size_t>(mosquito_pars["N"])),
   lambda(Rcpp::as<arma::Mat<double> >(mosquito_pars["lambda"])),
   psi(Rcpp::as<arma::Mat<double> >(mosquito_pars["psi"])),
   EIP(Rcpp::as<arma::Col<size_t> >(mosquito_pars["EIP"])),
   maxEIP(Rcpp::as<size_t>(mosquito_pars["maxEIP"])),
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
   ZZ_shift(maxEIP,maxEIP),
   patches(N)
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

  /* integer sequence of patches (just for writing output) */
  std::generate(patches.begin(), patches.end(), [n = 1] () mutable { return n++; });

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


/* ################################################################################
 * interface
################################################################################ */

/* simulation */
void mosquito_rm::simulate(){

  /* absolute time and day of the year */
  u_int t_abs = tileP->get_tnow();
  u_int today = t_abs % 365;

  /* simulation */
  aquatic_dynamics(today);
  adult_dynamics(today);

  /* logging */

  /* write M */
  tileP->get_logger()->get_stream("mosquito") << today << ",M,";
  for(auto it = M.begin(); it != M.end()-1; it++){
    tileP->get_logger()->get_stream("mosquito") << *it << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << *(M.end()-1) << "\n";

  /* write Y */
  tileP->get_logger()->get_stream("mosquito") << today << ",Y,";
  for(auto it = Y.begin(); it != Y.end()-1; it++){
    tileP->get_logger()->get_stream("mosquito") << *it << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << *(Y.end()-1) << "\n";

  /* write Z */
  tileP->get_logger()->get_stream("mosquito") << today << ",Z,";
  for(auto it = Z.begin(); it != Z.end()-1; it++){
    tileP->get_logger()->get_stream("mosquito") << *it << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << *(Z.end()-1) << "\n";

};

/* number of infectious bites */
double mosquito_rm::get_beta(const size_t p){
  return a * Z.at(p);
}

/* initialize logging */
void mosquito_rm::initialize_logging(){

  u_int tnow = tileP->get_tnow();

  /* format: patch,1,2,3,... */
  tileP->get_logger()->get_stream("mosquito") << "time,state,patch,";
  for(size_t i=0; i<N-1; i++){
    tileP->get_logger()->get_stream("mosquito") << patches[i] << ",";
  }
  tileP->get_logger()->get_stream("mosquito") << N-1 << "\n";

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
