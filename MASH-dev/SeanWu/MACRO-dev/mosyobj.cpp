#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <vector>

class mosquitoRM {
public:
  
  mosquitoRM(const size_t N_, const arma::Mat<double> lambda_, const arma::Mat<double> psi_,
             const arma::Col<size_t> EIP_) : 
    N(N_), lambda(lambda_), psi(psi_), EIP(EIP_)
  {};
  ~mosquitoRM();
  
private:
  
  size_t                  N; /* number of patches */
  arma::Mat<double>       lambda; /* emergence matrix (365 X N) */
  arma::Mat<double>       psi; /* diffusion matrix (N X N) */
  
  arma::Col<size_t>       EIP; /* EIP on each day of the year */
  size_t                  maxEIP;
  arma::Col<double>       P; /* survival over EIP */
  
  double                  p; /* daily survival */
  double                  f; /* blood feeding rate */
  double                  Q; /* human biting rate */
  double                  a; /* human biting rate */
  double                  v; /* daily egg laying rate */
  
  /* life stages */
  arma::Col<double>       M; /* adult females */
  arma::Col<double>       Y; /* incubating mosquitos */
  arma::Col<double>       Y0; /* newly infected mosquitos */
  arma::Col<double>       Z; /* infectious mosquitos */
  arma::Mat<double>       ZZ; /* incubating mosquitos */
  
};