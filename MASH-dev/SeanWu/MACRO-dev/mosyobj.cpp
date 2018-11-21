// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]   
#include <RcppArmadillo.h>

#include <vector>
#include <algorithm>

#include <iostream>

#include <memory>

class mosquitoRM {
public:

  mosquitoRM(const size_t N_, const arma::Mat<double>& lambda_, const arma::Mat<double>& psi_,
             const arma::Col<size_t>& EIP_, const size_t maxEIP_,
             const double p_, const double f_, const double Q_, const double v_,
             const arma::Col<double>& M_, const arma::Col<double>& Y_, const arma::Col<double>& Z_) :
    N(N_), lambda(lambda_), psi(psi_), EIP(EIP_), maxEIP(maxEIP_), P(maxEIP_),
    p(p_), f(f_), Q(Q_), a(f*Q), v(v_),
    M(M_), Y(Y_), Y0(N_), Z(Z_), ZZ(N_,maxEIP_)
  {
    std::cout << "mosquitoRM being born at " << this << std::endl;

    /* compute P */
    unsigned int i = 1;
    std::generate(P.begin(),P.end(),[&](){ return std::pow(p,++i); });

    /* compute ZZ */
    for(size_t i=0; i<ZZ.n_cols; i++){
      for(auto it = ZZ.begin_col(i); it != ZZ.end_col(i); it++){
        (*it) = Z.at(i)*(1.0 - p);
      }
    }
    
    /* zero out Y0 */
    Y0.fill(0.0);

  };

  ~mosquitoRM(){
    std::cout << "mosquitoRM being killed at " << this << std::endl;
  };

  void print(){
    std::cout << "mosquitoRM being printed at " << this << std::endl;
    P.print("printing P");
    ZZ.print("printing ZZ");
  }

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

// [[Rcpp::export]]
void test_mosyRM(const size_t N_, const arma::Mat<double>& lambda_, const arma::Mat<double>& psi_,
                 const arma::Col<size_t>& EIP_, const size_t maxEIP_,
                 const double p_, const double f_, const double Q_, const double v_,
                 const arma::Col<double>& M_, const arma::Col<double>& Y_, const arma::Col<double>& Z_){

  std::unique_ptr<mosquitoRM> mPtr = std::make_unique<mosquitoRM>(N_, lambda_, psi_,
                                                                  EIP_, maxEIP_,
                                                                  p_, f_, Q_, v_,
                                                                  M_,Y_, Z_);

  mPtr->print();

};