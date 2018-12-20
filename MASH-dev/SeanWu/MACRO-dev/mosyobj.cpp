// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]   
#include <RcppArmadillo.h>

#include <vector>
#include <algorithm>

#include <iostream>

#include <memory>
#include <algorithm>

/* note tnow should be 0,...,364 as a yearly cycle */
class mosquitoRM {
public:

  mosquitoRM(const size_t N_, const arma::Mat<double>& lambda_, const arma::Mat<double>& psi_,
             const arma::Col<size_t>& EIP_, const size_t maxEIP_,
             const double p_, const double f_, const double Q_, const double v_,
             const arma::Row<double>& M_, const arma::Row<double>& Y_, const arma::Row<double>& Z_) :
    N(N_), lambda(lambda_), psi(psi_), EIP(EIP_), maxEIP(maxEIP_), P(maxEIP_),
    p(p_), f(f_), Q(Q_), a(f*Q), v(v_),
    M(M_), Y(Y_), Y0(N_), Z(Z_), ZZ(maxEIP_,N_), ZZ_shift(maxEIP_,maxEIP_)
  {
    std::cout << "mosquitoRM being born at " << this << std::endl;

    /* compute P */
    // unsigned int i = 0;
    std::generate(P.begin(),P.end(),[this->p,i = 0]() mutable { return std::pow(p,++i); });
    
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

  ~mosquitoRM(){
    std::cout << "mosquitoRM being killed at " << this << std::endl;
  };

  void print(){
    std::cout << "mosquitoRM being printed at " << this << std::endl;
    P.print("printing P");
    ZZ.print("printing ZZ");
    ZZ_shift.print("printing ZZ_shift");
  }
  
  void aquatic_dynamics(u_int tnow){
    std::cout << std::endl;
    std::cout << "testing aquatic_dynamics ... " << std::endl;
    M.print("M before aquatic_dynamics");
    arma::Row<double> lambda_today = lambda.row(tnow);
    for(size_t i=0; i<M.size(); i++){
      // M.at(i) += R::rpois(lambda.at(i));
      M.at(i) += lambda_today.at(i);
    }
    M.print("M after aquatic_dynamics");
    std::cout << std::endl;
  };
  
  void pop_dynamics(u_int tnow, const arma::Row<double>& kappa){
    
    std::cout << std::endl;
    std::cout << "testing pop_dynamics ... " << std::endl;
    
    unsigned int EIP_today = EIP.at(tnow);
    
    /* daily dynamics */
    M = p*M;
    Y = p*Y;
    Z = p*Z;
    
    M.print("M after daily dynamics");
    Y.print("Y after daily dynamics");
    Z.print("Z after daily dynamics");
    
    /* transmission */
    Y0 = (f * Q * kappa) % (M - Y);
    if(any(Y0 < 0.0)){
      std::replace_if(Y0.begin(), Y0.end(), [](const double y0){
        return y0 < 0.0;
      },0.0);
    }
    
    Y0.print("Y0 after daily dynamics");
    
    /* newly incubating mosquitos */
    Y += Y0;
    
    Y.print("Y after newly incubating mosquitos");
    
    /* migration & incubation */
    M = M * psi;
    Y = Y * psi;
    Z = (Z + ZZ.row(0)) * psi;
    
    M.print("M after migration");
    Y.print("Y after migration");
    Z.print("Z after migration");
    
    /* incubating mosquitos */
    ZZ = ZZ_shift * ZZ;
    ZZ.row(EIP_today-1) += P.at(EIP_today-1) * (Y0 * psi);
    
    std::cout << std::endl;
    
  };

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
  arma::Row<double>       M; /* adult females */
  arma::Row<double>       Y; /* incubating mosquitos */
  arma::Row<double>       Y0; /* newly infected mosquitos */
  arma::Row<double>       Z; /* infectious mosquitos */
  arma::Mat<double>       ZZ; /* incubating mosquitos */
  arma::Mat<int>          ZZ_shift;
};

/*
N <- 10
lambda <- matrix(rpois(N*365,10),365,N)
psi <- diag(N)
EIP <- rep(11,365)
maxEIP <- 11
p <- 0.95
f <- 1/3
Q <- 0.9
v <- 20
M <- rpois(N,1e4)
Y <- rpois(N,1e3)
Z <- rpois(N,1e2)
kappa <- rlnorm(N)
test_mosyRM(N,lambda,psi,EIP,maxEIP,p,f,Q,v,M,Y,Z,kappa)
 */
// [[Rcpp::export]]
void test_mosyRM(const size_t N_, const arma::Mat<double>& lambda_, const arma::Mat<double>& psi_,
                 const arma::Col<size_t>& EIP_, const size_t maxEIP_,
                 const double p_, const double f_, const double Q_, const double v_,
                 const arma::Row<double>& M_, const arma::Row<double>& Y_, const arma::Row<double>& Z_,
                 const arma::Row<double>& kappa){

  std::unique_ptr<mosquitoRM> mPtr = std::make_unique<mosquitoRM>(N_, lambda_, psi_,
                                                                  EIP_, maxEIP_,
                                                                  p_, f_, Q_, v_,
                                                                  M_,Y_, Z_);
  
  mPtr->print();
  mPtr->aquatic_dynamics(0);
  mPtr->pop_dynamics(0,kappa);

};

// [[Rcpp::export]]
arma::Row<double> test_psiMult(const arma::Mat<double>& psi, const arma::Row<double>& pop){
  arma::Row<double> out = pop * psi;
  return out;
}

// [[Rcpp::export]]
arma::Mat<double> test_shiftMat(const arma::Mat<double>& Z){
  
  arma::Mat<int> Z_shift(Z.n_rows,Z.n_rows);
  Z_shift.fill(0);
  Z_shift.submat(0,1,Z.n_rows-2,Z.n_rows-1) = arma::eye<arma::Mat<int> >(Z.n_rows-1,Z.n_rows-1);
  
  arma::Mat<double> out;
  out = Z_shift * Z;
  return out;
}

// [[Rcpp::export]]
arma::Row<double> test_transmission(const double a, const arma::Row<double>& kappa, 
                                    const arma::Row<double>& M, const arma::Row<double>& Y){
  
  arma::Row<double> out;
  out = (a * kappa) % (M - Y);
  return out;
}