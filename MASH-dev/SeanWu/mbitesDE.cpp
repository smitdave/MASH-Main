/*
 * ################################################################################
 * #
 * #  MBITES-DE
 * #  PDE approximation of semi-markov model of mosquito life-cycle
 * #  MBITES Team
 * #  October 2017
 * #
 * ################################################################################
 */

#include <RcppArmadillo.h>
#include <math.h> // for M_PI

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;


/*
 * ################################################################################
 * #  Helper functions and parameters setup
 * ################################################################################
 */

// parameters struct
typedef struct parameters {
  // parameters
  int e = 5;                    // expected number of eggs in each clutch that makes it to adulthood
  double PF = 0.99;             // probability of success at event
  double PB = 0.99;
  double PR = 0.99;
  double PL = 0.99;
  double PO = 0.99;
  double sF = 0.95;             // probabiltiy of surviving event
  double sB = 0.98;
  double sR = 0.98;
  double sL = 0.80;
  double sO = 0.98;
  double tau = 21.0;            // time from eggs being laid to adult-mosquito-hood
  arma::Mat<double> M;          // transition probabilities given success
  // constructor
  parameters(const int &_e, 
             const double &_PF, const double &_PB, const double &_PR, const double &_PL, const double &_PO, 
             const double &_sF, const double &_sB, const double &_sR, const double &_sL, const double &_sO, 
             const double &_tau) : e(_e), PF(_PF), PB(_PB), PR(_PR), PL(_PL), PO(_PO), sF(_sF), sB(_sB), sR(_sR), sL(_sL), sO(_sO), tau(_tau) {};
  ~parameters(){};
} parameters;

// normalize arma::Mat
inline arma::Mat<double> makeTransitionMatrix(){
  arma::Mat<double> MM = {
    {4,2,2,1,6},
    {2,1,1,1,4},
    {1,1,1,1,2},
    {0,0,0,0,1},
    {1,1,2,1,0} 
  };
  arma::colvec rowSums = arma::sum(MM,1);
  for(size_t i=0; i<5; i++){
    MM.row(i) = MM.row(i) / rowSums.at(i);
  }
  return MM;
}

// destructor for parameters
inline void free_parameters(parameters* ptr){
  ::free(ptr);
  ::printf("free_parameters called.\n");
};

// Rcpp pointer interface
typedef Rcpp::XPtr<parameters, Rcpp::PreserveStorage, free_parameters> parameters_ptr;

// [[Rcpp::export]]
SEXP make_parameters(const int &e = 5, 
                     const double &PF = 0.99, const double &PB = 0.99, const double &PR = 0.99, const double &PL = 0.99, const double &PO = 0.99, 
                     const double &sF = 0.95, const double &sB = 0.98, const double &sR = 0.98, const double &sL = 0.80, const double &sO = 0.98, 
                     const double &tau = 21.0){
  parameters *par = new parameters(e,PF,PB,PR,PL,PO,sF,sB,sR,sL,sO,tau);
  par->M = makeTransitionMatrix();
  parameters_ptr ptr(par,true);
  return ptr;
};

// diurnal activity
inline double activity(const double &t, const double &avg, const double &max){
  return (max-avg)*cos(2*M_PI*t/24)+avg;
};

// unconditional waiting times
inline double gammaF(const double &t){
  return activity(t,1.0,(1.5*1.0));
};
inline double gammaB(const double &t){
  return activity(t,0.75,(1.5*0.75));
};
inline double gammaR(const double &t){
  return activity(t,1.5,(1.5*1.5));
};
inline double gammaL(const double &t){
  return activity(t,0.75,(1.5*0.75));
};
inline double gammaO(const double &t){
  return activity(t,1.0,(1.5*1.0));
};

// success * survive/time
inline double rF(const double &t, const parameters* par){
  return par->PF / (gammaF(t) * par->sF);
};
inline double rB(const double &t, const parameters* par){
  return par->PB / (gammaB(t) * par->sB);
};
inline double rR(const double &t, const parameters* par){
  return par->PR / (gammaR(t) * par->sR);
};
inline double rL(const double &t, const parameters* par){
  return par->PL / (gammaL(t) * par->sL);
};
inline double rO(const double &t, const parameters* par){
  return par->PO / (gammaO(t) * par->sO);
};

// 1/time (rate *something* happens)
inline double dF(const double &t){
  return 1.0 / gammaF(t);
};
inline double dB(const double &t){
  return 1.0 / gammaB(t);
};
inline double dR(const double &t){
  return 1.0 / gammaR(t);
};
inline double dL(const double &t){
  return 1.0 / gammaL(t);
};
inline double dO(const double &t){
  return 1.0 / gammaO(t);
};

// Fail * survive / time
inline double aF(const double &t, const parameters* par){
  return (1 - par->PF) * par->sF / gammaF(t);
};
inline double aB(const double &t, const parameters* par){
  return (1 - par->PB) * par->sB / gammaB(t);
};
inline double aR(const double &t, const parameters* par){
  return (1 - par->PR) * par->sR / gammaR(t);
};
inline double aL(const double &t, const parameters* par){
  return (1 - par->PL) * par->sL / gammaL(t);
};
inline double aO(const double &t, const parameters* par){
  return (1 - par->PO) * par->sO / gammaO(t);
};


/*
 * ################################################################################
 * #  Function to calculate derivatives for John Henry's solver
 * ################################################################################
 */

// [[Rcpp::export]]
Rcpp::NumericVector mbitesDE_cpp(const double &Fo, const double &Fe, 
                                 const double &Bo, const double &Be,
                                 const double &R,
                                 const double &Lo, const double &Le,
                                 const double &Oo, const double &Oe,
                                 const int &t, const double &dt, const SEXP &p){


  Rcpp::NumericVector out(9);

  Rcpp::XPtr<parameters> par(p);
  
  double tt = (t-1)*dt;

  out.at(0) = rO(tt,par)*par->M.at(4,0)*(Oe+Oo) + rB(tt,par)*par->M.at(1,0)*(Be+Bo) + rR(tt,par)*par->M.at(2,0)*R + (rF(tt,par)*par->M.at(0,0)+aF(tt,par))*Fe - dF(tt)*Fo; // Fo
  out.at(1) = (rF(tt,par)*par->M.at(0,0)+aF(tt,par))*Fo - dF(tt)*Fe; // Fe
  out.at(2) = rO(tt,par)*par->M.at(2,1)*(Oe+Oo) + rF(tt,par)*par->M.at(0,1)*(Fe+Fo) + rR(tt,par)*par->M.at(2,1)*R + rB(tt,par)*(par->M.at(1,1)+aB(tt,par))*Bo - dB(tt)*Bo; // Bo
  out.at(3) = (rB(tt,par)*par->M.at(1,1)+aB(tt,par))*Bo - dB(tt)*Be;
  out.at(4) = rB(tt,par)*par->M.at(1,2)*(Be+Bo) - dR(tt)*R;
  out.at(5) = rR(tt,par)*par->M.at(2,3)*R + rO(tt,par)*par->M.at(4,3)*(Oe+Oo) + (rL(tt,par)*par->M.at(3,3)+aL(tt,par))*Le - dL(tt)*Lo;
  out.at(6) = (rL(tt,par)*par->M.at(3,3)+aL(tt,par))*Lo - dL(tt)*Le;
  out.at(7) = rL(tt,par)*par->M.at(3,4)*(Le+Lo)+ rR(tt,par)*par->M.at(2,4)*R + (rO(tt,par)*par->M.at(4,4)+aO(tt,par))*Oe - dO(tt)*Oo;
  out.at(8) = (rO(tt,par)*par->M.at(4,4)+aO(tt,par))*Oo - dO(tt)*Oe;

  return out;
};


/*
 * ################################################################################
 * #  Standalone C++ implementation of John Henry's upwind first order scheme
 * #  TO DO: avoid extra memory copy at end of loop over compartments 1...K
 * ################################################################################
 */

// PDE system
inline arma::Row<double> mbitesDE_cppInline(const arma::Cube<double> &array, const int &t, const int &a, const double &dt, const double &da, const parameters* par){

  double Fo = array.at(t,a,0);
  double Fe = array.at(t,a,1);
  double Bo = array.at(t,a,2);
  double Be = array.at(t,a,3);
  double R = array.at(t,a,4);
  double Lo = array.at(t,a,5);
  double Le = array.at(t,a,6);
  double Oo = array.at(t,a,7);
  double Oe = array.at(t,a,8);

  arma::Row<double> out(9);

  double tt = (t-1)*dt;

  out.at(0) = rO(tt,par)*par->M.at(4,0)*(Oe+Oo) + rB(tt,par)*par->M.at(1,0)*(Be+Bo) + rR(tt,par)*par->M.at(2,0)*R + (rF(tt,par)*par->M.at(0,0)+aF(tt,par))*Fe - dF(tt)*Fo; // Fo
  out.at(1) = (rF(tt,par)*par->M.at(0,0)+aF(tt,par))*Fo - dF(tt)*Fe; // Fe
  out.at(2) = rO(tt,par)*par->M.at(2,1)*(Oe+Oo) + rF(tt,par)*par->M.at(0,1)*(Fe+Fo) + rR(tt,par)*par->M.at(2,1)*R + rB(tt,par)*(par->M.at(1,1)+aB(tt,par))*Bo - dB(tt)*Bo; // Bo
  out.at(3) = (rB(tt,par)*par->M.at(1,1)+aB(tt,par))*Bo - dB(tt)*Be;
  out.at(4) = rB(tt,par)*par->M.at(1,2)*(Be+Bo) - dR(tt)*R;
  out.at(5) = rR(tt,par)*par->M.at(2,3)*R + rO(tt,par)*par->M.at(4,3)*(Oe+Oo) + (rL(tt,par)*par->M.at(3,3)+aL(tt,par))*Le - dL(tt)*Lo;
  out.at(6) = (rL(tt,par)*par->M.at(3,3)+aL(tt,par))*Lo - dL(tt)*Le;
  out.at(7) = rL(tt,par)*par->M.at(3,4)*(Le+Lo)+ rR(tt,par)*par->M.at(2,4)*R + (rO(tt,par)*par->M.at(4,4)+aO(tt,par))*Oe - dO(tt)*Oo;
  out.at(8) = (rO(tt,par)*par->M.at(4,4)+aO(tt,par))*Oo - dO(tt)*Oe;

  return out;
};

// [[Rcpp::export]]
arma::Cube<double> upwindSolveCPP(const double &dt, const int &tfin, const double &dx, const int &xfin,
                                  const int &e = 5, 
                                  const double &PF = 0.99, const double &PB = 0.99, const double &PR = 0.99, const double &PL = 0.99, const double &PO = 0.99, 
                                  const double &sF = 0.95, const double &sB = 0.98, const double &sR = 0.98, const double &sL = 0.80, const double &sO = 0.98, 
                                  const double &tau = 21.0){
// void upwindSolve(const double &dt, const int &tfin, const double &dx, const int &xfin){

  // make parameters
  parameters *par = new parameters(e,PF,PB,PR,PL,PO,sF,sB,sR,sL,sO,tau);
  par->M = makeTransitionMatrix();

  int ttot = int(tfin / dt);
  int xtot = int(xfin / dx);

  arma::Mat<double> m(xtot+1,xtot+1);
  m.eye();
  m.diag(-1).fill(-1);
  m = m * double(dt/dx);

  // array to contain all 9 matrices (time x age for each state variable)
  arma::Mat<double> v(xtot+1,9);
  v.zeros();
  v(0,0) = 1000;

  arma::Mat<double> w = v;

  arma::Cube<double> A(ttot+1,xtot+1,9);
  A.zeros();
  A(0,0,0) = 1000;

  for(int i=0; i<ttot; i++){
    for(int k=0; k<9; k++){

      w.col(k) = v.col(k) - m * v.col(k);

      // boundary condition - emerge from eggs
      if(k==0){
        if( (dt*i+dt/2.0) <= par->tau ){
          v(0,k) = 0;
        } else {
          v(0,k) = par->e*arma::accu(v.col(7)+v.col(8));
        }
      }

      for(int j=1; j<xtot+1; j++){
        arma::Row<double> rhs = mbitesDE_cppInline(A,i,j,dt,dx,par);
        v(j,k) = w(j,k) + dt*rhs(k);
      }
      
      arma::cube newCube((const double*)v.begin(),1,xtot+1,9); // annoying memory copy...must be a way to avoid this
      A(arma::span(i+1,i+1),arma::span::all,arma::span::all) = newCube;
    }
  }

  return(A);
};


/*
 * ################################################################################
 * #  DEPRECATED CODE
 * ################################################################################
 */

// // [[Rcpp::export]]
// arma::Cube<double> testCube(){
//   arma::Cube<double> a(3,4,5);
//   double i = 1.0;
//   for(arma::cube::iterator it = a.begin(); it != a.end(); it++){
//     (*it) = i;
//     i+=1.0;
//   }
//   a.print();
//   // a.subcube(0,0,0,0,a.n_cols-1,a.n_slices-1).print();
//   // std::cout << "span " << std::endl;
//   // std::cout << std::endl;
//   a(arma::span(0,0),arma::span::all,arma::span::all).print();
//   // corresponds to: x = array(data = 1:(3*4*5),dim = c(3,4,5)); x[1,,]
//   
//   arma::Mat<double> MM = {
//     {5,4,3,2,1},
//     {6,5,4,3,2},
//     {7,6,5,4,3},
//     {8,7,6,5,4}
//   };
//   // std::cout << "print MM    " << std::endl;
//   // MM.print();
//   
//   arma::cube newCube((const double*)MM.begin(),1,4,5);
//   // newCube.print();
//   
//   a(arma::span(0,0),arma::span::all,arma::span::all) = newCube;
//   a.print();
//   return(a);
// };