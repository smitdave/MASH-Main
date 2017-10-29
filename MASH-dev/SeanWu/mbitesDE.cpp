#include <RcppArmadillo.h>
#include <math.h> // for M_PI

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

// parameters struct
typedef struct {
  int e = 5;                // expected number of eggs in each clutch that makes it to adulthood
  double PF = 0.99;            // probability of success at event
  double PB = 0.99;
  double PR = 0.99;
  double PL = 0.99;
  double PO = 0.99;
  double sF = 0.95;            // probabiltiy of surviving event
  double sB = 0.98;
  double sR = 0.98;
  double sL = 0.80;
  double sO = 0.98;
  arma::Mat<double> M;  // transition probabilities given success
  double tau = 21.0;           // time from eggs being laid to adult-mosquito-hood
} parameters;

// normalize arma::Mat
inline arma::Mat<double> makeTransitionMatrix(){
  arma::Mat<double> MM = {
    {4,2,1,0,1},
    {2,1,1,0,1},
    {2,1,1,0,2},
    {1,1,1,0,1},
    {6,4,2,1,0} 
  };
  MM.each_row() /= arma::sum(MM,1).t();
  MM.print();
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
SEXP make_parameters(){
  parameters *par = new parameters;
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

// [[Rcpp::export]]
Rcpp::NumericVector mbitesDE_cpp(const double &Fo, const double &Fe, const double &Bo, const double &Be,
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









// [[Rcpp::export]]
arma::Cube<double> testCube(){
  arma::Cube<double> a(3,4,5);
  double i = 1.0;
  for(arma::cube::iterator it = a.begin(); it != a.end(); it++){
    (*it) = i;
    i+=1.0;
  }
  // a.slice(0).print();
  a.subcube(0,0,0,0,a.n_cols-1,a.n_slices-1).print();
  // corresponds to: x = array(data = 1:(3*4*5),dim = c(3,4,5)); x[1,,]
  return(a);
};



// // PDE system
// inline arma::Row<double> mbitesDE(const arma::Cube<double> &array, const int &t, const int &a, const double &dt, const parameters* par){
//   
//   double Fo = array.at(t,a,0);
//   double Fe = array.at(t,a,1);
//   double Bo = array.at(t,a,2);
//   double Be = array.at(t,a,3);
//   double R = array.at(t,a,4);
//   double Lo = array.at(t,a,5);
//   double Le = array.at(t,a,6);
//   double Oo = array.at(t,a,7);
//   double Oe = array.at(t,a,8);
//   
//   arma::Row<double> out(9);
//   
//   double tt = (t-1)*dt;
//   
//   out.at(0) = rO(tt,par)*par->M.at(4,0)*(Oe+Oo) + rB(tt,par)*par->M.at(1,0)*(Be+Bo) + rR(tt,par)*par->M.at(2,0)*R + (rF(tt,par)*par->M.at(0,0)+aF(tt,par))*Fe - dF(tt)*Fo; // Fo
//   out.at(1) = (rF(tt,par)*par->M.at(0,0)+aF(tt,par))*Fo - dF(tt)*Fe; // Fe
//   out.at(2) = rO(tt,par)*par->M.at(2,1)*(Oe+Oo) + rF(tt,par)*par->M.at(0,1)*(Fe+Fo) + rR(tt,par)*par->M.at(2,1)*R + rB(tt,par)*(par->M.at(1,1)+aB(tt,par))*Bo - dB(tt)*Bo; // Bo
//   out.at(3) = (rB(tt,par)*par->M.at(1,1)+aB(tt,par))*Bo - dB(tt)*Be;
//   out.at(4) = rB(tt,par)*par->M.at(1,2)*(Be+Bo) - dR(tt)*R;
//   out.at(5) = rR(tt,par)*par->M.at(2,3)*R + rO(tt,par)*par->M.at(4,3)*(Oe+Oo) + (rL(tt,par)*par->M.at(3,3)+aL(tt,par))*Le - dL(tt)*Lo;
//   out.at(6) = (rL(tt,par)*par->M.at(3,3)+aL(tt,par))*Lo - dL(tt)*Le;
//   out.at(7) = rL(tt,par)*par->M.at(3,4)*(Le+Lo)+ rR(tt,par)*par->M.at(2,4)*R + (rO(tt,par)*par->M.at(4,4)+aO(tt,par))*Oe - dO(tt)*Oo;
//   out.at(8) = (rO(tt,par)*par->M.at(4,4)+aO(tt,par))*Oo - dO(tt)*Oe;
//   
//   return out;
// };

// // [[Rcpp::export]]
// void upwindSolve(const double &dt, const int &tfin, const double &dx, const int &xfin){
// // arma::Cube<double> upwindSolve(const double &dt, const int &tfin, const double &dx, const int &xfin){
// 
//   // make parameters
//   parameters p;
//   parameters* par = &p;
//   par->M = makeTransitionMatrix();
// 
//   // int ttot = int(tfin / dt);
//   int xtot = int(xfin / dx);
// 
//   // // DEBUG
//   // std::cout << par->PB << std::endl;
//   // std::cout << "    " << std::endl;
//   // (*par).M.print();
//   // std::cout << "    " << std::endl;
//   // // DEBUG
//   
//   arma::Mat<double> m(xtot+1,xtot+1);
//   m.eye();
//   m.diag(-1).fill(-1);
// 
//   m = m * double(dt/dx);
// 
//   // DEBUG
//   m.print();
//   // DEBUG
// 
//   // // array to contain all 9 matrices (time x age for each state variable)
//   // arma::Mat<double> v(xtot+1,9);
//   // v.zeros();
//   // v(0,0) = 1000;
//   // 
//   // arma::Mat<double> w = v;
//   // 
//   // arma::Cube<double> A(ttot+1,xtot+1,9);
//   // A.zeros();
//   // A(0,0,0) = 1000;
//   // 
//   // for(int i=0; i<ttot; i++){
//   //   for(int k=0; k<9; k++){
//   // 
//   //     w.col(k) = v.col(k) - m * v.col(k);
//   // 
//   //     // boundary condition - emerge from eggs
//   //     if(k==0){
//   //       if( (dt*i+dt/2.0) <= par->tau ){
//   //         v(0,k) = 0;
//   //       } else {
//   //         v(0,k) = par->e*arma::accu(v.col(7)+v.col(8));
//   //       }
//   //     }
//   // 
//   //     for(int j=1; j<xtot+1; j++){
//   //       arma::Row<double> rhs = mbitesDE(A,i,j,dt,par);
//   //       v(j,k) = w(j,k) + dt*rhs(k);
//   //     }
//   //     v.print();
//   //     // A.tube(i,0,i,A.n_cols) ;
//   //     // A.subcube(i,0,0,i,A.n_cols,A.n_slices);
//   //   }
//   // }
// 
//   // return(A);
// };