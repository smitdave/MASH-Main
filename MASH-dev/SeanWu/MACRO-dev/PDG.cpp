// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]
#include <RcppArmadillo.h>

/* STL includes */
#include <vector>
#include <algorithm>

#include <math.h>


// [[Rcpp::export]]
arma::Mat<int> ageMatrix(const int size){
  arma::Mat<int> A = arma::zeros<arma::Mat<int> >(size,size);
  A.diag(-1).ones();
  A.at(size-1,size-1) = 1;
  return A;
}

/* NOTE: in R, NaN is used to stop tracking Pt and Gt; in C++ we just set it to -1 */

/* human class */
class PDGHuman {

public:

  /* constructor */
  PDGHuman(
    const size_t ixH_,
    const std::vector<double> Ptmu_,
    const std::vector<double> Ptvar_,
    const size_t pfAges_ = 16,
    const double ggr_ = 0.01,
    const double gdk_ = 0.7,
    const double pfdr_ = 0.005,
    const double Imm_ = 0.0,
    const double immHalf_ = 4.0,
    const double immSlope_ = 3.0,
    const double immCounter_ = 0.0,
    const double immThresh_ = 7.5,
    const double feverThresh_ = 8.0
  ) : ixH(ixH_), age(24.0), sex(1),
      pfAges(pfAges_), Pf(pfAges_,0),
      Pt(-1.0), Gt(-1.0),
      ggr(ggr_), gdk(gdk_), pfdr(pfdr_),
      Ptmu(Ptmu_), Ptvar(Ptvar_),
      MOI(0),
      Imm(Imm_), immHalf(immHalf_), immSlope(immSlope_), immCounter(immCounter_), immThresh(immThresh_),
      fever(false),
      feverThresh(feverThresh_)
  {
    /* make the ageing matrix */
    ageMatrix(pfAges);
  };

  /* destructor */
  ~PDGHuman();

  /* infection methods */
  void infect_human(const size_t nInfections){
   Pf.at(0) += nInfections;
 };

  /* update functions */
  void update_human(){

    /* these use old value of Pt, so must be computed first */
    update_Imm();
    update_Gt();

    /* these update respectively Pf, Pt, MOI */
    age_Infections();
    update_Pt();
    update_MOI();

    update_History();
  };

  void age_Infections(){
    // Pf.at(pfAges-1) = max(private$Pf[private$pfAges] - sum(rbinom(private$Pf[private$pfAges],1,private$pfdr)),0)
    Pf.at(pfAges-1) = Pf.at(pfAges-1) - R::rbinom(Pf.at(pfAges-1),pfdr);
    Pf.at(pfAges-1) = std::max(Pf.at(pfAges-1),0);
  };

  void update_Pt(){

    Pt = 0.0;

    /* pull from all of the age-specific distributions, sum to get total Pt; limit tails of dist'ns */
    for(size_t i=0; i<pfAges; i++){
      if(Pf.at(i) > 0){
        Pt = std::log10(std::pow(Pt,10) + std::pow(std::min( (double)Rcpp::sum(Rcpp::rlnorm(Pf.at(i),Ptmu.at(i),Ptvar.at(i))) , 13.0),10));
      }
    }

    /* don't care about very small numbers of parasites */

  }


private:

  /* make the age matrix A */
  void ageMatrix(const int size){
    A = arma::zeros<arma::Mat<int> >(size,size);
    A.diag(-1).ones();
    A.at(size-1,size-1) = 1;
  };

  size_t            ixH; /* id number */
  double            age;
  size_t            sex;

  /* Pf */
  size_t              pfAges; /* number of age categories of infection */
  arma::Col<int>      Pf; /* number of active infections in each age category */
  double              Pt; /* asexual parasite count */
  double              Gt; /* gametocyte count */
  double              ggr; /* "gametocyte growth rate" (per asexual rate of Gt production) */
  double              gdk; /* "gametocyte decay rate" (death rate of Gt) */
  double              pfdr; /* probability of clearing old infection */
  arma::Mat<int>      A; /* ageing matrix */
  std::vector<double> Ptmu; /* mean of lognormal densities for ages 1:pfAges; must be of length pfAges */
  std::vector<double> Ptvar; /* variances of lognormal densities for ages 1:pfAges; must be of length pfAges */
  size_t              MOI; /* multiplicity of infection, sum of vector pf */

  /* Immunity */
  double              Imm; /* normalized immune strength */
  double              immHalf; /* half maximum immunity, sigmoid param */
  double              immSlope; /* slope of immune conversion, sigmoid param */
  double              immCounter; /* counts up if Pt > PtThresh, down otherwise */
  double              immThresh; /* immunogenic threshhold, based on Pt */

  /* Health */
  bool                fever;
  double              feverThresh;

  /* History Tracking */

};
