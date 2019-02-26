/* ################################################################################
#
#   Testing PDG (simple standalone c++ conversion)
#   Based on the R code in John Henry's dev folder
#
################################################################################ */

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]
#include <RcppArmadillo.h>

/* STL includes */
#include <vector>
#include <algorithm>

#include <memory>

/* for NAN macro and mathematical fn's */
#include <math.h>


/* NOTE: in R, NaN is used to stop tracking Pt and Gt */

/* human class */
class PDGHuman {

public:

  /* constructor */
  PDGHuman(
    const size_t ixH_,
    const std::vector<double>& Ptmu_,
    const std::vector<double>& Ptvar_,
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
      pfAges(pfAges_), Pf(pfAges_),
      Pt(NAN), Gt(NAN),
      ggr(ggr_), gdk(gdk_), pfdr(pfdr_),
      Ptmu(Ptmu_), Ptvar(Ptvar_),
      MOI(0),
      Imm(Imm_), immHalf(immHalf_), immSlope(immSlope_), immCounter(immCounter_), immThresh(immThresh_),
      fever(false),
      feverThresh(feverThresh_)
  {
    Pf.zeros();

    /* make the ageing matrix */
    ageMatrix(pfAges);

    // history
    hist_MOI.reserve(100);
    hist_Pt.reserve(100);
    hist_Gt.reserve(100);
    hist_Imm.reserve(100);
    hist_immCounter.reserve(100);
  };

  /* destructor */
  ~PDGHuman(){};

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

    /* removes from final category at a particular rate, relatively small */
    Pf.at(pfAges-1) = Pf.at(pfAges-1) - Rcpp::sum(Rcpp::rbinom(Pf.at(pfAges-1),1,pfdr));
    Pf.at(pfAges-1) = std::max(Pf.at(pfAges-1),0);

    /* shifts to next age group */
    Pf = A * Pf;

  };

  void update_Pt(){

    Pt = 0.0;

    /* pull from all of the age-specific distributions, sum to get total Pt; limit tails of dist'ns */
    for(size_t i=0; i<pfAges; i++){
      if(Pf.at(i) > 0){
        Pt = std::log10(std::pow(10,Pt) + std::pow(10,std::min( (double)Rcpp::min(Rcpp::rlnorm(Pf.at(i),Ptmu.at(i),Ptvar.at(i))) , 13.0)));
      }
    }

    /* don't care about very small numbers of parasites */
    if(Pt < 1.0){
      Pt = NAN;
    }

    /* include immune effect; this is a stub; here we just discount Pt by at most 99 percent */
    Pt = std::log10((1.0 - .99 * Imm)*std::pow(10,Pt));

  };

  void update_Gt(){

    /*
      multiply previous Pt by the average Gt created per Pt, log scaling
      sequestration/delay handled by the large (1-2 wk) time steps
    */
    if(!isnan(Pt)){
      if(isnan(Gt)){
        Gt = std::log10(ggr * std::pow(10,Pt));
      } else {
        Gt = std::log10((1.0 - gdk) * std::pow(10,Gt) + ggr * std::pow(10,Pt));
      }

    } else {
      Gt = std::log10((1.0 - gdk)*pow(10,Gt));
    }

    if(Gt < 3.0){
      Gt = NAN;
    }

  };

  void update_MOI(){
    MOI = arma::accu(Pf);
  };

  void update_Imm(){
    /* count up if above threshhold parasite density, down if below */
    if(isnan(Pt) || (Pt < immThresh)){
      immCounter = std::max(immCounter - 0.1, 0.0);
    } else {
      immCounter = std::min(immCounter + 1.0, 10.0);
    }

    /* ensures nonnegative-definiteness of counters */
    immCounter = std::max(immCounter,0.0);

    /* sigmoidal conversion of counter to immune effect */
    Imm = sigmoid(immCounter,immHalf,immSlope);
  };

  void update_History(){
    hist_MOI.emplace_back(MOI);
    hist_Pt.emplace_back(Pt);
    hist_Gt.emplace_back(Gt);
    hist_Imm.emplace_back(Imm);
    hist_immCounter.emplace_back(immCounter);
  };

  // get history bits
  std::vector<size_t> get_MOI(){return hist_MOI;}
  std::vector<double> get_Pt(){return hist_Pt;}
  std::vector<double> get_Gt(){return hist_Gt;}
  std::vector<double> get_Imm(){return hist_Imm;}
  std::vector<double> get_immCounter(){return hist_immCounter;}

private:

  /* make the age matrix A */
  void ageMatrix(const int size){
    A = arma::zeros<arma::Mat<int> >(size,size);
    A.diag(-1).ones();
    A.at(size-1,size-1) = 1;
  };

  /* sigmoid function */
  double sigmoid(const double x, const double xhalf, const double b){
    return std::pow(x/xhalf,b) / (std::pow(x/xhalf,b) + 1.0);
  }

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
  std::vector<size_t> hist_MOI;
  std::vector<double> hist_Pt;
  std::vector<double> hist_Gt;
  std::vector<double> hist_Imm;
  std::vector<double> hist_immCounter;
};

// [[Rcpp::export]]
Rcpp::List runPDG(const size_t tmax, const std::vector<double>& Ptmu, const std::vector<double>& Ptvar){

  std::unique_ptr<PDGHuman> hptr = std::make_unique<PDGHuman>(0,Ptmu,Ptvar);

  hptr->infect_human(1);

  for(size_t t=0; t<tmax; t++){
    hptr->update_human();
  }

  return Rcpp::List::create(Rcpp::Named("MOI") = Rcpp::wrap(hptr->get_MOI()),
                     Rcpp::Named("Pt") = Rcpp::wrap(hptr->get_Pt()),
                     Rcpp::Named("Gt") = Rcpp::wrap(hptr->get_Gt()),
                     Rcpp::Named("Imm") = Rcpp::wrap(hptr->get_Imm()),
                     Rcpp::Named("immCounter") = Rcpp::wrap(hptr->get_immCounter())
                   );
}


// // [[Rcpp::export]]
// arma::Mat<int> ageMatrix(const int size){
//   arma::Mat<int> A = arma::zeros<arma::Mat<int> >(size,size);
//   A.diag(-1).ones();
//   A.at(size-1,size-1) = 1;
//   return A;
// }
// 
// // [[Rcpp::export]]
// double checkPt(const std::vector<double>& Ptmu, const std::vector<double>& Ptvar, const arma::Col<int>& Pf, const double Pt0){
//   double Pt = Pt0;
//   std::cout << "Pt: " << Pt << std::endl;
//   /* pull from all of the age-specific distributions, sum to get total Pt; limit tails of dist'ns */
//   for(size_t i=0; i<Pf.size(); i++){
//     if(Pf.at(i) > 0){
//       std::cout << "i: " << i << " Pt: " << Pt << std::endl;
//       Pt = std::log10(std::pow(10,Pt) + std::pow(10,std::min( (double)Rcpp::min(Rcpp::rlnorm(Pf.at(i),Ptmu.at(i),Ptvar.at(i))) , 13.0)));
//     }
//   }
//   return Pt;
// }