/* ################################################################################
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  A very simple version for the PRISM data
 *  Stand-alone PfMOI with no mosquitos (EIR is forcing)
 *
################################################################################ */

/* Rcpp bits */
#include <Rcpp.h>

// [[Rcpp::plugins(cpp14)]]

// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>

/* STL includes */
#include <string>
#include <functional>
#include <algorithm>
#include <memory>


/* global simulation time */
static unsigned int tnow_global = 0;

/* global parameters */
static double DurationPf = 200.0; // for simple infection
static double b = 0.55;
static double sigma = 1.0; // sigma > 1: competition (faster clearance), 0 < sigma < 1: facilitation (slower clearance)


/* ################################################################################
* sample waiting time (hazard functions for events)
################################################################################ */

/* clearance of one clonal strain */
double pfmoi_ttClear(const int MOI){
 double rho = (1.0/DurationPf) * std::pow(MOI,sigma);
 return R::rexp(1.0/rho);;
};

/* no latent period */
double pfmoi_ttLatent(){
  return 0;
}


/* ################################################################################
 * generic event class (abstract base)
################################################################################ */

class event {
public:

  /* constructor */
  event(std::string tag_, double tEvent_, std::function<void()> eventF_):
    tag(tag_),tEvent(tEvent_),eventF(eventF_) {};

  /* destructor */
  virtual ~event(){};

  /* move operators */
  event(event&&) = default;
  event& operator=(event&&) = default;

  /* copy operators */
  event(event&) = default;
  event& operator=(event&) = default;

  /* print (debugging) */
  void print(){
    std::cout << "event -- tag: " << tag << ", tEvent: " << tEvent << std::endl;
  };

  /* comparison for sorting */
  bool operator<(event e) const {
    return tEvent < e.tEvent;
  };

  /* information for event */
  std::string                        tag;
  double                             tEvent;
  std::function<void()>              eventF;

};


/* ################################################################################
* PfMOI event declarations
################################################################################ */

/* need to forward declare events here */
class human;

/* simulated infectious bite; tag: PfMOI_SimBite */
class e_pfmoi_bite : public event {
public:
  /* constructor */
  e_pfmoi_bite(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_bite(){};

};

/* start a PfMOI infection; tag: PfMOI_infection */
class e_pfmoi_infect : public event {
public:
  /* constructor */
  e_pfmoi_infect(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_infect(){};
};


/* clear a PfMOI infection; tag: PfMOI_clearance */
class e_pfmoi_clear : public event {
public:
  /* constructor */
  e_pfmoi_clear(double tEvent_, human* h);

  /* destructor */
  ~e_pfmoi_clear(){};
};


/* ################################################################################
* human class
################################################################################ */

using eventP = std::unique_ptr<event>;

/* the class definition */
class human {
public:

  /* constructor & destructor */
  human(const int id_, const std::vector<double>& EIR_size_, const std::vector<double>& EIR_prob_, const int moi, Rcpp::IntegerVector* const foi_ptr) :
    id(id_), tnow(0.0), MOI(0), EIR_size(EIR_size_), EIR_prob(EIR_prob_), foi_hist(foi_ptr), bites(0) {
      if(moi > 0){
        for(size_t i=0; i<moi; i++){
          addEvent2Q(e_pfmoi_infect(0.0,this));
        }
      }
    };
  ~human(){};

  /* move operators */
  human(human&&) = default;
  human& operator=(human&&) = default;

  /* copy operators */
  human(human&) = delete;
  human& operator=(human&) = delete;

  /* print */
  void print();

  /* accessors */
  u_int                 get_id(){return id;};
  double                get_tnow(){return tnow;};

  unsigned int          get_MOI(){return MOI;};
  void                  inc_MOI(){MOI++;};
  void                  dec_MOI(){MOI--;};

  int                   get_bites(){return bites;};

  Rcpp::IntegerVector*  get_foi_hist(){return foi_hist;};

  /* event queue related functions */
  void                  addEvent2Q(event&& e);
  void                  rmTagFromQ(const std::string &tag);
  void                  fireEvent();

  /* interface */
  void                  simulate();

private:

  /* basic fields */
  u_int                 id;     /* my id */
  double                tnow;   /* my local simulation time (time of last jump) */
  unsigned int          MOI;    /* multiplicity of infection (number of genotypically distinct clones) */

  std::vector<eventP>   eventQ;

  /* biting: for nbinom(size,prob) parameterization */
  std::vector<double>   EIR_size;
  std::vector<double>   EIR_prob;

  /* pointer to the vector that keeps track of when new infections happen */
  Rcpp::IntegerVector*  foi_hist;

  /* number of potentially infectious bites i got today */
  int                   bites;

  /* called by simulate */
  void                  queue_bites();

};


/* ################################################################################
 * event queue
################################################################################ */

/* add an event to my queue */
void human::addEvent2Q(event&& e){
  eventQ.emplace_back(std::make_unique<event>(e));
  std::sort(eventQ.begin(),eventQ.end(),[](const std::unique_ptr<event>& e1, const std::unique_ptr<event>& e2){
    return e1->tEvent < e2->tEvent;
  });
};

/* remove future queued events */
void human::rmTagFromQ(const std::string &tag){
  eventQ.erase(std::remove_if(eventQ.begin(),eventQ.end(),
                              [tag](eventP& e){
                                return(tag.compare(e->tag)==0);
                              }),eventQ.end());
};

/* fire the first event */
void human::fireEvent(){
  if(eventQ.size() > 0){
    tnow = eventQ.front()->tEvent; /* update local simulation time */
    eventQ.front()->eventF();
    eventQ.erase(eventQ.begin());
  }
};


/* ################################################################################
 * PfMOI events
################################################################################ */

/* simulated biting event */
e_pfmoi_bite::e_pfmoi_bite(double tEvent_, human* h):
  event("PfMOI_SimBite",tEvent_,[tEvent_,h](){
    /* transmission efficiency */
    if(R::runif(0.0, 1.0) < b){
      double tInfStart = tEvent_ + pfmoi_ttLatent();
      h->addEvent2Q(e_pfmoi_infect(tInfStart,h));
    }

  })
{};

/* infection */
e_pfmoi_infect::e_pfmoi_infect(double tEvent_, human* h):
  event("PfMOI_infection",tEvent_,[tEvent_,h](){

    /* invaded by a new clonal infection */
    h->inc_MOI();

    /* track hist */
    h->get_foi_hist()->at(tnow_global) += 1;

    /* queue clearance event */
    double tEnd = tEvent_ + pfmoi_ttClear(h->get_MOI());
    h->addEvent2Q(e_pfmoi_clear(tEnd,h));

  })
{};

/* recovery */
e_pfmoi_clear::e_pfmoi_clear(double tEvent_, human* h):
  event("PfMOI_clearance",tEvent_,[tEvent_,h](){

    /* MOI -= 1 */
    h->dec_MOI();

  })
{};


/* ################################################################################
 * PfMOI simulation loop
################################################################################ */

void human::simulate(){

  /* fire all events that occur on this time step */
  while(eventQ.size() > 0 && eventQ.front()->tEvent < tnow_global){
    fireEvent();
  }

  /* queue bites */
  queue_bites();

};


void human::queue_bites(){

  /* parameters of nbinom biting */
  double size = EIR_size.at(tnow_global);
  double prob = EIR_prob.at(tnow_global);

  bites = (int)R::rnbinom(size, prob);

  if(bites > 0){
    for(size_t i=0; i<bites; i++){
      addEvent2Q(e_pfmoi_bite(tnow_global,this));
    }
  }

};


/* ################################################################################
 * Run a simulation from R
################################################################################ */

/* human pointer */
using humanP = std::unique_ptr<human>;

// [[Rcpp::export]]
Rcpp::List tiny_pfmoi(const unsigned int tmax,
                     const size_t nh,
                     const Rcpp::IntegerVector init,
                     const Rcpp::List EIR_size,
                     const Rcpp::List EIR_prob,
                     const bool pb){

  /* checks */
  if(nh != EIR_size.size() || nh != EIR_prob.size()){
    Rcpp::stop("number of humans to simulate must be same as length of EIR size and prob lists");
  }

  /* global clock */
  tnow_global = 0;

  /* output matrix */
  Rcpp::IntegerMatrix out_mat(tmax,nh);
  Rcpp::IntegerMatrix out_bites(tmax,nh);
  Rcpp::IntegerVector out_foi(tmax);

  /* set up our ensemble of people */
  std::vector<humanP> humans;
  humans.reserve(nh);
  for(size_t i=0; i<nh; i++){

    humans.emplace_back(std::make_unique<human>(
      i,
      Rcpp::as<std::vector<double> >(EIR_size.at(i)),
      Rcpp::as<std::vector<double> >(EIR_prob.at(i)),
      init.at(i),
      &out_foi
    ));

  }

  /* track progress */
  Progress progbar(tmax, pb);

  /* run simulation */
  while(tnow_global < tmax){

    if(Progress::check_abort()){
      Rcpp::stop("user abort detected");
    };

    /* sim humans */
    for(auto& h : humans){
      h->simulate();
    }

    /* write output */
    for(auto& h : humans){

      /* write bites */
      out_bites.at(tnow_global,h->get_id()) = h->get_bites();

      /* write MOI */
      out_mat.at(tnow_global,h->get_id()) = h->get_MOI();
    }

    progbar.increment();
    tnow_global++;
  }

  /* return a list */
  return Rcpp::List::create(Rcpp::Named("MOI") = out_mat,
                            Rcpp::Named("bites") = out_bites,
                            Rcpp::Named("foi") = out_foi
                          );
};
