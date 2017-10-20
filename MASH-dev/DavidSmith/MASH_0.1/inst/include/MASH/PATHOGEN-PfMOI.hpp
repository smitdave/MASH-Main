#ifndef _MASH_PFMOI_HPP_
#define _MASH_PFMOI_HPP_

#include <Rcpp.h>

namespace MASH {

// Human-stage PfMOI Object
class humanPfMOI {
// public members
public:

  ///////////////////////////////////
  // Human Stage PfMOI Constructor
  ///////////////////////////////////

  humanPfMOI(const int &PfID_init, const double &tInf_init = -1, const int &MOI_init = 0,
    const double &b_init = 0.55, const double &c_init = 0.15,
    const int &damID_init = -1, const int &sireID_init = -1,
    const bool &chemoprophylaxis_init = false);

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  std::vector<int> get_PfID(){
    return(PfID);
  };
  void push_PfID(const int &PfID_new){
    PfID.push_back(PfID_new);
  };

  std::vector<double> get_tInf(){
    return(tInf);
  };
  void push_tInf(const double &tInf_new){
    tInf.push_back(tInf_new);
  };

  int get_MOI(){
    return(MOI);
  };
  void set_MOI(const int &MOI_new){
    MOI = MOI_new;
  };

  double get_b(){
    return(b);
  };
  void set_b(const double &b_new){
    b = b_new;
  };

  double get_c(){
    return(c);
  };
  void set_c(const double &c_new){
    c = c_new;
  };

  std::vector<int> get_damID(){
    return(damID);
  };
  void push_damID(const int &damID_new){
    damID.push_back(damID_new);
  };

  std::vector<int> get_sireID(){
    return(sireID);
  };
  void push_sireID(const int &sireID_new){
    sireID.push_back(sireID_new);
  };

  bool get_chemoprophylaxis(){
    return(chemoprophylaxis);
  };
  void set_chemoprophylaxis(const bool &chemoprophylaxis_new){
    chemoprophylaxis = chemoprophylaxis_new;
  };

  ///////////////////////////////////
  // Infection Dynamics
  ///////////////////////////////////

  // add a new infection
  void add_Infection(const int &PfID_new, const int &damID_new, const int &sireID_new){
    PfID.push_back(PfID_new);
    damID.push_back(damID_new);
    sireID.push_back(sireID_new);
    MOI += 1;
    // eventually can push back this stuff to some history vectors.
  };

  // completely clear the infection associated with index ix
  void clear_Infection(const int &PfID_ix){
    // find infection associated with this PfID
    auto it = std::find(PfID.begin(), PfID.end(), PfID_ix);
    size_t ix = std::distance(PfID.begin(), it);
    PfID.erase(PfID.begin()+ix);
    damID.erase(damID.begin()+ix);
    sireID.erase(sireID.begin()+ix);
    MOI -= 1;
  };

  // get all infections where PfID != -1
  Rcpp::List get_Infection(){

    std::vector<int> infIx;
    auto it = std::find_if(PfID.begin(), PfID.end(), [](const int &PfID_iter){
        return(PfID_iter != -1);
    });
    while(it != PfID.end()){
        infIx.emplace_back(std::distance(PfID.begin(), it));
        it = std::find_if(std::next(it), std::end(PfID), [](const int &PfID_iter){
            return(PfID_iter != -1);
        });
    }

    // export these infections that have passed the EIP
    std::vector<int> PfID_out;
    std::vector<int> damID_out;
    std::vector<int> sireID_out;
    std::transform(infIx.begin(), infIx.end(), std::back_inserter(PfID_out), [this](size_t ix){
      return(PfID[ix]);
    });
    std::transform(infIx.begin(), infIx.end(), std::back_inserter(damID_out), [this](size_t ix){
      return(damID[ix]);
    });
    std::transform(infIx.begin(), infIx.end(), std::back_inserter(sireID_out), [this](size_t ix){
      return(sireID[ix]);
    });

    return(
      Rcpp::List::create(
        Rcpp::Named("PfID") = PfID_out,
        Rcpp::Named("damID") = damID_out,
        Rcpp::Named("sireID") = sireID_out
      )
    );
  };

  ///////////////////////////////////
  // PfMOI History
  ///////////////////////////////////

  // history tracking
  void track_history(const double &tEvent, const std::string &event){
    events.push_back(event);
    eventT.push_back(tEvent);
    MOI_history.push_back(MOI);
  };

  // return history
  Rcpp::List get_history(){
    return(
      Rcpp::List::create(
        Rcpp::Named("events") = events,
        Rcpp::Named("eventT") = eventT,
        Rcpp::Named("MOI") = MOI_history
      )
    );
  };

// private members
private:

  // PfMOI History
  std::vector<std::string> events;
  std::vector<double>      eventT;
  std::vector<int>         MOI_history;

  // PfMOI Parameters & State Variables
  std::vector<int> PfID; // pathogen ID
  std::vector<double> tInf; // time of infection (mosquito to human transmission)
  int MOI; // multiplicity of infection
  double b; // transmission efficiency: infected mosquito to human
  double c; // transmission efficiency: infected human to mosquito
  std::vector<int> damID; // female gametocyte mother
  std::vector<int> sireID; // male gametocyte father
  bool chemoprophylaxis;

};

// inline definition of constructor to accept default argument values
inline humanPfMOI::humanPfMOI(const int &PfID_init, const double &tInf_init,
  const int &MOI_init, const double &b_init, const double &c_init,
  const int &damID_init, const int &sireID_init,
  const bool &chemoprophylaxis_init){

    // set parameters and state variables
    PfID.push_back(PfID_init);
    tInf.push_back(tInf_init);
    MOI = MOI_init;
    b = b_init;
    c = c_init;
    damID.push_back(damID_init);
    sireID.push_back(sireID_init);
    chemoprophylaxis = chemoprophylaxis_init;

    // reserve memory for history
    events.reserve(50);
    events.push_back("init");
    eventT.reserve(50);
    eventT.push_back(-1);
    MOI_history.reserve(50);

  }


// Mosquito-stage PfMOI Object
class mosquitoPfMOI {
// public members
public:

  ///////////////////////////////////
  // Mosquito Stage PfMOI Constructor
  ///////////////////////////////////

  mosquitoPfMOI(const int &PfID_init = -1, const double &tInf_init = -1,
    const int &MOI_init = 0,
    const int &damID_init = -1, const int &sireID_init = -1);

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  std::vector<int> get_PfID(){
    return(PfID);
  };
  void push_PfID(const int &PfID_new){
    PfID.push_back(PfID_new);
  }

  std::vector<double> get_tInf(){
    return(tInf);
  };
  void push_tInf(const double &tInf_new){
    tInf.push_back(tInf_new);
  };

  int get_MOI(){
    return(MOI);
  };
  void set_MOI(const int &MOI_new){
    MOI = MOI_new;
  }

  std::vector<int> get_damID(){
    return(damID);
  };
  void push_damID(const int &damID_new){
    damID.push_back(damID_new);
  };

  std::vector<int> get_sireID(){
    return(sireID);
  };
  void push_sireID(const int &sireID_new){
    sireID.push_back(sireID_new);
  };

  ///////////////////////////////////
  // Infection Dynamics
  ///////////////////////////////////

  // add a new infection
  void add_Infection(const int &PfID_new, const double &tInf_new, const int &damID_new, const int &sireID_new){
    PfID.push_back(PfID_new);
    tInf.push_back(tInf_new);
    damID.push_back(damID_new);
    sireID.push_back(sireID_new);
    MOI += 1;
  };

  // return the clonal variant associated with given PfID
  Rcpp::List get_Infection(const int &PfID_ix){
    auto it = std::find(PfID.begin(), PfID.end(), PfID_ix);
    size_t ix = std::distance(PfID.begin(), it);
    return(
      Rcpp::List::create(
        Rcpp::Named("PfID") = PfID[ix],
        Rcpp::Named("damID") = damID[ix],
        Rcpp::Named("sireID") = sireID[ix]
      )
    );
  };

  // get clonal variants just depending on their position in vector
  Rcpp::List get_InfectionIx(const int &ix){
    return(
      Rcpp::List::create(
        Rcpp::Named("PfID") = PfID[ix],
        Rcpp::Named("damID") = damID[ix],
        Rcpp::Named("sireID") = sireID[ix]
      )
    );
  };

  // get_InfectionEIP: argument 'incubation' = tNow - EIP; only return infections that started at tNow - EIP in the past
  // because only those can possibly have passed the EIP and produced sporozoites.
  Rcpp::List get_InfectionEIP(const double &incubation){

    // find infections where tInf < tNow - EIP (incubation)
    std::vector<int> incubationIx;
    auto it = std::find_if(tInf.begin(), tInf.end(), [incubation](const double &infectionTime){
        return(infectionTime < incubation && infectionTime != -1);
    });
    while(it != tInf.end()){
        incubationIx.emplace_back(std::distance(tInf.begin(), it));
        it = std::find_if(std::next(it), std::end(tInf), [incubation](const double &infectionTime){
            return(infectionTime < incubation && infectionTime != -1);
        });
    }

    // export these infections that have passed the EIP
    std::vector<int> PfID_out;
    std::vector<int> damID_out;
    std::vector<int> sireID_out;
    std::transform(incubationIx.begin(), incubationIx.end(), std::back_inserter(PfID_out), [this](size_t ix){
      return(PfID[ix]);
    });
    std::transform(incubationIx.begin(), incubationIx.end(), std::back_inserter(damID_out), [this](size_t ix){
      return(damID[ix]);
    });
    std::transform(incubationIx.begin(), incubationIx.end(), std::back_inserter(sireID_out), [this](size_t ix){
      return(sireID[ix]);
    });

    return(
      Rcpp::List::create(
        Rcpp::Named("PfID") = PfID_out,
        Rcpp::Named("damID") = damID_out,
        Rcpp::Named("sireID") = sireID_out
      )
    );
  };

  // same as above but only return the indices
  std::vector<int> which_EIP(const double &incubation) {

    // find infections where tInf < tNow - EIP (incubation)
    std::vector<int> incubationIx;
    auto it = std::find_if(tInf.begin(), tInf.end(), [incubation](const double &infectionTime){
        return(infectionTime < incubation && infectionTime != -1);
    });
    while(it != tInf.end()){
        incubationIx.emplace_back(std::distance(tInf.begin(), it));
        it = std::find_if(std::next(it), std::end(tInf), [incubation](const double &infectionTime){
            return(infectionTime < incubation && infectionTime != -1);
        });
    }

    return(incubationIx);
  };

// private members
private:

  // PfMOI Parameters & State Variables
  std::vector<int>              PfID;
  std::vector<double>           tInf;
  int                           MOI;
  std::vector<int>              damID;
  std::vector<int>              sireID;

};

// inline definition of constructor to accept default argument values
inline mosquitoPfMOI::mosquitoPfMOI(const int &PfID_init, const double &tInf_init, const int &MOI_init,
  const int &damID_init, const int &sireID_init){

    // set parameters and state variables
    PfID.clear();
    tInf.clear();
    damID.clear();
    sireID.clear();

    PfID.push_back(PfID_init);
    tInf.push_back(tInf_init);
    damID.push_back(damID_init);
    sireID.push_back(sireID_init);
    MOI = MOI_init;

  }


}

#endif
