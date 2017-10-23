#ifndef _MASH_PFSI_HPP_
#define _MASH_PFSI_HPP_

#include <Rcpp.h>

namespace MASH {

// Human-stage PfSI Object
class humanPfSI {
// public members
public:

  ///////////////////////////////////
  // Human Stage PfSI Constructor
  ///////////////////////////////////

  humanPfSI(const int &PfID_init, const double &tInf_init = -1,
    const double &b_init = 0.55, const double &c_init = 0.15,
    const int &damID_init = -1, const int &sireID_init = -1,
    const bool &infected_init = false, const bool &chemoprophylaxis_init = false);

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  std::vector<int> get_PfID(){
    return(PfID);
  };
  void push_PfID(const int &PfID_new){
    PfID.push_back(PfID_new);
  };
  int back_PfID(){
    return(PfID.back());
  };

  std::vector<double> get_tInf(){
    return(tInf);
  };
  void push_tInf(const double &tInf_new){
    tInf.push_back(tInf_new);
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

  bool get_infected(){
    return(infected);
  };
  void set_infected(const bool &infected_new){
    infected = infected_new;
  };

  bool get_chemoprophylaxis(){
    return(chemoprophylaxis);
  };
  void set_chemoprophylaxis(const bool &chemoprophylaxis_new){
    chemoprophylaxis = chemoprophylaxis_new;
  };


  ///////////////////////////////////
  // PfSI History
  ///////////////////////////////////

  // history tracking
  void track_history(const double &tEvent, const std::string &event){
    events.push_back(event);
    eventT.push_back(tEvent);
  };

  // return history
  Rcpp::List get_history(){
    return(
      Rcpp::List::create(
        Rcpp::Named("events") = events,
        Rcpp::Named("eventT") = eventT
      )
    );
  };

// private members
private:

  // PfSI History
  std::vector<std::string> events;
  std::vector<double>      eventT;

  // PfSI Parameters & State Variables
  std::vector<int> PfID; // pathogen ID
  std::vector<double> tInf; // time of infection (mosquito to human transmission)
  double b; // transmission efficiency: infected mosquito to human
  double c; // transmission efficiency: infected human to mosquito
  std::vector<int> damID; // female gametocyte mother
  std::vector<int> sireID; // male gametocyte father
  bool infected;
  bool chemoprophylaxis;

};

// inline definition of constructor to accept default argument values
inline humanPfSI::humanPfSI(const int &PfID_init, const double &tInf_init,
  const double &b_init, const double &c_init,
  const int &damID_init, const int &sireID_init,
  const bool &infected_init, const bool &chemoprophylaxis_init){

    // set parameters and state variables
    PfID.push_back(PfID_init);
    tInf.push_back(tInf_init);
    b = b_init;
    c = c_init;
    damID.push_back(damID_init);
    sireID.push_back(sireID_init);
    infected = infected_init;
    chemoprophylaxis = chemoprophylaxis_init;

    // reserve memory for history
    events.reserve(50);
    events.push_back("init");
    eventT.reserve(50);
    eventT.push_back(-1);

  }

// Mosquito-stage PfSI Object
class mosquitoPfSI {
// public members
public:

  ///////////////////////////////////
  // Mosquito Stage PfSI Constructor
  ///////////////////////////////////

  mosquitoPfSI(const int &PfID_init, const double &tInf_init = -1,
    const int &damID_init = -1, const int &sireID_init = -1, const bool &infected_init = false);

  ///////////////////////////////////
  // Getters & Setters
  ///////////////////////////////////

  int get_PfID(){
    return(PfID);
  };
  void set_PfID(const int &PfID_new){
    PfID = PfID_new;
  }

  double get_tInf(){
    return(tInf);
  };
  void set_tInf(const double &tInf_new){
    tInf = tInf_new;
  };

  int get_damID(){
    return(damID);
  };
  void set_damID(const int &damID_new){
    damID = damID_new;
  };

  int get_sireID(){
    return(sireID);
  };
  void set_sireID(const int &sireID_new){
    sireID = sireID_new;
  };

  bool get_infected(){
    return(infected);
  };
  void set_infected(const bool &infected_new){
    infected = infected_new;
  };

// private members
private:

  // PfSI Parameters & State Variables
  int PfID; // pathogen ID
  double tInf; // time of infection (human to mosquito transmission)
  int damID; // female gametocyte mother
  int sireID; // male gametocyte father
  bool infected; // infection

};

// inline definition of constructor to accept default argument values
inline mosquitoPfSI::mosquitoPfSI(const int &PfID_init, const double &tInf_init,
  const int &damID_init, const int &sireID_init, const bool &infected_init){

    // set parameters and state variables
    PfID = PfID_init;
    tInf = tInf_init;
    damID = damID_init;
    sireID = sireID_init;
    infected = infected_init;

  }


}

#endif
