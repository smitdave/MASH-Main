///////////////////////////////////////////////////////////////////////////////
//      ____  _________ ____
//     / __ \/ __/ ___//  _/
//    / /_/ / /_ \__ \ / /
//   / ____/ __/___/ // /
//  /_/   /_/  /____/___/
//
//  MASH-MICRO
//  MICRO: PfSI Methods
//  MASH-CPP Team
//  September 7, 2017
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MASHCPP_PFSI_HPP_
#define _MASHCPP_PFSI_HPP_

#include <Rcpp.h>

namespace MASHcpp {

// Human-stage PfSI Object
class humanPfSI {
// public members
public:

  ///////////////////////////////////
  // Human Stage PfSI Constructor
  ///////////////////////////////////

  humanPfSI(const int &PfID_init, const double &tInf_init = -1,
    const double &b_init = 0.55, const double &c_init = 0.15,
    const bool &infected_init = false, const bool &chemoprophylaxis_init = false,
    const int &N = 20
  );

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

  std::vector<std::string> get_vectorInf(){
    return(vectorInf);
  };
  void push_vectorInf(const std::string &vectorInf_new){
    vectorInf.push_back(vectorInf_new);
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
  void track_history(const double &tEvent, const std::string &event, const std::string &vectorInf_new){
    events.push_back(event);
    eventT.push_back(tEvent);
    vectorInf.push_back(vectorInf_new);
  };

  // return history
  Rcpp::List get_history(){
    return(
      Rcpp::List::create(
        Rcpp::Named("events") = events,
        Rcpp::Named("eventT") = eventT,
        Rcpp::Named("vectorInf") = vectorInf
      )
    );
  };

// private members
private:

  // PfSI History
  std::vector<std::string> events;
  std::vector<double>      eventT;
  std::vector<std::string> vectorInf;

  // PfSI Parameters & State Variables
  std::vector<int> PfID; // pathogen ID
  std::vector<double> tInf; // time of infection (mosquito to human transmission)
  double b; // transmission efficiency: infected mosquito to human
  double c; // transmission efficiency: infected human to mosquito
  bool infected;
  bool chemoprophylaxis;

};

// inline definition of constructor to accept default argument values
inline humanPfSI::humanPfSI(const int &PfID_init, const double &tInf_init,
  const double &b_init, const double &c_init,
  const bool &infected_init, const bool &chemoprophylaxis_init, const int &N){

    // set parameters and state variables
    PfID.push_back(PfID_init);
    tInf.push_back(tInf_init);
    b = b_init;
    c = c_init;
    infected = infected_init;
    chemoprophylaxis = chemoprophylaxis_init;

    // reserve memory for history
    events.reserve(N);
    events.push_back("init");
    eventT.reserve(N);
    eventT.push_back(-1);
    vectorInf.reserve(N);
    vectorInf.push_back("init");

  }

// Mosquito-stage PfSI Object
class mosquitoPfSI {
// public members
public:

  ///////////////////////////////////
  // Mosquito Stage PfSI Constructor
  ///////////////////////////////////

  mosquitoPfSI(const int &PfID_init, const double &tInf_init = -1,
    const bool &infected_init = false);

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

  std::string get_humanInf(){
    return(humanInf);
  };
  void set_humanInf(const std::string &humanInf_new){
    humanInf = humanInf_new;
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
  std::string humanInf; // id of infecting human
  bool infected; // infection

};

// inline definition of constructor to accept default argument values
inline mosquitoPfSI::mosquitoPfSI(const int &PfID_init, const double &tInf_init,
  const bool &infected_init){

    // set parameters and state variables
    PfID = PfID_init;
    tInf = tInf_init;
    infected = infected_init;

  }


}

#endif
